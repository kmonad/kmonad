module KMonad.Model.TH (mkStages) where

import qualified RIO.NonEmpty as N

import Language.Haskell.TH

-- | Generate alias for the different stages of `Cfg`.
-- Prefix it with the same letter as the stage, except
-- if it wouln't be unique. Then use entire stage name as prefix.
mkStages :: Name -> DecsQ
mkStages cfgN = do
  let cfgT = ConT cfgN
  TyConI (DataD [] _ [xBV] _ [RecC conN vbts] _) <- reify cfgN
  ConT stagesN <- case xBV of
    PlainTV xN _ -> reifyType xN
    KindedTV _ _ xT -> pure xT
  TyConI (DataD [] _ [] _ stageCs _) <- reify stagesN
  stageNs <- for stageCs $ \case NormalC n [] -> pure n; _ -> fail "Invalid stage constructor"
  stageNs' <- for stageNs $ \n -> maybe (fail "Constructor without name") (pure . (,n)) . N.nonEmpty $ nameBase n
  let aliases = N.groupAllWith (N.head . fst) stageNs' >>= \case
       (prefix :| _, con) :| [] -> [([prefix], con)]
       nonUniqs -> first toList <$> toList nonUniqs
  let vts = vbts <&> \(nameBase -> name, _, type') -> (mkName name, type')
  aliases' <- for aliases $ \(prefix, con) -> do
    aliasVTs <- for vts $ \case
      (v, AppT xtf _) -> do
        let t = AppT xtf $ PromotedT con
        t' <- appXTF t
        pure $ if t' == TupleT 0 then Nothing else Just (v, t)
      _ -> fail "Expected type-family with only extension argument"
    pure (prefix, con, aliasVTs)

  pure $ aliases' >>= \(prefix, con, aliasVTs) ->
    let aliasTN = mkName $ prefix ++ nameBase cfgN
        aliasCN = mkName $ prefix ++ nameBase conN
        aliasPatArgs = RecordPatSyn $ fst <$> (toList =<< aliasVTs)
        aliasPat = ConP conN [] $ maybe (TupP []) (VarP . fst) <$> aliasVTs 

     in [ TySynD aliasTN [] . AppT cfgT $ PromotedT con
        , PragmaD $ CompleteP [aliasCN] Nothing
        , PatSynSigD aliasCN $ foldr AppT (AppT cfgT $ PromotedT con) $ AppT ArrowT . snd <$> (toList =<< aliasVTs)
        , PatSynD aliasCN aliasPatArgs ImplBidir aliasPat
        ]
 where
  appXTF :: Type -> Q Type
  appXTF pat@(ConT xtf `AppT` PromotedT _) = do
    FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ [_] _ _) eqs) [] <- reify xtf
    eqs' <- for eqs $ \case
      TySynEqn Nothing pat' val -> pure (pat', val)
      TySynEqn (Just [_]) pat'@(ConT _ `AppT` VarT _) val -> pure (pat', val)
      x -> fail $ "Unexpected type-family pattern" <> show x
    let ress = eqs' >>= \case
         (_ `AppT` VarT _, val) -> [val]
         (pat', val)
          | pat == pat' -> [val]
          | otherwise -> []
    case ress^?_head of Just res -> pure res; _ -> fail "Could not determine correct pattern"
  appXTF _ = fail "Failed to apply top level extension type-family"
