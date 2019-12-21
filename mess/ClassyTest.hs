--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE IncoherentInstances  #-}
module KMonad.Types.Keyboard.Test

where

import KMonad.Prelude

-- lens21 :: Lens' a b -> (b -> c -> d) -> Lens' a d
-- lens21 l f = l . go where go f

data GetA = GetA
  { _aFoo :: Int
  , _aBar :: String
  }

data GetB = GetB
  { __bA :: GetA
  , __bFoo :: Float
  }

data GetC = GetC
  { _cB :: GetB
  , _cBaz :: ()
  }

makeClassy ''GetA
makeLenses ''GetB

class HasGetA e => HasGetB e where
  getB :: Lens' e GetB

  bA :: Lens' e GetA
  bA   = getB . _bA --  go where go f (GetB a b) = (\a' -> GetB a' b) <$> f a

  bFoo :: Lens' e Float
  bFoo = getB . _bFoo -- go where go f (GetB a b) = (\b' -> GetB a b') <$> f b

instance HasGetB GetB where getB = id
instance HasGetA GetB where getA = bA

class HasGetB e => HasGetC e where
  getC :: Lens' e GetC

  cB :: Lens' e GetB
  cB = getC . go where go f (GetC a b) = (\a' -> GetC a' b) <$> f a

  cBaz :: Lens' e ()
  cBaz = getC . go where go f (GetC a b) = (\b' -> GetC a b') <$> f b

instance HasGetC GetC where getC = id
instance HasGetB GetC where getB = cB
instance HasGetA GetC where getA = cB . bA

foo :: HasGetA e => e -> Int
foo e = (e^.aFoo) + 1

testA :: GetA
testA = GetA 3 "hello"

testB :: GetB
testB = GetB (GetA 5 "nope") 3.1415

testC :: GetC
testC = GetC (GetB (GetA 9 "whoopie") 1.2345) ()

yo :: HasGetB a => a -> Int
yo a = a^.aFoo + (floor $ a^.bFoo)


----------- Best code: a bit wordy in the definition, but clean/works, no weirdo extensions

-- data GetA = GetA
--   { _aFoo :: Int
--   , _aBar :: String
--   }

-- data GetB = GetB
--   { _bA :: GetA
--   , _bFoo :: Float
--   }

-- data GetC = GetC
--   { _cB :: GetB
--   , _cBaz :: ()
--   }

-- makeClassy ''GetA

-- class HasGetA e => HasGetB e where
--   getB :: Lens' e GetB

--   bA :: Lens' e GetA
--   bA   = getB . go where go f (GetB a b) = (\a' -> GetB a' b) <$> f a

--   bFoo :: Lens' e Float
--   bFoo = getB . go where go f (GetB a b) = (\b' -> GetB a b') <$> f b

-- instance HasGetB GetB where getB = id
-- instance HasGetA GetB where getA = bA

-- class HasGetB e => HasGetC e where
--   getC :: Lens' e GetC

--   cB :: Lens' e GetB
--   cB = getC . go where go f (GetC a b) = (\a' -> GetC a' b) <$> f a

--   cBaz :: Lens' e ()
--   cBaz = getC . go where go f (GetC a b) = (\b' -> GetC a b') <$> f b

-- instance HasGetC GetC where getC = id
-- instance HasGetB GetC where getB = cB
-- instance HasGetA GetC where getA = cB . bA

-- foo :: HasGetA e => e -> Int
-- foo e = (e^.aFoo) + 1

-- testA :: GetA
-- testA = GetA 3 "hello"

-- testB :: GetB
-- testB = GetB (GetA 5 "nope") 3.1415

-- testC :: GetC
-- testC = GetC (GetB (GetA 9 "whoopie") 1.2345) ()

-- yo :: HasGetB a => a -> Int
-- yo a = a^.aFoo + (floor $ a^.bFoo)




---------------- This code works but requires Undecidable and Incoherent instances

-- data GetA = GetA
--   { _aFoo :: Int
--   , _aBar :: String
--   }

-- data GetB = GetB
--   { _bA :: GetA
--   , _bFoo :: Float
--   }

-- data GetC = GetC
--   { _cB :: GetB
--   , _cBaz :: ()
--   }

-- makeClassy ''GetA
-- makeClassy ''GetB


-- instance HasGetA GetB where getA = bA
-- instance HasGetA GetC where getA = getB . getA
-- instance HasGetB GetC where getB = cB


-- foo :: HasGetA e => e -> Int
-- foo e = (e^.aFoo) + 1

-- testA :: GetA
-- testA = GetA 3 "hello"

-- testB :: GetB
-- testB = GetB (GetA 5 "nope") 3.1415

-- testC :: GetC
-- testC = GetC (GetB (GetA 9 "whoopie") 1.2345) ()

-- instance HasGetB e => HasGetA e where
--   getA = getB . getA

-- yo :: HasGetB a => a -> Int
-- yo a = a^.aFoo + (floor $ a^.bFoo)




------------------- This code sort of works but requires verbose type signatures or calling

-- data A = A
--   { aFoo :: Int
--   , aBar :: String
--   }

-- data B = B
--   { bA :: A
--   , bFoo :: Float
--   }

-- data C = C
--   { cB :: B
--   , cBaz :: ()
--   }


-- class HasA e where
--   getA :: Lens' e A
-- instance HasA A where
--   getA = id

-- class HasA e => HasB e where
--   getB :: Lens' e B
-- instance HasA B where
--   getA = go where go f (B a o) = (\a' -> B a' o) <$> f a
-- instance HasB B where
--   getB = id


-- class HasB e => HasC e where
--   getC :: Lens' e C

-- bL :: Lens' C B
-- bL = go where go f (C b z) = (\b' -> C b' z) <$> f b

-- instance HasA C where
--   getA = bL . getA
-- instance HasB C where
--   getB = bL
-- instance HasC C where
--   getC = id

-- foo :: HasA e => e -> Int
-- foo e = (aFoo $ e^.getA) + 1

-- testA :: A
-- testA = A 3 "hello"

-- testB :: B
-- testB = B (A 5 "nope") 3.1415
