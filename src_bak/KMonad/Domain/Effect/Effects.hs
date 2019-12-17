module KMonad.Domain.Effect.Effects
  ( CanButton

  , module Control.Monad.Logger
  , module KMonad.Domain.Effect.Effects.MonadButton
  , module KMonad.Domain.Effect.Effects.MonadEmit
  , module KMonad.Domain.Effect.Effects.MonadFork
  , module KMonad.Domain.Effect.Effects.MonadFuture
  , module KMonad.Domain.Effect.Effects.MonadHandler
  , module KMonad.Domain.Effect.Effects.MonadHold
  , module KMonad.Domain.Effect.Effects.MonadInject
  , module KMonad.Domain.Effect.Effects.MonadLock
  , module KMonad.Domain.Effect.Effects.MonadMaskInput
  , module KMonad.Domain.Effect.Effects.MonadNext
  , module KMonad.Domain.Effect.Effects.MonadNow
  , module KMonad.Domain.Effect.Effects.MonadRace
  , module KMonad.Domain.Effect.Effects.MonadStackManip
  , module KMonad.Domain.Effect.Effects.MonadSymbol
  , module KMonad.Domain.Effect.Effects.MonadVar
  , module KMonad.Domain.Effect.Effects.MonadWait
  )
where

import Control.Monad.Logger

import KMonad.Domain.Effect.Effects.MonadButton
import KMonad.Domain.Effect.Effects.MonadEmit
import KMonad.Domain.Effect.Effects.MonadFork
import KMonad.Domain.Effect.Effects.MonadFuture
import KMonad.Domain.Effect.Effects.MonadHandler
import KMonad.Domain.Effect.Effects.MonadHold
import KMonad.Domain.Effect.Effects.MonadInject
import KMonad.Domain.Effect.Effects.MonadLock
import KMonad.Domain.Effect.Effects.MonadMaskInput
import KMonad.Domain.Effect.Effects.MonadNext
import KMonad.Domain.Effect.Effects.MonadNow
import KMonad.Domain.Effect.Effects.MonadRace
import KMonad.Domain.Effect.Effects.MonadStackManip
import KMonad.Domain.Effect.Effects.MonadSymbol
import KMonad.Domain.Effect.Effects.MonadVar
import KMonad.Domain.Effect.Effects.MonadWait

type CanButton m =
  ( MonadButton     m
  , MonadEmit       m
  , MonadFork       m
  , MonadFuture     m
  , MonadHold       m
  , MonadInject     m
  , MonadLock       m
  , MonadLogger     m
  , MonadMaskInput  m
  , MonadNext       m
  , MonadNow        m
  , MonadRace       m
  , MonadStackManip m
  , MonadSymbol     m
  , MonadVar        m
  , MonadWait       m
  )
