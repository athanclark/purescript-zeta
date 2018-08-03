module IxSignal.Extra where

import Signal.Types (READ, WRITE)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.UUID (GENUUID, genUUID)
import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)



-- * Proceeding when satisfying a Predicate

-- | Applies the function only once, when the predicate is satisfied (potentially immediately)
onWhenIx :: forall eff rw a b
          . (b -> Maybe a)
         -> (a -> Eff (ref :: REF | eff) Unit)
         -> String
         -> IxSignal (read :: READ | rw) (ref :: REF | eff) b
         -> Eff (ref :: REF | eff) Unit
onWhenIx g f k sig =
  -- uses subscribeIx to aggressively attempt application
  IxSignal.subscribeIx go k sig
  where
    go b = case g b of
      Nothing -> pure unit
      Just x -> do
        IxSignal.deleteSubscriber k sig
        f x

onWhen :: forall eff rw a b
        . (b -> Maybe a)
       -> (a -> Eff (ref :: REF, uuid :: GENUUID | eff) Unit)
       -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: GENUUID | eff) b
       -> Eff (ref :: REF, uuid :: GENUUID | eff) Unit
onWhen g f sig = do
  k <- show <$> genUUID
  onWhenIx g f k sig


-- | Applies the handler once
onAvailableIx :: forall eff rw a
               . (a -> Eff (ref :: REF | eff) Unit)
              -> String
              -> IxSignal (read :: READ | rw) (ref :: REF | eff) (Maybe a)
              -> Eff (ref :: REF | eff) Unit
onAvailableIx = onWhenIx id


onAvailable :: forall eff rw a
             . (a -> Eff (ref :: REF, uuid :: GENUUID | eff) Unit)
            -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: GENUUID | eff) (Maybe a)
            -> Eff (ref :: REF, uuid :: GENUUID | eff) Unit
onAvailable = onWhen id


getWhenIx :: forall eff rw a b
           . (b -> Maybe a)
          -> String
          -> IxSignal (read :: READ | rw) (ref :: REF | eff) b
          -> Aff (ref :: REF | eff) a
getWhenIx g k sig =
  makeAff \resolve -> do
    onWhenIx g (resolve <<< Right) k sig
    pure nonCanceler


getWhen :: forall eff rw a b
         . (b -> Maybe a)
        -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: GENUUID | eff) b
        -> Aff (ref :: REF, uuid :: GENUUID | eff) a
getWhen g sig = do
  k <- show <$> liftEff genUUID
  getWhenIx g k sig


getAvailableIx :: forall eff rw a
                . String
               -> IxSignal (read :: READ | rw) (ref :: REF | eff) (Maybe a)
               -> Aff (ref :: REF | eff) a
getAvailableIx = getWhenIx id


getAvailable :: forall eff rw a
              . IxSignal (read :: READ | rw) (ref :: REF, uuid :: GENUUID | eff) (Maybe a)
             -> Aff (ref :: REF, uuid :: GENUUID | eff) a
getAvailable sig = do
  k <- show <$> liftEff genUUID
  getAvailableIx k sig


-- * Proceeding on the next change


-- | Applies the function only once, on the next change
onNextIx :: forall eff rw a
          . (a -> Eff (ref :: REF | eff) Unit)
         -> String
         -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
         -> Eff (ref :: REF | eff) Unit
onNextIx f k sig =
  IxSignal.subscribeIxLight go k sig
  where
    go x = do
      IxSignal.deleteSubscriber k sig
      f x


onNext :: forall eff rw a
        . (a -> Eff (ref :: REF, uuid :: GENUUID | eff) Unit)
       -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: GENUUID | eff) a
       -> Eff (ref :: REF, uuid :: GENUUID | eff) Unit
onNext f sig = do
  k <- show <$> genUUID
  onNextIx f k sig


getNextIx :: forall eff rw a
           . String
          -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
          -> Aff (ref :: REF | eff) a
getNextIx k sig =
  makeAff \resolve -> do
    onNextIx (resolve <<< Right) k sig
    pure nonCanceler


getNext :: forall eff rw a
         . IxSignal (read :: READ | rw) (ref :: REF, uuid :: GENUUID | eff) a
        -> Aff (ref :: REF, uuid :: GENUUID | eff) a
getNext sig = do
  k <- show <$> liftEff genUUID
  getNextIx k sig
