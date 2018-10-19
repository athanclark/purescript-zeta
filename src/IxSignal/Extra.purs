module IxSignal.Extra where

import Signal.Types (READ, Handler)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, makeAff, nonCanceler)



-- * Proceeding when satisfying a Predicate

-- | Applies the function only once, when the predicate is satisfied (potentially immediately)
onWhenIx :: forall rw a b
          . (b -> Maybe a)
         -> Handler a
         -> String
         -> IxSignal (read :: READ | rw) b
         -> Effect Unit
onWhenIx g f k sig =
  -- uses subscribeIx to aggressively attempt application
  IxSignal.subscribeIx go k sig
  where
    go b = case g b of
      Nothing -> pure unit
      Just x -> do
        IxSignal.deleteSubscriber k sig
        f x

onWhen :: forall rw a b
        . (b -> Maybe a)
       -> Handler a
       -> IxSignal (read :: READ | rw) b
       -> Effect Unit
onWhen g f sig = do
  k <- show <$> genUUID
  onWhenIx g f k sig


-- | Applies the handler once
onAvailableIx :: forall rw a
               . Handler a
              -> String
              -> IxSignal (read :: READ | rw) (Maybe a)
              -> Effect Unit
onAvailableIx = onWhenIx identity


onAvailable :: forall rw a
             . Handler a
            -> IxSignal (read :: READ | rw) (Maybe a)
            -> Effect Unit
onAvailable = onWhen identity


getWhenIx :: forall rw a b
           . (b -> Maybe a)
          -> String
          -> IxSignal (read :: READ | rw) b
          -> Aff a
getWhenIx g k sig =
  makeAff \resolve -> do
    onWhenIx g (resolve <<< Right) k sig
    pure nonCanceler


getWhen :: forall rw a b
         . (b -> Maybe a)
        -> IxSignal (read :: READ | rw) b
        -> Aff a
getWhen g sig = do
  k <- show <$> liftEffect genUUID
  getWhenIx g k sig


getAvailableIx :: forall rw a
                . String
               -> IxSignal (read :: READ | rw) (Maybe a)
               -> Aff a
getAvailableIx = getWhenIx identity


getAvailable :: forall rw a
              . IxSignal (read :: READ | rw) (Maybe a)
             -> Aff a
getAvailable sig = do
  k <- show <$> liftEffect genUUID
  getAvailableIx k sig


-- * Proceeding on the next change


-- | Applies the function only once, on the next change
onNextIx :: forall rw a
          . Handler a
         -> String
         -> IxSignal (read :: READ | rw) a
         -> Effect Unit
onNextIx f k sig =
  IxSignal.subscribeIxLight go k sig
  where
    go x = do
      IxSignal.deleteSubscriber k sig
      f x


onNext :: forall rw a
        . Handler a
       -> IxSignal (read :: READ | rw) a
       -> Effect Unit
onNext f sig = do
  k <- show <$> genUUID
  onNextIx f k sig


getNextIx :: forall rw a
           . String
          -> IxSignal (read :: READ | rw) a
          -> Aff a
getNextIx k sig =
  makeAff \resolve -> do
    onNextIx (resolve <<< Right) k sig
    pure nonCanceler


getNext :: forall rw a
         . IxSignal (read :: READ | rw) a
        -> Aff a
getNext sig = do
  k <- show <$> liftEffect genUUID
  getNextIx k sig
