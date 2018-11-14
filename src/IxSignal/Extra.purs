-- | Utilities for asynchronously drawing values out of a signal.

module IxSignal.Extra where

import Signal.Types (READ, Handler)
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.UUID (UUID, genUUID)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, makeAff, Canceler (..))



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

-- | With a random key
onWhen :: forall rw a b
        . (b -> Maybe a)
       -> Handler a
       -> IxSignal (read :: READ | rw) b
       -> Effect UUID
onWhen g f sig = do
  k <- genUUID
  onWhenIx g f (show k) sig
  pure k


-- | Applies the handler once when the value exists
onAvailableIx :: forall rw a
               . Handler a
              -> String
              -> IxSignal (read :: READ | rw) (Maybe a)
              -> Effect Unit
onAvailableIx = onWhenIx identity


onAvailable :: forall rw a
             . Handler a
            -> IxSignal (read :: READ | rw) (Maybe a)
            -> Effect UUID
onAvailable = onWhen identity


-- | Draws the value out when the predicate is matched, potentially immediately
getWhenIx :: forall rw a b
           . (b -> Maybe a)
          -> String
          -> IxSignal (read :: READ | rw) b
          -> Aff a
getWhenIx g k sig =
  makeAff \resolve -> do
    onWhenIx g (resolve <<< Right) k sig
    pure $ Canceler $ \_ -> liftEffect (IxSignal.deleteSubscriber k sig)


getWhen :: forall rw a b
         . (b -> Maybe a)
        -> IxSignal (read :: READ | rw) b
        -> Aff (Tuple UUID a)
getWhen g sig = do
  k <- liftEffect genUUID
  Tuple k <$> getWhenIx g (show k) sig


-- | Draws the value out when `Just`
getAvailableIx :: forall rw a
                . String
               -> IxSignal (read :: READ | rw) (Maybe a)
               -> Aff a
getAvailableIx = getWhenIx identity


getAvailable :: forall rw a
              . IxSignal (read :: READ | rw) (Maybe a)
             -> Aff (Tuple UUID a)
getAvailable sig = do
  k <- liftEffect genUUID
  Tuple k <$> getAvailableIx (show k) sig


-- * Proceeding on the next change


-- | Applies the handler only once, on the next change
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


-- | Using a random key
onNext :: forall rw a
        . Handler a
       -> IxSignal (read :: READ | rw) a
       -> Effect UUID
onNext f sig = do
  k <- genUUID
  onNextIx f (show k) sig
  pure k


-- | Draws the value out on the next change
getNextIx :: forall rw a
           . String
          -> IxSignal (read :: READ | rw) a
          -> Aff a
getNextIx k sig =
  makeAff \resolve -> do
    onNextIx (resolve <<< Right) k sig
    pure $ Canceler $ \_ -> liftEffect (IxSignal.deleteSubscriber k sig)


getNext :: forall rw a
         . IxSignal (read :: READ | rw) a
        -> Aff (Tuple UUID a)
getNext sig = do
  k <- liftEffect genUUID
  Tuple k <$> getNextIx (show k) sig
