-- | Utilities for asynchronously drawing values out of a signal.

module Zeta.Extra where

import Zeta.Types (READ, Handler)
import IxZeta (IxSignal)
import IxZeta (deleteSubscriber, subscribeLight, subscribe) as IxZeta

import Prelude (Unit, discard, identity, pure, (<<<), unit, ($))
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, makeAff, Canceler (..))



-- * Proceeding when satisfying a Predicate

-- | Applies the function only once, when the predicate is satisfied (potentially immediately)
onWhen :: forall rw a b
        . (b -> Maybe a)
       -> Handler a
       -> String
       -> IxSignal (read :: READ | rw) b
       -> Effect Unit
onWhen g f k sig =
  -- uses subscribeIx to aggressively attempt application
  IxZeta.subscribe k go sig
  where
    go b = case g b of
      Nothing -> pure unit
      Just x -> do
        IxZeta.deleteSubscriber k sig
        f x


-- | Applies the handler once when the value exists
onAvailable :: forall rw a
             . Handler a
            -> String
            -> IxSignal (read :: READ | rw) (Maybe a)
            -> Effect Unit
onAvailable = onWhen identity


-- | Draws the value out when the predicate is matched, potentially immediately
getWhen :: forall rw a b
         . (b -> Maybe a)
        -> String
        -> IxSignal (read :: READ | rw) b
        -> Aff a
getWhen g k sig =
  makeAff \resolve -> do
    onWhen g (resolve <<< Right) k sig
    pure $ Canceler $ \_ -> liftEffect (IxZeta.deleteSubscriber k sig)


-- | Draws the value out when `Just`
getAvailable :: forall rw a
              . String
             -> IxSignal (read :: READ | rw) (Maybe a)
             -> Aff a
getAvailable = getWhen identity



-- * Proceeding on the next change


-- | Applies the handler only once, on the next change
onNext :: forall rw a
        . Handler a
       -> String
       -> IxSignal (read :: READ | rw) a
       -> Effect Unit
onNext f k sig =
  IxZeta.subscribeLight k go sig
  where
    go x = do
      IxZeta.deleteSubscriber k sig
      f x


-- | Draws the value out on the next change
getNext :: forall rw a
         . String
        -> IxSignal (read :: READ | rw) a
        -> Aff a
getNext k sig =
  makeAff \resolve -> do
    onNext (resolve <<< Right) k sig
    pure $ Canceler $ \_ -> liftEffect (IxZeta.deleteSubscriber k sig)
