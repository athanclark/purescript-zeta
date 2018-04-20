module IxSignal.Internal where

import Prelude hiding (map)
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.Traversable (traverse_)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.UUID as UUID
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)



newtype IxSignal eff a = IxSignal
  { subscribers :: Ref (StrMap (String -> a -> Eff eff Unit))
  , individual :: Ref (StrMap a)
  , broadcast :: Ref a
  }

subscribe :: forall eff a
           . (a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
          -> IxSignal (ref :: REF, uuid :: UUID.GENUUID | eff) a
          -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribe f = subscribeWithKey (\_ -> f)

subscribeIx :: forall eff a
             . (a -> Eff (ref :: REF | eff) Unit)
            -> String
            -> IxSignal (ref :: REF | eff) a
            -> Eff (ref :: REF | eff) Unit
subscribeIx f = subscribeIxWithKey (\_ -> f)

subscribeWithKey :: forall eff a
                  . (String -> a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
                 -> IxSignal (ref :: REF, uuid :: UUID.GENUUID | eff) a
                 -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribeWithKey f sig = do
  k <- show <$> UUID.genUUID
  subscribeIxWithKey f k sig

-- | Add a subscribers to the set
subscribeIxWithKey :: forall eff a
                  . (String -> a -> Eff (ref :: REF | eff) Unit)
                  -> String
                  -> IxSignal (ref :: REF | eff) a
                  -> Eff (ref :: REF | eff) Unit
subscribeIxWithKey f k (IxSignal {subscribers,individual,broadcast}) = do
  x <- do
    mI <- StrMap.lookup k <$> readRef individual
    case mI of
      Nothing -> readRef broadcast
      Just i -> do
        modifyRef individual (StrMap.delete k)
        pure i
  modifyRef subscribers (StrMap.insert k f)
  f k x

-- | Publish a message to the set of subscribers
set :: forall eff a. a -> IxSignal (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
set x (IxSignal {subscribers,individual,broadcast}) = do
  writeRef broadcast x
  fs <- readRef subscribers
  traverse_ (\(Tuple k f) -> f k x) (StrMap.toUnfoldable fs :: Array (Tuple String (String -> a -> Eff (ref :: REF | eff) Unit)))

setIx :: forall eff a. a -> String -> IxSignal (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
setIx x k (IxSignal {subscribers,individual,broadcast}) = do
  modifyRef individual (StrMap.insert k x)
  mF <- StrMap.lookup k <$> readRef subscribers
  case mF of
    Nothing -> pure unit
    Just f -> f k x

-- | Gets the last message published to the subscribers
get :: forall eff a. IxSignal (ref :: REF | eff) a -> Eff (ref :: REF | eff) a
get (IxSignal {broadcast}) = readRef broadcast


getIx :: forall eff a. String -> IxSignal (ref :: REF | eff) a -> Eff (ref :: REF | eff) a
getIx k (IxSignal {individual,broadcast}) = do
  mX <- StrMap.lookup k <$> readRef individual
  case mX of
    Nothing -> readRef broadcast
    Just x -> pure x


-- | Removes all subscribers
clearSubscribers :: forall eff a
       . IxSignal (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) Unit
clearSubscribers (IxSignal {subscribers}) =
  writeRef subscribers StrMap.empty

-- | Removes all individual
clearIndividual :: forall eff a
       . IxSignal (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) Unit
clearIndividual (IxSignal {individual}) =
  writeRef individual StrMap.empty


-- | Removes all from both
clear :: forall eff a
       . IxSignal (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) Unit
clear sig = clearSubscribers sig >>= \_ -> clearIndividual sig


deleteSubscriber :: forall eff a
        . String -> IxSignal (ref :: REF | eff) a
       -> Eff (ref :: REF | eff) Unit
deleteSubscriber k (IxSignal {subscribers}) =
  modifyRef subscribers (StrMap.delete k)

deleteIndividual :: forall eff a
        . String -> IxSignal (ref :: REF | eff) a
       -> Eff (ref :: REF | eff) Unit
deleteIndividual k (IxSignal {individual}) =
  modifyRef individual (StrMap.delete k)

delete :: forall eff a
        . String -> IxSignal (ref :: REF | eff) a
       -> Eff (ref :: REF | eff) Unit
delete k sig = deleteSubscriber k sig >>= \_ -> deleteIndividual k sig


-- | Create a signal with a starting value
make :: forall eff a. a -> Eff (ref :: REF | eff) (IxSignal (ref :: REF | eff) a)
make x = do
  subscribers <- newRef StrMap.empty
  individual <- newRef StrMap.empty
  broadcast <- newRef x
  pure (IxSignal {subscribers,individual,broadcast})
