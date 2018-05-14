module IxSignal.Internal where

import Prelude hiding (map)
import Data.Maybe (Maybe (..))
import Data.TraversableWithIndex (traverseWithIndex)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.UUID as UUID
import Data.Array as Array
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

subscribeLight :: forall eff a
           . (a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
          -> IxSignal (ref :: REF, uuid :: UUID.GENUUID | eff) a
          -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribeLight f = subscribeWithKeyLight (\_ -> f)

subscribeIx :: forall eff a
             . (a -> Eff (ref :: REF | eff) Unit)
            -> String
            -> IxSignal (ref :: REF | eff) a
            -> Eff (ref :: REF | eff) Unit
subscribeIx f = subscribeIxWithKey (\_ -> f)

subscribeIxLight :: forall eff a
             . (a -> Eff (ref :: REF | eff) Unit)
            -> String
            -> IxSignal (ref :: REF | eff) a
            -> Eff (ref :: REF | eff) Unit
subscribeIxLight f = subscribeIxWithKeyLight (\_ -> f)

subscribeWithKey :: forall eff a
                  . (String -> a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
                 -> IxSignal (ref :: REF, uuid :: UUID.GENUUID | eff) a
                 -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribeWithKey f sig = do
  k <- show <$> UUID.genUUID
  subscribeIxWithKey f k sig

subscribeWithKeyLight :: forall eff a
                  . (String -> a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
                 -> IxSignal (ref :: REF, uuid :: UUID.GENUUID | eff) a
                 -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribeWithKeyLight f sig = do
  k <- show <$> UUID.genUUID
  subscribeIxWithKeyLight f k sig

-- | Add a subscriber to the set, removing and using a specific named value if it exists
subscribeIxWithKey :: forall eff a
                  . (String -> a -> Eff (ref :: REF | eff) Unit)
                  -> String
                  -> IxSignal (ref :: REF | eff) a
                  -> Eff (ref :: REF | eff) Unit
subscribeIxWithKey f k (IxSignal {individual,broadcast,subscribers}) = do
  modifyRef subscribers (StrMap.insert k f)
  x <- do
    mI <- StrMap.lookup k <$> readRef individual
    case mI of
      Nothing -> readRef broadcast
      Just i -> do
        modifyRef individual (StrMap.delete k)
        pure i
  f k x

-- | Add a subscriber to the set, without applying a value first. Deletes specific indexed named value, if it exists.
subscribeIxWithKeyLight :: forall eff a
                        . (String -> a -> Eff (ref :: REF | eff) Unit)
                        -> String
                        -> IxSignal (ref :: REF | eff) a
                        -> Eff (ref :: REF | eff) Unit
subscribeIxWithKeyLight f k (IxSignal {subscribers,individual}) = do
  modifyRef individual (StrMap.delete k)
  modifyRef subscribers (StrMap.insert k f)


-- | Publish a message to the set of subscribers
set :: forall eff a. a -> IxSignal (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
set x sig = setExcept [] x sig


setExcept :: forall eff a. Array String -> a -> IxSignal (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
setExcept except x (IxSignal {subscribers,broadcast}) = do
  writeRef broadcast x
  fs <- readRef subscribers
  let go k f
        | k `Array.notElem` except = f k x
        | otherwise = pure unit
  void (traverseWithIndex go fs)


-- | Set a distinguished value for the index, storing if a subscriber is absent
setIx :: forall eff a. a -> String -> IxSignal (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
setIx x k (IxSignal {subscribers,individual,broadcast}) = do
  mF <- StrMap.lookup k <$> readRef subscribers
  case mF of
    Nothing -> modifyRef individual (StrMap.insert k x)
    Just f -> do
      modifyRef individual (StrMap.delete k) -- ensure no residual pending value
      f k x

-- | Gets the last message published to the subscribers
get :: forall eff a. IxSignal (ref :: REF | eff) a -> Eff (ref :: REF | eff) a
get (IxSignal {broadcast}) = readRef broadcast


-- | Attempts to get the last named value, else use the global one.
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
