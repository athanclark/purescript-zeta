module IxSignal.Internal where

import Signal.Types (kind SCOPE, READ, WRITE, class SignalScope)

import Prelude hiding (map)
import Data.Maybe (Maybe (..))
import Data.TraversableWithIndex (traverseWithIndex)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.UUID as UUID
import Data.Array as Array
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)



newtype IxSignal (rw :: # SCOPE) (eff :: # Effect) a = IxSignal
  { subscribers :: Ref (StrMap (String -> a -> Eff eff Unit))
  , individual :: Ref (StrMap a)
  , broadcast :: Ref a
  }

instance signalScopeIxSignal :: SignalScope IxSignal where
  readOnly (IxSignal xs) = IxSignal xs
  writeOnly (IxSignal xs) = IxSignal xs
  allowReading (IxSignal xs) = IxSignal xs
  allowWriting (IxSignal xs) = IxSignal xs

subscribe :: forall eff rw a
           . (a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
          -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: UUID.GENUUID | eff) a
          -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribe f = subscribeWithKey (\_ -> f)

subscribeLight :: forall eff rw a
           . (a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
          -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: UUID.GENUUID | eff) a
          -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribeLight f = subscribeWithKeyLight (\_ -> f)

subscribeDiff :: forall eff rw a
           . Eq a
          => (a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
          -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: UUID.GENUUID | eff) a
          -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribeDiff f = subscribeWithKeyDiff (\_ -> f)

subscribeDiffLight :: forall eff rw a
           . Eq a
          => (a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
          -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: UUID.GENUUID | eff) a
          -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribeDiffLight f = subscribeWithKeyDiffLight (\_ -> f)

subscribeIx :: forall eff rw a
             . (a -> Eff (ref :: REF | eff) Unit)
            -> String
            -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
            -> Eff (ref :: REF | eff) Unit
subscribeIx f = subscribeIxWithKey (\_ -> f)

subscribeIxLight :: forall eff rw a
             . (a -> Eff (ref :: REF | eff) Unit)
            -> String
            -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
            -> Eff (ref :: REF | eff) Unit
subscribeIxLight f = subscribeIxWithKeyLight (\_ -> f)

subscribeIxDiff :: forall eff rw a
             . Eq a
            => (a -> Eff (ref :: REF | eff) Unit)
            -> String
            -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
            -> Eff (ref :: REF | eff) Unit
subscribeIxDiff f = subscribeIxWithKeyDiff (\_ -> f)

subscribeIxDiffLight :: forall eff rw a
             . Eq a
            => (a -> Eff (ref :: REF | eff) Unit)
            -> String
            -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
            -> Eff (ref :: REF | eff) Unit
subscribeIxDiffLight f = subscribeIxWithKeyDiffLight (\_ -> f)

subscribeWithKey :: forall eff rw a
                  . (String -> a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
                 -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: UUID.GENUUID | eff) a
                 -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribeWithKey f sig = do
  k <- show <$> UUID.genUUID
  subscribeIxWithKey f k sig

subscribeWithKeyLight :: forall eff rw a
                  . (String -> a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
                 -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: UUID.GENUUID | eff) a
                 -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribeWithKeyLight f sig = do
  k <- show <$> UUID.genUUID
  subscribeIxWithKeyLight f k sig

subscribeWithKeyDiff :: forall eff rw a
                      . Eq a
                     => (String -> a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
                     -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: UUID.GENUUID | eff) a
                     -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribeWithKeyDiff f sig = do
  k <- show <$> UUID.genUUID
  subscribeIxWithKeyDiff f k sig

subscribeWithKeyDiffLight :: forall eff rw a
                      . Eq a
                     => (String -> a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
                     -> IxSignal (read :: READ | rw) (ref :: REF, uuid :: UUID.GENUUID | eff) a
                     -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribeWithKeyDiffLight f sig = do
  k <- show <$> UUID.genUUID
  subscribeIxWithKeyDiffLight f k sig


-- | Add a subscriber to the set, removing and using a specific named value if it exists
subscribeIxWithKey :: forall eff rw a
                  . (String -> a -> Eff (ref :: REF | eff) Unit)
                  -> String
                  -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
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
subscribeIxWithKeyLight :: forall eff rw a
                        . (String -> a -> Eff (ref :: REF | eff) Unit)
                        -> String
                        -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
                        -> Eff (ref :: REF | eff) Unit
subscribeIxWithKeyLight f k (IxSignal {subscribers,individual}) = do
  modifyRef individual (StrMap.delete k)
  modifyRef subscribers (StrMap.insert k f)


-- | Only respond to changes in signal's value, not submissions in total
subscribeIxWithKeyDiff :: forall eff rw a
                        . Eq a
                       => (String -> a -> Eff (ref :: REF | eff) Unit)
                       -> String
                       -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
                       -> Eff (ref :: REF | eff) Unit
subscribeIxWithKeyDiff f k sig = do
  lastValueRef <- newRef =<< get sig
  let go k' x = do
        lastValue <- readRef lastValueRef
        when (x /= lastValue) $ do
          writeRef lastValueRef x
          f k' x
  subscribeIxWithKey go k sig


-- | Like `subscribeIxWithKeyDiff`, but without an initial firing
subscribeIxWithKeyDiffLight :: forall eff rw a
                             . Eq a
                            => (String -> a -> Eff (ref :: REF | eff) Unit)
                            -> String
                            -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
                            -> Eff (ref :: REF | eff) Unit
subscribeIxWithKeyDiffLight f k sig = do
  lastValueRef <- newRef =<< get sig
  let go k' x = do
        lastValue <- readRef lastValueRef
        when (x /= lastValue) $ do
          writeRef lastValueRef x
          f k' x
  subscribeIxWithKeyLight go k sig


-- | Publish a message to the set of subscribers
set :: forall eff rw a
     . a -> IxSignal (write :: WRITE | rw) (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
set x sig = setExcept [] x sig


setExcept :: forall eff rw a. Array String -> a -> IxSignal (write :: WRITE | rw) (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
setExcept except x (IxSignal {subscribers,broadcast}) = do
  writeRef broadcast x
  fs <- readRef subscribers
  let go k f
        | k `Array.notElem` except = f k x
        | otherwise = pure unit
  void (traverseWithIndex go fs)


-- | Set a distinguished value for the index, storing if a subscriber is absent
setIx :: forall eff rw a. a -> String -> IxSignal (write :: WRITE | rw) (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
setIx x k (IxSignal {subscribers,individual,broadcast}) = do
  mF <- StrMap.lookup k <$> readRef subscribers
  case mF of
    Nothing -> modifyRef individual (StrMap.insert k x)
    Just f -> do
      modifyRef individual (StrMap.delete k) -- ensure no residual pending value
      f k x


setDiff :: forall eff rw a. Eq a => a -> IxSignal (read :: READ, write :: WRITE) (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
setDiff x sig = do
  y <- get sig
  when (y /= x) (set x sig)


setExceptDiff :: forall eff rw a. Eq a => Array String -> a -> IxSignal (read :: READ, write :: WRITE) (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
setExceptDiff ks x sig = do
  y <- get sig
  when (y /= x) (setExcept ks x sig)


setIxDiff :: forall eff rw a. Eq a => a -> String -> IxSignal (read :: READ, write :: WRITE) (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
setIxDiff x k sig = do
  y <- get sig
  when (y /= x) (setIx x k sig)


-- | Gets the last message published to the subscribers
get :: forall eff rw a. IxSignal (read :: READ | rw) (ref :: REF | eff) a -> Eff (ref :: REF | eff) a
get (IxSignal {broadcast}) = readRef broadcast


-- | Attempts to get the last named value, else use the global one.
getIx :: forall eff rw a. String -> IxSignal (read :: READ | rw) (ref :: REF | eff) a -> Eff (ref :: REF | eff) a
getIx k (IxSignal {individual,broadcast}) = do
  mX <- StrMap.lookup k <$> readRef individual
  case mX of
    Nothing -> readRef broadcast
    Just x -> pure x


-- | Removes all subscribers
clearSubscribers :: forall eff rw a
       . IxSignal (read :: READ | rw) (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) Unit
clearSubscribers (IxSignal {subscribers}) =
  writeRef subscribers StrMap.empty

-- | Removes all individual
clearIndividual :: forall eff rw a
       . IxSignal (read :: READ | rw) (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) Unit
clearIndividual (IxSignal {individual}) =
  writeRef individual StrMap.empty


-- | Removes all from both
clear :: forall eff rw a
       . IxSignal (read :: READ | rw) (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) Unit
clear sig = clearSubscribers sig >>= \_ -> clearIndividual sig


deleteSubscriber :: forall eff rw a
        . String -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
       -> Eff (ref :: REF | eff) Unit
deleteSubscriber k (IxSignal {subscribers}) =
  modifyRef subscribers (StrMap.delete k)

deleteIndividual :: forall eff rw a
        . String -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
       -> Eff (ref :: REF | eff) Unit
deleteIndividual k (IxSignal {individual}) =
  modifyRef individual (StrMap.delete k)

delete :: forall eff rw a
        . String -> IxSignal (read :: READ | rw) (ref :: REF | eff) a
       -> Eff (ref :: REF | eff) Unit
delete k sig = deleteSubscriber k sig >>= \_ -> deleteIndividual k sig


-- | Create a signal with a starting value
make :: forall eff a. a -> Eff (ref :: REF | eff)
        (IxSignal (read :: READ, write :: WRITE) (ref :: REF | eff) a)
make x = do
  subscribers <- newRef StrMap.empty
  individual <- newRef StrMap.empty
  broadcast <- newRef x
  pure (IxSignal {subscribers,individual,broadcast})
