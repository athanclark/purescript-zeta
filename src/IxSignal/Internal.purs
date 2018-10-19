module IxSignal.Internal where

import Signal.Types (kind SCOPE, READ, WRITE, class SignalScope, Handler)

import Prelude hiding (map)
import Data.Maybe (Maybe (..))
import Data.TraversableWithIndex (traverseWithIndex)
-- import Data.Object (Object)
-- import Data.Object as Object
import Data.UUID as UUID
import Data.Array as Array
import Foreign.Object (Object)
import Foreign.Object as Object
-- import Control.Monad.Eff (Eff, kind Effect)
-- import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref



newtype IxSignal (rw :: # SCOPE) a = IxSignal
  { subscribers :: Ref (Object (String -> a -> Effect Unit))
  , individual :: Ref (Object a)
  , broadcast :: Ref a
  }

instance signalScopeIxSignal :: SignalScope IxSignal where
  readOnly (IxSignal xs) = IxSignal xs
  writeOnly (IxSignal xs) = IxSignal xs
  allowReading (IxSignal xs) = IxSignal xs
  allowWriting (IxSignal xs) = IxSignal xs

subscribe :: forall rw a
           . Handler a
          -> IxSignal (read :: READ | rw) a
          -> Effect Unit
subscribe f = subscribeWithKey (\_ -> f)

subscribeLight :: forall rw a
                . Handler a
               -> IxSignal (read :: READ | rw) a
               -> Effect Unit
subscribeLight f = subscribeWithKeyLight (\_ -> f)

subscribeDiff :: forall rw a
               . Eq a
              => Handler a
              -> IxSignal (read :: READ | rw) a
              -> Effect Unit
subscribeDiff f = subscribeWithKeyDiff (\_ -> f)

subscribeDiffLight :: forall rw a
                    . Eq a
                   => Handler a
                   -> IxSignal (read :: READ | rw) a
                   -> Effect Unit
subscribeDiffLight f = subscribeWithKeyDiffLight (\_ -> f)

subscribeIx :: forall rw a
             . Handler a
            -> String
            -> IxSignal (read :: READ | rw) a
            -> Effect Unit
subscribeIx f = subscribeIxWithKey (\_ -> f)

subscribeIxLight :: forall rw a
                  . Handler a
                 -> String
                 -> IxSignal (read :: READ | rw) a
                 -> Effect Unit
subscribeIxLight f = subscribeIxWithKeyLight (\_ -> f)

subscribeIxDiff :: forall rw a
                 . Eq a
                => Handler a
                -> String
                -> IxSignal (read :: READ | rw) a
                -> Effect Unit
subscribeIxDiff f = subscribeIxWithKeyDiff (\_ -> f)

subscribeIxDiffLight :: forall rw a
                      . Eq a
                     => Handler a
                     -> String
                     -> IxSignal (read :: READ | rw) a
                     -> Effect Unit
subscribeIxDiffLight f = subscribeIxWithKeyDiffLight (\_ -> f)

subscribeWithKey :: forall rw a
                  . (String -> Handler a)
                 -> IxSignal (read :: READ | rw) a
                 -> Effect Unit
subscribeWithKey f sig = do
  k <- show <$> UUID.genUUID
  subscribeIxWithKey f k sig

subscribeWithKeyLight :: forall rw a
                       . (String -> Handler a)
                      -> IxSignal (read :: READ | rw) a
                      -> Effect Unit
subscribeWithKeyLight f sig = do
  k <- show <$> UUID.genUUID
  subscribeIxWithKeyLight f k sig

subscribeWithKeyDiff :: forall rw a
                      . Eq a
                     => (String -> Handler a)
                     -> IxSignal (read :: READ | rw) a
                     -> Effect Unit
subscribeWithKeyDiff f sig = do
  k <- show <$> UUID.genUUID
  subscribeIxWithKeyDiff f k sig

subscribeWithKeyDiffLight :: forall rw a
                           . Eq a
                          => (String -> Handler a)
                          -> IxSignal (read :: READ | rw) a
                          -> Effect Unit
subscribeWithKeyDiffLight f sig = do
  k <- show <$> UUID.genUUID
  subscribeIxWithKeyDiffLight f k sig


-- | Add a subscriber to the set, removing and using a specific named value if it exists
subscribeIxWithKey :: forall rw a
                    . (String -> Handler a)
                   -> String
                   -> IxSignal (read :: READ | rw) a
                   -> Effect Unit
subscribeIxWithKey f k (IxSignal {individual,broadcast,subscribers}) = do
  void (Ref.modify (Object.insert k f) subscribers)
  x <- do
    mI <- Object.lookup k <$> Ref.read individual
    case mI of
      Nothing -> Ref.read broadcast
      Just i -> do
        void (Ref.modify (Object.delete k) individual)
        pure i
  f k x

-- | Add a subscriber to the set, without applying a value first. Deletes specific indexed named value, if it exists.
subscribeIxWithKeyLight :: forall rw a
                         . (String -> Handler a)
                        -> String
                        -> IxSignal (read :: READ | rw) a
                        -> Effect Unit
subscribeIxWithKeyLight f k (IxSignal {subscribers,individual}) = do
  void (Ref.modify (Object.delete k) individual)
  void (Ref.modify (Object.insert k f) subscribers)


-- | Only respond to changes in signal's value, not submissions in total
subscribeIxWithKeyDiff :: forall rw a
                        . Eq a
                       => (String -> Handler a)
                       -> String
                       -> IxSignal (read :: READ | rw) a
                       -> Effect Unit
subscribeIxWithKeyDiff f k sig = do
  lastValueRef <- Ref.new =<< get sig
  let go k' x = do
        lastValue <- Ref.read lastValueRef
        when (x /= lastValue) $ do
          Ref.write x lastValueRef
          f k' x
  subscribeIxWithKey go k sig


-- | Like `subscribeIxWithKeyDiff`, but without an initial firing
subscribeIxWithKeyDiffLight :: forall rw a
                             . Eq a
                            => (String -> Handler a)
                            -> String
                            -> IxSignal (read :: READ | rw) a
                            -> Effect Unit
subscribeIxWithKeyDiffLight f k sig = do
  lastValueRef <- Ref.new =<< get sig
  let go k' x = do
        lastValue <- Ref.read lastValueRef
        when (x /= lastValue) $ do
          Ref.write x lastValueRef
          f k' x
  subscribeIxWithKeyLight go k sig


-- | Publish a message to the set of subscribers
set :: forall rw a. a -> IxSignal (write :: WRITE | rw) a -> Effect Unit
set x sig = setExcept [] x sig


setExcept :: forall rw a. Array String -> a -> IxSignal (write :: WRITE | rw) a -> Effect Unit
setExcept except x (IxSignal {subscribers,broadcast}) = do
  Ref.write x broadcast
  fs <- Ref.read subscribers
  let go k f
        | k `Array.notElem` except = f k x
        | otherwise = pure unit
  void (traverseWithIndex go fs)


-- | Set a distinguished value for the index, storing if a subscriber is absent
setIx :: forall rw a. a -> String -> IxSignal (write :: WRITE | rw) a -> Effect Unit
setIx x k (IxSignal {subscribers,individual,broadcast}) = do
  mF <- Object.lookup k <$> Ref.read subscribers
  case mF of
    Nothing -> void (Ref.modify (Object.insert k x) individual)
    Just f -> do
      void (Ref.modify (Object.delete k) individual) -- ensure no residual pending value
      f k x


setDiff :: forall a. Eq a => a -> IxSignal (read :: READ, write :: WRITE) a -> Effect Unit
setDiff x sig = do
  y <- get sig
  when (y /= x) (set x sig)


setExceptDiff :: forall a. Eq a => Array String -> a -> IxSignal (read :: READ, write :: WRITE) a -> Effect Unit
setExceptDiff ks x sig = do
  y <- get sig
  when (y /= x) (setExcept ks x sig)


setIxDiff :: forall a. Eq a => a -> String -> IxSignal (read :: READ, write :: WRITE) a -> Effect Unit
setIxDiff x k sig = do
  y <- get sig
  when (y /= x) (setIx x k sig)


-- | Gets the last message published to the subscribers
get :: forall rw a. IxSignal (read :: READ | rw) a -> Effect a
get (IxSignal {broadcast}) = Ref.read broadcast


-- | Attempts to get the last named value, else use the global one.
getIx :: forall rw a. String -> IxSignal (read :: READ | rw) a -> Effect a
getIx k (IxSignal {individual,broadcast}) = do
  mX <- Object.lookup k <$> Ref.read individual
  case mX of
    Nothing -> Ref.read broadcast
    Just x -> pure x


-- | Removes all subscribers
clearSubscribers :: forall rw a. IxSignal (read :: READ | rw) a -> Effect Unit
clearSubscribers (IxSignal {subscribers}) =
  Ref.write Object.empty subscribers

-- | Removes all individual
clearIndividual :: forall rw a. IxSignal (read :: READ | rw) a -> Effect Unit
clearIndividual (IxSignal {individual}) =
  Ref.write Object.empty individual


-- | Removes all from both
clear :: forall rw a. IxSignal (read :: READ | rw) a -> Effect Unit
clear sig = clearSubscribers sig >>= \_ -> clearIndividual sig


deleteSubscriber :: forall rw a. String -> IxSignal (read :: READ | rw) a -> Effect Unit
deleteSubscriber k (IxSignal {subscribers}) =
  void (Ref.modify (Object.delete k) subscribers)

deleteIndividual :: forall rw a. String -> IxSignal (read :: READ | rw) a -> Effect Unit
deleteIndividual k (IxSignal {individual}) =
  void (Ref.modify (Object.delete k) individual)

delete :: forall rw a. String -> IxSignal (read :: READ | rw) a -> Effect Unit
delete k sig = deleteSubscriber k sig >>= \_ -> deleteIndividual k sig


-- | Create a signal with a starting value
make :: forall a. a -> Effect (IxSignal (read :: READ, write :: WRITE) a)
make x = do
  subscribers <- Ref.new Object.empty
  individual  <- Ref.new Object.empty
  broadcast   <- Ref.new x
  pure (IxSignal {subscribers,individual,broadcast})
