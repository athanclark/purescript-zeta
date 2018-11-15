-- | Indexed signals are similar to normal signals in that they represent a single value at all times,
-- | and can have a set of handlers that react to the value's change. However, indexed signals also
-- | allow for distinguished values for each handler if necessary (where handlers and these indexed
-- | values are indexed by a string). Updating a value for a signal can also be indexed, so the
-- | change is only reflected to that one handler. Likewise, you can broadcast the update.
-- | IxSignals could be useful for many dynamic handlers that need to listen to a shared value -
-- | for instance, the window size, so UI components can react a 'la responsive design at their own
-- | individual volition, while allowing the handler to be unregistered when the user interface
-- | component unmounts.

module IxSignal where

import Signal.Types (allowReading, kind SCOPE, READ, WRITE, class SignalScope, Handler)
import Signal (make, set, Signal) as Sig

import Prelude
  ( Unit, bind, pure, void, (>>=), (<$>), class Eq, when, (/=), discard, (=<<)
  , otherwise, ($), show, unit)
import Data.Maybe (Maybe (..))
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.UUID (UUID)
import Data.UUID (genUUID) as UUID
import Data.Array (notElem) as Array
import Foreign.Object (Object)
import Foreign.Object (freezeST, thawST, empty, lookup) as Object
import Foreign.Object.ST (delete, poke) as Object
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write, modify) as Ref
import Control.Monad.ST (ST)
import Control.Monad.ST (run) as ST



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

-- | Add a handler to the signal, and fire on the current value
subscribe :: forall rw a
           . Handler a
          -> IxSignal (read :: READ | rw) a
          -> Effect UUID
subscribe f = subscribeWithKey (\_ -> f)

-- | Add a handler without an initial fire
subscribeLight :: forall rw a
                . Handler a
               -> IxSignal (read :: READ | rw) a
               -> Effect UUID
subscribeLight f = subscribeWithKeyLight (\_ -> f)

-- | Add a handler and fire initially, but only fire when the value changes
subscribeDiff :: forall rw a
               . Eq a
              => Handler a
              -> IxSignal (read :: READ | rw) a
              -> Effect UUID
subscribeDiff f = subscribeWithKeyDiff (\_ -> f)

-- | Add a handler, but only fire when the value changes
subscribeDiffLight :: forall rw a
                    . Eq a
                   => Handler a
                   -> IxSignal (read :: READ | rw) a
                   -> Effect UUID
subscribeDiffLight f = subscribeWithKeyDiffLight (\_ -> f)

-- | Add a handler to the signal and fire initially, but with a specific key
subscribeIx :: forall rw a
             . Handler a
            -> String
            -> IxSignal (read :: READ | rw) a
            -> Effect Unit
subscribeIx f = subscribeIxWithKey (\_ -> f)

-- | Add a handler to the signal without firing initially, but with a specific key
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

-- | Subscribe a handler to a random UUID key
subscribeWithKey :: forall rw a
                  . (String -> Handler a)
                 -> IxSignal (read :: READ | rw) a
                 -> Effect UUID
subscribeWithKey f sig = do
  k <- UUID.genUUID
  subscribeIxWithKey f (show k) sig
  pure k

-- | Subscribe without initial application
subscribeWithKeyLight :: forall rw a
                       . (String -> Handler a)
                      -> IxSignal (read :: READ | rw) a
                      -> Effect UUID
subscribeWithKeyLight f sig = do
  k <- UUID.genUUID
  subscribeIxWithKeyLight f (show k) sig
  pure k

subscribeWithKeyDiff :: forall rw a
                      . Eq a
                     => (String -> Handler a)
                     -> IxSignal (read :: READ | rw) a
                     -> Effect UUID
subscribeWithKeyDiff f sig = do
  k <- UUID.genUUID
  subscribeIxWithKeyDiff f (show k) sig
  pure k

subscribeWithKeyDiffLight :: forall rw a
                           . Eq a
                          => (String -> Handler a)
                          -> IxSignal (read :: READ | rw) a
                          -> Effect UUID
subscribeWithKeyDiffLight f sig = do
  k <- UUID.genUUID
  subscribeIxWithKeyDiffLight f (show k) sig
  pure k


-- | Add a subscriber to the set, removing and using a specific named value if it exists
subscribeIxWithKey :: forall rw a
                    . (String -> Handler a)
                   -> String
                   -> IxSignal (read :: READ | rw) a
                   -> Effect Unit
subscribeIxWithKey f k sig@(IxSignal {individual,broadcast,subscribers}) = do
  let go2 o =
        let go' :: forall r. ST r (Object (String -> a -> Effect Unit))
            go' = do
              o' <- Object.thawST o
              o'' <- Object.poke k f o'
              Object.freezeST o''
        in  ST.run go'
  void (Ref.modify go2 subscribers)
  x <- do
    mI <- Object.lookup k <$> Ref.read individual
    case mI of
      Nothing -> Ref.read broadcast
      Just i -> do
        deleteIndividual k sig
        pure i
  f k x

-- | Add a subscriber to the set, without applying a value first.
--   Deletes specific indexed named value, if it exists.
subscribeIxWithKeyLight :: forall rw a
                         . (String -> Handler a)
                        -> String
                        -> IxSignal (read :: READ | rw) a
                        -> Effect Unit
subscribeIxWithKeyLight f k sig@(IxSignal {subscribers,individual}) = do
  deleteIndividual k sig
  let go2 o =
        let go' :: forall r. ST r (Object (String -> a -> Effect Unit))
            go' = do
              o' <- Object.thawST o
              o'' <- Object.poke k f o'
              Object.freezeST o''
        in  ST.run go'
  void (Ref.modify go2 subscribers)


-- | Only respond to changes in signal's value, not submissions in total
subscribeIxWithKeyDiff :: forall rw a
                        . Eq a
                       => (String -> Handler a)
                       -> String
                       -> IxSignal (read :: READ | rw) a
                       -> Effect Unit
subscribeIxWithKeyDiff f k sig = do
  lastValueRef <- Ref.new Nothing
  let go k' x = do
        lastValue <- Ref.read lastValueRef
        when (Just x /= lastValue) $ do
          Ref.write (Just x) lastValueRef
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
  traverseWithIndex_ go fs


-- | Set a distinguished value for the index, storing if a subscriber is absent
setIx :: forall rw a. a -> String -> IxSignal (write :: WRITE | rw) a -> Effect Unit
setIx x k sig@(IxSignal {subscribers,individual,broadcast}) = do
  mF <- Object.lookup k <$> Ref.read subscribers
  case mF of
    Nothing ->
      let go1 o =
            let go' :: forall r. ST r (Object a)
                go' = do
                  o' <- Object.thawST o
                  o'' <- Object.poke k x o'
                  Object.freezeST o''
            in  ST.run go'
      in  void (Ref.modify go1 individual)
    Just f -> do
      deleteIndividual k (allowReading sig) -- ensure no residual pending value
      f k x

-- | Only set the value if it differs from the current one - useful if you don't want
--   each handler individually to attempt diffing
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


-- | Removes a subscriber
deleteSubscriber :: forall rw a. String -> IxSignal (read :: READ | rw) a -> Effect Unit
deleteSubscriber k (IxSignal {subscribers}) =
  let go1 o =
        let go' :: forall r. ST r (Object (String -> a -> Effect Unit))
            go' = do
              o' <- Object.thawST o
              o'' <- Object.delete k o'
              Object.freezeST o''
        in  ST.run go'
  in  void (Ref.modify go1 subscribers)

-- | Removes an individual value
deleteIndividual :: forall rw a. String -> IxSignal (read :: READ | rw) a -> Effect Unit
deleteIndividual k (IxSignal {individual}) =
  let go1 o =
        let go' :: forall r. ST r (Object a)
            go' = do
              o' <- Object.thawST o
              o'' <- Object.delete k o'
              Object.freezeST o''
        in  ST.run go'
  in  void (Ref.modify go1 individual)

-- | Removes both
delete :: forall rw a. String -> IxSignal (read :: READ | rw) a -> Effect Unit
delete k sig = deleteSubscriber k sig >>= \_ -> deleteIndividual k sig


-- | Create a signal with a starting value
make :: forall a. a -> Effect (IxSignal (read :: READ, write :: WRITE) a)
make x = do
  subscribers <- Ref.new Object.empty
  individual  <- Ref.new Object.empty
  broadcast   <- Ref.new x
  pure (IxSignal {subscribers,individual,broadcast})


ixSignalToSignalIx :: forall rw a
                    . IxSignal (read :: READ | rw) a
                   -> String
                   -> Effect (Sig.Signal (read :: READ, write :: WRITE) a)
ixSignalToSignalIx sig k = do
  out <- Sig.make =<< get sig
  subscribeIx (\a -> Sig.set a out) k sig
  pure out


-- | Generates a UUID
ixSignalToSignal :: forall rw a
                  . IxSignal (read :: READ | rw) a
                 -> Effect (Sig.Signal (read :: READ, write :: WRITE) a)
ixSignalToSignal sig = do
  k <- show <$> UUID.genUUID
  ixSignalToSignalIx sig k
