module Test.Main.IxSignal where

import Prelude
import Data.Maybe (Maybe (..))
import IxSignal as IxS
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)


subscribeSync :: forall a
              . Eq a
              => a
              -> (Boolean -> Effect Unit)
              -> Effect Unit
subscribeSync x onComplete = do
  sig <- IxS.make x
  void $ IxS.subscribe (\y -> onComplete (y == x)) sig


setSubscribeSync :: forall a
                  . Eq a
                  => a
                  -> (Boolean -> Effect Unit)
                  -> Effect Unit
setSubscribeSync x onComplete = do
  obtained <- Ref.new Nothing
  sig <- IxS.make x
  let go y = do
        mOb <- Ref.read obtained
        case mOb of
          Nothing -> Ref.write (Just y) obtained
          Just y' -> onComplete (x == y')
  _ <- IxS.subscribe go sig
  IxS.set x sig


subscribeLightNoSync :: forall a
                      . Eq a
                      => a
                      -> (Boolean -> Effect Unit)
                      -> Effect Unit
subscribeLightNoSync x onComplete = do
  obtained <- Ref.new Nothing
  sig <- IxS.make x
  let go y = Ref.write (Just y) obtained
  _ <- IxS.subscribeLight go sig
  void $ setTimeout 100 $ do
    mOb <- Ref.read obtained
    onComplete (mOb == Nothing)


getIdentity :: forall a
             . Eq a
             => a
             -> (Boolean -> Effect Unit)
             -> Effect Unit
getIdentity x onComplete = do
  sig <- IxS.make x
  y <- IxS.get sig
  onComplete (x == y)


getIdempotent :: forall a
             . Eq a
             => a
             -> (Boolean -> Effect Unit)
             -> Effect Unit
getIdempotent x onComplete = do
  sig <- IxS.make x
  y1 <- IxS.get sig
  y2 <- IxS.get sig
  onComplete (y1 == y2 && y2 == x)


clearNoSync :: forall a
             . Eq a
             => a
             -> (Boolean -> Effect Unit)
             -> Effect Unit
clearNoSync x onComplete = do
  obtained <- Ref.new Nothing
  sig <- IxS.make x
  let go y = Ref.write (Just y) obtained
  _ <- IxS.subscribeLight go sig
  IxS.clear sig
  IxS.set x sig
  void $ setTimeout 100 $ do
    mOb <- Ref.read obtained
    onComplete (mOb == Nothing)
