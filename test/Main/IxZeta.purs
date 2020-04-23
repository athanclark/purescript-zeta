module Test.Main.IxZeta where

import Prelude
import Data.Maybe (Maybe (..))
import IxZeta as IxS
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)


subscribeSync :: forall a
              . Eq a
              => String
              -> a
              -> (Boolean -> Effect Unit)
              -> Effect Unit
subscribeSync k x onComplete = do
  sig <- IxS.make x
  IxS.subscribe k (\y -> onComplete (y == x)) sig


setSubscribeSync :: forall a
                  . Eq a
                  => String
                  -> a
                  -> (Boolean -> Effect Unit)
                  -> Effect Unit
setSubscribeSync k x onComplete = do
  obtained <- Ref.new Nothing
  sig <- IxS.make x
  let go y = do
        mOb <- Ref.read obtained
        case mOb of
          Nothing -> Ref.write (Just y) obtained
          Just y' -> onComplete (x == y')
  _ <- IxS.subscribe k go sig
  IxS.set x sig


subscribeLightNoSync :: forall a
                      . Eq a
                      => String
                      -> a
                      -> (Boolean -> Effect Unit)
                      -> Effect Unit
subscribeLightNoSync k x onComplete = do
  obtained <- Ref.new Nothing
  sig <- IxS.make x
  let go y = Ref.write (Just y) obtained
  _ <- IxS.subscribeLight k go sig
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
             => String
             -> a
             -> (Boolean -> Effect Unit)
             -> Effect Unit
clearNoSync k x onComplete = do
  obtained <- Ref.new Nothing
  sig <- IxS.make x
  let go y = Ref.write (Just y) obtained
  _ <- IxS.subscribeLight k go sig
  IxS.clear sig
  IxS.set x sig
  void $ setTimeout 100 $ do
    mOb <- Ref.read obtained
    onComplete (mOb == Nothing)
