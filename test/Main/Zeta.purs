module Test.Main.Zeta where

import Prelude
import Data.Maybe (Maybe (..))
import Zeta as S
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)


subscribeSync :: forall a
              . Eq a
              => a
              -> (Boolean -> Effect Unit)
              -> Effect Unit
subscribeSync x onComplete = do
  sig <- S.make x
  S.subscribe (\y -> onComplete (y == x)) sig


setSubscribeSync :: forall a
                  . Eq a
                  => a
                  -> (Boolean -> Effect Unit)
                  -> Effect Unit
setSubscribeSync x onComplete = do
  obtained <- Ref.new Nothing
  sig <- S.make x
  let go y = do
        mOb <- Ref.read obtained
        case mOb of
          Nothing -> Ref.write (Just y) obtained
          Just y' -> onComplete (x == y')
  S.subscribe go sig
  S.set x sig


subscribeLightNoSync :: forall a
                      . Eq a
                      => a
                      -> (Boolean -> Effect Unit)
                      -> Effect Unit
subscribeLightNoSync x onComplete = do
  obtained <- Ref.new Nothing
  sig <- S.make x
  let go y = Ref.write (Just y) obtained
  S.subscribeLight go sig
  void $ setTimeout 100 $ do
    mOb <- Ref.read obtained
    onComplete (mOb == Nothing)


getIdentity :: forall a
             . Eq a
             => a
             -> (Boolean -> Effect Unit)
             -> Effect Unit
getIdentity x onComplete = do
  sig <- S.make x
  y <- S.get sig
  onComplete (x == y)


getIdempotent :: forall a
             . Eq a
             => a
             -> (Boolean -> Effect Unit)
             -> Effect Unit
getIdempotent x onComplete = do
  sig <- S.make x
  y1 <- S.get sig
  y2 <- S.get sig
  onComplete (y1 == y2 && y2 == x)


clearNoSync :: forall a
             . Eq a
             => a
             -> (Boolean -> Effect Unit)
             -> Effect Unit
clearNoSync x onComplete = do
  obtained <- Ref.new Nothing
  sig <- S.make x
  let go y = Ref.write (Just y) obtained
  S.subscribeLight go sig
  S.clear sig
  S.set x sig
  void $ setTimeout 100 $ do
    mOb <- Ref.read obtained
    onComplete (mOb == Nothing)
