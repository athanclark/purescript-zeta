module Test.Main where

import Prelude
import Test.Main.Zeta as STest
import Test.Main.IxZeta as IxSTest
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen as QC

import Data.Enum (succ)
import Data.Either (Either (..))
import Data.Maybe (Maybe (Just))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, makeAff, nonCanceler, runAff_)
import Effect.Exception (throwException, error)
import Effect.Console (log, warn)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)


test :: (forall a. Eq a => a -> (Boolean -> Effect Unit) -> Effect Unit)
     -> Aff Unit
test go = makeAff \resolve -> do
  testCases <- QC.randomSample' 100 (arbitrary :: QC.Gen Int)
  successes <- Ref.new 0
  let report :: Boolean -> Effect Unit
      report success
        | success = do
          let inc :: Int -> Int
              inc x = unsafePartial $ case succ x of
                Just y -> y
          new <- Ref.modify inc successes
          if new == 100
            then do
              log "   success!"
              resolve (Right unit)
            else pure unit
        | otherwise = resolve $ Left $ error "failure!"
  traverse_ (\testCase -> go testCase report) testCases
  pure nonCanceler

main :: Effect Unit
main =
  let resolve eX = case eX of
        Left e -> do
          warn (show e)
          throwException e
        Right _ -> pure unit
  in  runAff_ resolve do
        let logSub s = liftEffect $ log $ " - " <> s

        liftEffect $ log "Signal:"
        logSub "Signal.subscribe after Signal.make"
        test STest.subscribeSync
        logSub "Signal.set after Signal.subscribe"
        test STest.setSubscribeSync
        logSub "Signal.subscribeLight doesn't sync after Signal.make"
        test STest.subscribeLightNoSync
        logSub "Signal.get identity"
        test STest.getIdentity
        logSub "Signal.get idempotent"
        test STest.getIdempotent
        logSub "Signal.clear clears"
        test STest.clearNoSync

        liftEffect $ log "IxSignal:"
        logSub "IxSignal.subscribe after IxSignal.make"
        test (IxSTest.subscribeSync "foo")
        logSub "IxSignal.set after IxSignal.subscribe"
        test (IxSTest.setSubscribeSync "foo")
        logSub "IxSignal.subscribeLight doesn't sync after IxSignal.make"
        test (IxSTest.subscribeLightNoSync "foo")
        logSub "IxSignal.get identity"
        test IxSTest.getIdentity
        logSub "IxSignal.get idempotent"
        test IxSTest.getIdempotent
        logSub "IxSignal.clear clears"
        test (IxSTest.clearNoSync "foo")
