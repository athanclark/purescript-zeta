module Signal.Time where

import Signal.Types (READ, WRITE)
import Signal (Signal, make, set, get, subscribe)

import Prelude (bind, (=<<), discard, pure, ($), unit, void, (>>=))
import Data.DateTime.Instant (Instant)
import Data.Time.Duration (Milliseconds (..))
import Data.Int (round)
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Ref (new, read, write) as Ref
import Effect.Now (now)
import Effect.Timer (setInterval, setTimeout, clearTimeout)


every :: Milliseconds -> Effect (Signal (write :: WRITE, read :: READ) Instant)
every (Milliseconds t) = do
  out <- make =<< now
  _ <- setInterval (round t) (now >>= \i -> set i out)
  pure out


delay :: forall rw a
       . Milliseconds
      -> Signal (read :: READ | rw) a
      -> Effect (Signal (read :: READ, write :: WRITE) a)
delay (Milliseconds t) sig = do
  out <- make =<< get sig
  subscribe (\x -> void $ setTimeout (round t) (set x out)) sig
  pure out


since :: forall rw a
       . Milliseconds
      -> Signal (read :: READ | rw) a
      -> Effect (Signal (read :: READ, write :: WRITE) Boolean)
since (Milliseconds t) sig = do
  threadRef <- Ref.new Nothing
  out <- make true
  let spawn = setTimeout (round t) do
        Ref.write Nothing threadRef
        set false out
  thread <- spawn
  Ref.write (Just thread) threadRef
  subscribe (\_ -> do
                mThread <- Ref.read threadRef
                case mThread of
                  Nothing -> pure unit
                  Just thread' -> clearTimeout thread'
                set true out
                thread' <- spawn
                Ref.write (Just thread') threadRef
            ) sig
  pure out


debounce :: forall rw a
          . Milliseconds
         -> Signal (read :: READ | rw) a
         -> Effect (Signal (read :: READ, write :: WRITE) a)
debounce (Milliseconds t) sig = do
  threadRef <- Ref.new Nothing
  out <- make =<< get sig
  subscribe (\x -> do
                mThread <- Ref.read threadRef
                case mThread of
                  Nothing -> pure unit
                  Just thread -> clearTimeout thread
                thread <- setTimeout (round t) $ do
                  Ref.write Nothing threadRef
                  set x out
                Ref.write (Just thread) threadRef
            ) sig
  pure out
