module Signal.Time where

import Signal.Types (READ, WRITE)
import Signal.Internal (Signal, make, set, get, subscribe)

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Time.Duration (Milliseconds (..))
import Data.Int (round)
import Data.Maybe (Maybe (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Timer (TIMER, setInterval, setTimeout, clearTimeout)


every :: forall eff
       . Milliseconds
      -> Eff (ref :: REF, now :: NOW, timer :: TIMER | eff)
           (Signal (write :: WRITE, read :: READ) (ref :: REF, now :: NOW, timer :: TIMER | eff) Instant)
every (Milliseconds t) = do
  out <- make =<< now
  _ <- setInterval (round t) (now >>= \i -> set i out)
  pure out


delay :: forall eff rw a
       . Milliseconds
      -> Signal (read :: READ | rw) (ref :: REF, timer :: TIMER | eff) a
      -> Eff (ref :: REF, timer :: TIMER | eff)
         (Signal (read :: READ, write :: WRITE) (ref :: REF, timer :: TIMER | eff) a)
delay (Milliseconds t) sig = do
  out <- make =<< get sig
  subscribe (\x -> void $ setTimeout (round t) (set x out)) sig
  pure out


since :: forall eff rw a
       . Milliseconds
      -> Signal (read :: READ | rw) (ref :: REF, timer :: TIMER | eff) a
      -> Eff (ref :: REF, timer :: TIMER | eff)
         (Signal (read :: READ, write :: WRITE) (ref :: REF, timer :: TIMER | eff) Boolean)
since (Milliseconds t) sig = do
  threadRef <- newRef Nothing
  out <- make true
  let spawn = setTimeout (round t) $ do
                writeRef threadRef Nothing
                set false out
  writeRef threadRef <<< Just =<< spawn
  subscribe (\_ -> do
                mThread <- readRef threadRef
                case mThread of
                  Nothing -> pure unit
                  Just thread -> clearTimeout thread
                set true out
                thread <- spawn
                writeRef threadRef (Just thread)
            ) sig
  pure out


debounce :: forall eff rw a
          . Milliseconds
         -> Signal (read :: READ | rw) (ref :: REF, timer :: TIMER | eff) a
         -> Eff (ref :: REF, timer :: TIMER | eff)
            (Signal (read :: READ, write :: WRITE) (ref :: REF, timer :: TIMER | eff) a)
debounce (Milliseconds t) sig = do
  threadRef <- newRef Nothing
  out <- make =<< get sig
  subscribe (\x -> do
                mThread <- readRef threadRef
                case mThread of
                  Nothing -> pure unit
                  Just thread -> clearTimeout thread
                thread <- setTimeout (round t) $ do
                  writeRef threadRef Nothing
                  set x out
                writeRef threadRef (Just thread)
            ) sig
  pure out
