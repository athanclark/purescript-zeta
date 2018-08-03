module Signal.DOM where

import Signal.Types (READ, WRITE)
import Signal.Internal (Signal, make, set)
import Signal (map')

import Prelude
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.Undefinable (toMaybe)
import Data.Array ((..))
import Data.Array as Array
import Data.Int (toNumber, round)
import Data.DateTime.Instant (Instant)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Now (NOW, now)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document, requestAnimationFrame, innerWidth, innerHeight)
import DOM.HTML.Document (body)
import DOM.HTML.Types (windowToEventTarget, htmlElementToElement, htmlDocumentToDocument)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (EventType (..))
import DOM.Node.Element (scrollLeft, scrollTop)
import DOM.Node.Document (documentElement)
import Unsafe.Coerce (unsafeCoerce)



keyPressed :: forall eff
            . Int
           -> Eff (ref :: REF, dom :: DOM | eff)
              (Signal (read :: READ, write :: WRITE) (ref :: REF, dom :: DOM | eff) Boolean)
keyPressed k = do
  w <- window
  out <- make false
  addEventListener (EventType "keydown") (eventListener $ \event -> when ((unsafeCoerce event).keyCode == k) (set true out)) false (windowToEventTarget w)
  addEventListener (EventType "keyup") (eventListener $ \event -> when ((unsafeCoerce event).keyCode == k) (set false out)) false (windowToEventTarget w)
  pure out


mouseButton :: forall eff
             . Int
            -> Eff (ref :: REF, dom :: DOM | eff)
               (Signal (read :: READ, write :: WRITE) (ref :: REF, dom :: DOM | eff) Boolean)
mouseButton m = do
  w <- window
  out <- make false
  addEventListener (EventType "mousedown") (eventListener $ \event -> when ((unsafeCoerce event).button == m) (set true out)) false (windowToEventTarget w)
  addEventListener (EventType "mouseup") (eventListener $ \event -> when ((unsafeCoerce event).button == m) (set false out)) false (windowToEventTarget w)
  pure out


type Touch =
  { id :: String
  , screenX :: Int
  , screenY :: Int
  , clientX :: Int
  , clientY :: Int
  , pageX :: Int
  , pageY :: Int
  , radiusX :: Int
  , radiusY :: Int
  , rotationAngle :: Number
  , force :: Number
  }


touch :: forall eff
       . Eff (ref :: REF, dom :: DOM | eff)
         (Signal (read :: READ, write :: WRITE) (ref :: REF, dom :: DOM | eff) (Array Touch))
touch = do
  out <- make []
  let report event = do
        let l = (unsafeCoerce event).touches.length
            xs = map (\i -> (unsafeCoerce event).touches.item i) (0 .. (l-1))
        set xs out
  w <- window
  addEventListener (EventType "touchstart") (eventListener report) false (windowToEventTarget w)
  addEventListener (EventType "touchend") (eventListener report) false (windowToEventTarget w)
  addEventListener (EventType "touchmove") (eventListener report) false (windowToEventTarget w)
  addEventListener (EventType "touchcancel") (eventListener report) false (windowToEventTarget w)
  pure out


tap :: forall eff
     . Eff (ref :: REF, dom :: DOM | eff)
       (Signal (read :: READ, write :: WRITE) (ref :: REF, dom :: DOM | eff) Boolean)
tap = do
  touches <- touch
  map' (\t -> Array.null t) touches


mousePos :: forall eff
          . Eff (ref :: REF, dom :: DOM, exception :: EXCEPTION | eff)
              (Signal (read :: READ, write :: WRITE) (ref :: REF, dom :: DOM, exception :: EXCEPTION | eff) {x :: Int, y :: Int})
mousePos = do
  out <- make {x: 0, y: 0}
  w <- window
  let go event = do
        let e = unsafeCoerce event
        case Tuple <$> toMaybe e.pageX <*> toMaybe e.pageY of
          Just (Tuple x y) -> set {x,y} out
          Nothing -> case Tuple <$> toMaybe e.clientX <*> toMaybe e.clientY of
            Just (Tuple x y) -> do
              d <- document w
              mB <- body d
              mD <- documentElement (htmlDocumentToDocument d)
              case Tuple <$> mB <*> mD of
                Nothing -> throw "No body!"
                Just (Tuple b d') -> do
                  bl <- scrollLeft (htmlElementToElement b)
                  dl <- scrollLeft d'
                  bt <- scrollTop (htmlElementToElement b)
                  dt <- scrollTop d'
                  set { x: round $ toNumber x + bl + dl
                      , y: round $ toNumber y + bt + dt
                      } out
            Nothing -> throw "Mouse position not recognized!"
  addEventListener (EventType "mousemove") (eventListener go) false (windowToEventTarget w)
  pure out


animationFrame :: forall eff
                . Eff (ref :: REF, dom :: DOM, now :: NOW | eff)
                  (Signal (read :: READ, write :: WRITE) (ref :: REF, dom :: DOM, now :: NOW | eff) Instant)
animationFrame = do
  w <- window
  out <- make =<< now
  let go = do
        t <- now
        set t out
        void $ requestAnimationFrame go w
  _ <- requestAnimationFrame go w
  pure out


windowDimensions :: forall eff
                  . Eff (ref :: REF, dom :: DOM | eff)
                    (Signal (read :: READ, write :: WRITE) (ref :: REF, dom :: DOM | eff) {w :: Int, h :: Int})
windowDimensions = do
  win <- window
  out <- make =<< ((\w h -> {w,h}) <$> innerWidth win <*> innerHeight win)
  let go event = do
        w <- innerWidth win
        h <- innerHeight win
        set {w,h} out
  addEventListener (EventType "resize") (eventListener go) false (windowToEventTarget win)
  pure out
