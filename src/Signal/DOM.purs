module Signal.DOM where

import Signal.Types (READ, WRITE)
import Signal (Signal, make, set)
import Signal.Compat (map')

import Prelude (bind, pure, (=<<), (<$>), discard, map, void, (<*>), (-), (+), when, (==))
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.Array ((..))
import Data.Array (null) as Array
import Data.Int (toNumber, round)
import Data.DateTime.Instant (Instant)
import Foreign.Object (lookup) as Object
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Now (now)

import Web.HTML (window)
import Web.HTML.Window (document, requestAnimationFrame, innerWidth, innerHeight)
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLDocument (toDocument) as HTMLDocument
import Web.HTML.HTMLElement (toElement) as HTMLElement
import Web.Event.Event (EventType (..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.DOM.Element (scrollLeft, scrollTop)
import Web.DOM.Document (documentElement)
import Unsafe.Coerce (unsafeCoerce)



keyPressed :: Int
           -> Effect (Signal (read :: READ, write :: WRITE) Boolean)
keyPressed k = do
  w <- window
  out <- make false
  l1 <- eventListener (\event -> when ((unsafeCoerce event).keyCode == k) (set true out))
  addEventListener (EventType "keydown") l1 false (Window.toEventTarget w)
  l2 <- eventListener (\event -> when ((unsafeCoerce event).keyCode == k) (set false out))
  addEventListener (EventType "keyup") l2 false (Window.toEventTarget w)
  pure out


mouseButton :: Int
            -> Effect (Signal (read :: READ, write :: WRITE) Boolean)
mouseButton m = do
  w <- window
  out <- make false
  l1 <- eventListener (\event -> when ((unsafeCoerce event).button == m) (set true out))
  addEventListener (EventType "mousedown") l1 false (Window.toEventTarget w)
  l2 <- eventListener (\event -> when ((unsafeCoerce event).button == m) (set false out))
  addEventListener (EventType "mouseup") l2 false (Window.toEventTarget w)
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


touch :: Effect (Signal (read :: READ, write :: WRITE) (Array Touch))
touch = do
  out <- make []
  let report event = do
        let l = (unsafeCoerce event).touches.length
            xs = map (\i -> (unsafeCoerce event).touches.item i) (0 .. (l-1))
        set xs out
  w <- window
  l <- eventListener report
  addEventListener (EventType "touchstart")  l false (Window.toEventTarget w)
  addEventListener (EventType "touchend")    l false (Window.toEventTarget w)
  addEventListener (EventType "touchmove")   l false (Window.toEventTarget w)
  addEventListener (EventType "touchcancel") l false (Window.toEventTarget w)
  pure out


tap :: Effect (Signal (read :: READ, write :: WRITE) Boolean)
tap = do
  touches <- touch
  map' (\t -> Array.null t) touches


mousePos :: Effect (Signal (read :: READ, write :: WRITE) {x :: Int, y :: Int})
mousePos = do
  out <- make {x: 0, y: 0}
  w <- window
  let go event = do
        let e = unsafeCoerce event
        case Tuple <$> Object.lookup "pageX" e <*> Object.lookup "pageY" e of
          Just (Tuple x y) -> set {x,y} out
          Nothing -> case Tuple <$> Object.lookup "clientX" e <*> Object.lookup "clientY" e of
            Just (Tuple x y) -> do
              d <- document w
              mB <- body d
              mD <- documentElement (HTMLDocument.toDocument d)
              case Tuple <$> mB <*> mD of
                Nothing -> throw "No body!"
                Just (Tuple b d') -> do
                  bl <- scrollLeft (HTMLElement.toElement b)
                  dl <- scrollLeft d'
                  bt <- scrollTop (HTMLElement.toElement b)
                  dt <- scrollTop d'
                  set { x: round (toNumber x + bl + dl)
                      , y: round (toNumber y + bt + dt)
                      } out
            Nothing -> throw "Mouse position not recognized!"
  l <- eventListener go
  addEventListener (EventType "mousemove") l false (Window.toEventTarget w)
  pure out


animationFrame :: Effect (Signal (read :: READ, write :: WRITE) Instant)
animationFrame = do
  w <- window
  out <- make =<< now
  let go = do
        t <- now
        set t out
        void (requestAnimationFrame go w)
  _ <- requestAnimationFrame go w
  pure out


windowDimensions :: Effect (Signal (read :: READ, write :: WRITE) {w :: Int, h :: Int})
windowDimensions = do
  win <- window
  out <- make =<< ((\w h -> {w,h}) <$> innerWidth win <*> innerHeight win)
  let go event = do
        w <- innerWidth win
        h <- innerHeight win
        set {w,h} out
  l <- eventListener go
  addEventListener (EventType "resize") l false (Window.toEventTarget win)
  pure out
