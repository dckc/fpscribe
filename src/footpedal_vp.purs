module Footpedal where

import Control.Monad.Eff
import qualified Control.Applicative as Ap
-- import qualified Control.Reactive Rx
import Control.Reactive
import Data.Array
import Data.Maybe
import Data.Traversable (sequence)

import Debug.Trace

import qualified Fs as Fs
import qualified Ws as Ws

main = do
  fs <- Fs.require_fs
  ws <- Ws.require_ws
  let endpoint = Ws.newServer ws {port: 8080}
  let dev = Fs.createReadStream fs "/dev/usb/hiddev0" Nothing
  service dev endpoint

service :: forall rx. Fs.EventStream ->
           (RVar (RVar Ws.Packet)) -> rx
service dev endpoint = do
    trace "awaiting connection..."
    subscribe endpoint runSession
    return {}
        where
          runSession :: forall rx. (RVar Ws.Packet) -> rx
          runSession socket = do
                         trace "got connection!"
                         trace "awaiting foot pedal event..."
                         subscribe devOutput $ \bytes -> notify socket (decode bytes)
                         return {}
          devOutput = Fs.streamObserver dev
          notify :: forall eff. (RVar Ws.Packet) -> Maybe [PedalState] -> Eff (reactive :: Reactive, trace :: Trace | eff) {}
          notify socket info = do
                         case info of
                           Just event -> do
                             trace $ show event
                             writeRVar socket event
                           Nothing -> do
                             trace "bad chunk of data from footpedal device"
                             return {}

data PedalState = PedalState {pedal:: Number, pressed:: Number}

foreign import unsafeToJSON "JSON.stringify" :: forall a. a -> String

instance showPedal :: Show PedalState where
    show (PedalState r) = unsafeToJSON r


decode :: [Number] -> Maybe [PedalState]
decode bytes = sequence do
  ix <- range 0 2
  let offset = ix * 8
  return $ parts offset
  where
    parts :: Number -> Maybe PedalState
    parts offset = do
                 pedal_num <- bytes !! offset
                 state <- bytes !! (offset + 4)
                 return $ PedalState { pedal: pedal_num, pressed: state }


