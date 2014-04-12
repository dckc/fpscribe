module Footpedal where

import qualified Control.Applicative as Ap
-- import qualified Control.Reactive Rx
import Control.Reactive
import Data.Array
import Data.Maybe
import Data.Traversable (sequence)

import Debug.Trace (trace)

import qualified Fs as Fs
import qualified Ws as Ws

main = do
  fs <- Fs.require_fs
  ws <- Ws.require_ws
  let endpoint = Ws.newServer {port: 8080}
  let dev = Fs.createReadStream "/dev/usb/hiddev0" Nothing
  service dev endpoint

service dev endpoint = do
    trace "awaiting connection..."
    subscribe endpoint $ \conn -> do
        trace "got connection!"
        trace "awaiting foot pedal event..."
        subscribe dev $ \chunk -> do
          case decode chunk of
            Just event -> do
              trace event
              writeRVar conn event
            Nothing -> do
              trace "bad chunk of data from footpedal device"


type PedalState = {pedal:: Number, pressed:: Number}

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
                 return { pedal: pedal_num, pressed: state }


