module Footpedal where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import qualified Control.Applicative as Ap
-- import qualified Control.Reactive Rx
import Control.Reactive
import Data.Array
import Data.Maybe
import Data.Traversable (sequence)

import Debug.Trace

import qualified Node.Events as Ev
import qualified Fs as Fs
import qualified Ws as Ws

main = do
  fs <- Fs.require_fs
  ws <- Ws.require_ws
  pedalDev <- Fs.createReadStream fs "/dev/usb/hiddev0" {}
  endpoint <- Ws.newServer ws {port: 8080}
  service pedalDev endpoint


type RxEff eff = forall eff. Eff (trace :: Trace,
                                  reactive :: Reactive,
                                  node :: Ev.Node
                                  | eff) {}

service :: forall eff conn. Ev.EventEmitter Fs.Buffer ->
           Ev.EventEmitter Ws.Socket ->
           RxEff eff
service dev endpoint = do
  -- @@connections <- fromEmitter endpoint
  -- trace "awaiting connection..."
  -- subscribe connections runSession
  return {}
      -- where
        -- runSession :: forall eff. Ws.Socket -> RxEff eff
        -- runSession pedalSocket =
            --do
              --trace "got connection!"
              --  pedal :: RVar (Maybe PedalState)
              --@@pedalOut <- toSocket pedalSocket
              --trace "awaiting foot pedal event..."
              --pedalIn <- fromEmitter dev
              --subscribe pedalIn $ \bytes -> do
              --           writeRVar pedalOut (decode bytes)
              --pure {}

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




fromEmitter :: forall eff a. Ev.EventEmitter a -> String ->
               Eff (reactive :: Reactive, n :: Ev.Node | eff) (RVar (Maybe a))
fromEmitter emitter event = do
  mv <- newRVar Nothing
  Ev.on emitter event $ \input -> do
                              writeRVar mv (Just input)
  pure mv

toSocket :: forall eff a. (Show a) => Ws.Socket ->
            Eff (reactive :: Reactive, n :: Ev.Node | eff) (RVar (Maybe a))
toSocket socket = do
  mv <- newRVar Nothing
  subscribe mv $ update
  pure mv
    where
      update Nothing = pure {}
      update (Just it) = Ws.send socket (show it)
