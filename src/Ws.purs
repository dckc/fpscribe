module Ws where

import Control.Monad.Eff
import Control.Reactive

import Node.Events (Node(..), EventEmitter(..))

foreign import ws "var ws = require('ws')" ::  WsModule
foreign import data WsModule :: *
foreign import data Socket :: *

foreign import createServer
  "function createServer(ws) {\
  \  return function(options){\
  \    return function(){\
  \      return ws.createServer(options);\
  \    };\
  \  };\
  \}" :: forall eff r. WsModule -> { port :: Number } ->
         Eff (node :: Node | eff) (EventEmitter Socket)

foreign import send
  "function send(socket){\
  \  return function(data){\
  \    return function() {\
  \      socket.send(data);\
  \    };\
  \  };\
  \}" :: forall eff. Socket -> String ->
         Eff (node :: Node | eff) {}

