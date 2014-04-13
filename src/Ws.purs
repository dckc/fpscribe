module Ws where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Reactive

import Node.Events (Node(..), EventEmitter(..))

foreign import ws "var ws = require('ws');" ::
    forall jex e. Eff (ex :: Exception jex | e) WsModule
foreign import data WsModule :: *
foreign import data Socket :: *

foreign import newServer
  "function newServer(ws) {\
  \  raise(\"@@\");\
  \}" :: forall eff. WsModule -> { port :: Number } ->
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

