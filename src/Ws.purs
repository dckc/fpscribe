module Ws where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Reactive

import Node.Events (Node(..), EventEmitter(..))

foreign import require_ws "require 'ws'" ::
    forall jex e. Eff (ex :: Exception jex | e) WsModule
foreign import data WsModule :: *
foreign import data Socket :: *

foreign import newServer
  "function newServer(ws) {\
  \  raise \"@@\";\
  \}" :: forall eff. WsModule -> { port :: Number } ->
         Eff (n :: Node | eff) (EventEmitter Socket)

foreign import send
  "function send(socket){\
  \  return function(data){\
  \    return function() {\
  \      socket.send(data);\
  \    };\
  \  };\
  \}" :: forall eff. Socket -> String ->
         Eff (n :: Node | eff) {}

