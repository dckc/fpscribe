module Ws where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Reactive

foreign import require_ws "require 'ws'" ::
    forall jex e. Eff (ex :: Exception jex | e) WsModule
foreign import data WsModule :: *


foreign import newServer
  "function newServer(ws) {\
  \    return function(options) {\
  \      return function(outgoing) {\
  \        return function(){\
  \            var endpoint = new ws.Server(options);\
  \            var buff = [];\
  \            var incoming = _ps.Control_Reactive.newRVar(\"\")();\
  \            var ready = false;\
  \            endpoint.on(\"connection\", function(conn) {\
  \                ready=true;\
  \                buff.forEach(function(msg){conn.send(msg)});\
  \                buff=[];\
  \            };\
  \            outgoing.subscribe(function(msg){\
  \                ready ? conn.send(msg) : buff.push(msg);\
  \            });\
  \            conn.onmessage = function(e){\
  \                incoming.update(e.data);\
  \            };\
  \            return incoming;\
  \        };\
  \      };\
  \}" :: forall eff. WsModule -> { port :: Number } -> RVar [Number] -> Eff (reactive :: Reactive | eff) (RVar String)


foreign import 
  "function newConnection(conn) {\
  \    return function(options) {\
  \      return function(outgoing) {\
  \        return function(){\
  \            var endpoint = new ws.Server(options);\
  \            var buff = [];\
  \            var incoming = _ps.Control_Reactive.newRVar(\"\")();\
  \            var ready = false;\
  \            endpoint.on(\"connection\", function(conn) {\
  \                ready=true;\
  \                buff.forEach(function(msg){conn.send(msg)});\
  \                buff=[];\
  \            };\
  \            outgoing.subscribe(function(msg){\
  \                ready ? conn.send(msg) : buff.push(msg);\
  \            });\
  \            conn.onmessage = function(e){\
  \                incoming.update(e.data);\
  \            };\
  \            return incoming;\
  \        };\
  \      };\
  \}" :: forall eff. WsModule -> { port :: Number } -> RVar [Number] -> Eff (reactive :: Reactive | eff) (RVar String)
