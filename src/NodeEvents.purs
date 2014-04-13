module Node.Events where

import Prelude ()
import Control.Monad.Eff

-- belongs in a Node.Events module, perhaps?
foreign import data Node :: !  -- effect of a node.js event loop turn
foreign import data EventEmitter :: * -> *

-- TODO: use Show rather than String for event?
foreign import on
  "function on(emitter) {\
  \  return function(event){\
  \    return function(cb)\
  \      return function() {\
  \        return emmitter.on(event, cb);\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff r i. EventEmitter i ->  String -> (i -> Eff (n :: Node | r) {}) ->
         Eff (n :: Node | eff) (EventEmitter i)
