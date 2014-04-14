module Fs where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Foreign
import Data.Maybe

import qualified Node.Events as Ev

foreign import fs "var fs = require('fs')" :: FsModule
foreign import data FsModule :: *

-- http://nodejs.org/api/fs.html#fs_fs_createreadstream_path_options
-- TODO: 
foreign import createReadStream
  "function createReadStream(fs){\
  \  return function(path){\
  \    return function (options){\
  \      return function (){\
  \        return fs.createReadStream(path, options);\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff opts d ex. FsModule -> String -> { | opts } ->
         Eff (node :: Ev.Node, exc :: Exception ex | eff) (Ev.EventEmitter d)

type Buffer = [Number]  -- cheating a little, I think
