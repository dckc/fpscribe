module Fs where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Foreign
import Data.Maybe

import qualified Node.Events as Ev

foreign import require_fs "require 'fs'" ::
    forall jex e. Eff (ex :: Exception jex | e) FsModule
foreign import data FsModule :: *

-- http://nodejs.org/api/fs.html#fs_fs_createreadstream_path_options
-- TODO: 
foreign import createReadStream
  "function(fs){\
  \  return function(path){\
  \    return function (options){\
  \      return function (){\
  \        return fs.createReadStream(path, options);\
  \    }\
  \  }\
  \}" :: forall eff opts d. FsModule -> String -> { | opts } ->
         Eff (n :: Ev.Node | eff) (Ev.EventEmitter d)

type Buffer = [Number]  -- cheating a little, I think
