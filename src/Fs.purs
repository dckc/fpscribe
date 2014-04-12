module Fs where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Reactive
import Data.Foreign
import Data.Maybe

foreign import require_fs "require 'fs'" ::
    forall jex e. Eff (ex :: Exception jex | e) FsModule
foreign import data FsModule :: *

-- Node event stream
foreign import data EventStream :: *

-- http://nodejs.org/api/fs.html#fs_fs_createreadstream_path_options
-- TODO: 
foreign import createReadStream'
  "function(fs){\
  \  function(path){\
  \    function (options){\
  \      return fs.createReadStream(path, options);\
  \    }\
  \  }\
  \}" :: forall a. FsModule -> String -> { | a } -> EventStream

foreign import js_null "null" :: Foreign
foreign import unsafeAsForeign
            "function unsafeAsForeign(x) { return x; }" :: forall a. a -> Foreign

maybeNull :: forall a. Maybe a -> Foreign
maybeNull Nothing = js_null
maybeNull (Just x) = unsafeAsForeign x

createReadStream :: FsModule -> String ->
		  Maybe { flags :: String,
                          encoding :: Maybe[String],
		  	  fd :: Maybe[Number],
			  mode :: Number,  -- break down to ugo x rwx?
                          autoClose :: Boolean } ->
	          EventStream
createReadStream fs path Nothing = createReadStream' fs path {}
createReadStream fs path (Just {
                            flags = flags,
                            encoding = encoding,
                            fd = fd,
                            mode = mode,
                            autoClose = autoClose
                          }) = createReadStream' fs path {
                       flags: flags,
                       encoding: maybeNull encoding,
                       fd: maybeNull fd,
                       mode: mode,
                       autoClose: autoClose }

foreign import streamObserver :: forall eff. EventStream -> (RVar [Number])

foreign import onEvent
  "function onEvent(stream) {\
  \  return function(tag) {\
  \    return function(cb) {\
  \      return function() {\
  \        stream.on(tag, cb);\
  \      }\
  \    }\
  \  }\
  \}" :: forall a eff. EventStream
      -> String
      -> ([Number] -> a)
         -> Eff (reactive :: Reactive | eff) (RVar a)
