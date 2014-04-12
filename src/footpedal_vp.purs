module Footpedal where

import Data.Array
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Control.Applicative

import Debug.Trace

main = trace "Hello, World!"

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


