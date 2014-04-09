module Main where

import Data.Array

import Debug.Trace

main = trace "Hello, World!"

decode :: [Number] -> [{pedal:: Number, pressed:: Number}]
decode chunk = do ix <- range 0 3
                  return { pedal: chunk !! (ix * 8), pressed: chunk !! (ix * 8 + 4) }

