module Main (main, indepHandlers) where

import Humblr.Worker.Storage

foreign export javascript "handlers" indepHandlers :: IO JSHandlers

main :: IO ()
main = pure ()
