module Main (main, handlers) where

import Humblr.Worker.Router

foreign export javascript "handlers" handlers :: IO JSHandlers

main :: IO ()
main = pure ()
