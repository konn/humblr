module Main (main, handlers) where

import Humblr.Worker.Storage

foreign export javascript "handlers" handlers :: IO StorageService

main :: IO ()
main = pure ()
