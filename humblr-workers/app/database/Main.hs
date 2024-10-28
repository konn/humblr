module Main (main, handlers) where

import Humblr.Worker.Database

foreign export javascript "handlers" handlers :: IO DatabaseService

main :: IO ()
main = pure ()
