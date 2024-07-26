module Main (main, frontendHandlers) where

import Humblr.Workers.Frontend

main :: IO ()
main = pure ()

foreign export javascript "handlers" frontendHandlers :: IO JSHandlers
