module Main (main, frontendHandlers) where

import Humblr.Workers.Frontend

foreign export javascript "handlers" frontendHandlers :: IO JSHandlers

main :: IO ()
main = pure ()
