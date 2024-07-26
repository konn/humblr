module Main (main, handlers) where

import Humblr.Workers.Frontend

handlers :: IO JSHandlers
handlers = frontendHandlers

foreign export javascript "handlers" handlers :: IO JSHandlers

main :: IO ()
main = pure ()