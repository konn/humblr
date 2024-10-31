module Main (main, handlers) where

import Humblr.Worker.SSR

foreign export javascript "handlers" handlers :: IO SSRService

main :: IO ()
main = pure ()
