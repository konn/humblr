module Main (main, handlers) where

import Humblr.Worker.Images

foreign export javascript "handlers" handlers :: IO ImagesService

main :: IO ()
main = pure ()
