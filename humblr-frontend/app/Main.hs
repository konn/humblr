module Main (main) where

import Humblr.Frontend (defaultMain)

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = defaultMain
