module Humblr.Worker.Utils (consoleLog) where

import GHC.Wasm.Object.Builtins

foreign import javascript unsafe "console.log($1)"
  consoleLog :: USVString -> IO ()
