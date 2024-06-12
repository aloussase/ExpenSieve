module Main where

import           ExpenSieve.Api
import           ExpenSieve.Impls.Program (mkProgram)
import           Network.Wai.Handler.Warp

main :: IO ()
main = do
  program <- mkProgram
  app <- mkApp program

  -- TODO: Get port from enviroment.
  run 8080 app
