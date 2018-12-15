module BackgroundJobRunner where

import Prelude (IO)

import qualified Executables.BackgroundJobRunner as App

main :: IO ()
main = App.run
