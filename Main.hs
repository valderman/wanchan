module Main where
import Control.Shell (shell_, cmdline)
import Nyanbda.Config (defaultConfig)
import Nyanbda.Backend (runMain)

main :: IO ()
main = shell_ $ runMain defaultConfig cmdline
