module Main where
import Control.Shell (shell_, cmdline)
import Wanchan.Config (defaultConfig)
import Wanchan.Backend (runMain)

main :: IO ()
main = shell_ $ runMain defaultConfig cmdline
