{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric #-}
module Nyanbda.Web.API
  ( Series (..)
  ) where
import Haste.App
import Haste.JSON
import GHC.Generics
import Nyanbda.Database

instance Node Server

instance Show Series where
  show s = concat
    [ if not (null (seriesGroup s))
        then "[" ++ seriesGroup s ++ "] "
        else ""
    , "<b>", seriesName s, "</b>"
    , if seriesSeason s == 1
        then ""
        else " S" ++ show (seriesSeason s)
    , if seriesResolution s /= Unknown
        then " (" ++ showRes (seriesResolution s) ++ ")"
        else ""
    ]

showRes :: Resolution -> String
showRes r = case toJSON r of Str r' -> fromJSStr r'

instance Serialize Resolution where
  toJSON HD1080  = "1080p"
  toJSON HD720   = "720p"
  toJSON SD480   = "480p"
  toJSON Unknown = ""
  parseJSON (Str "1080p") = pure HD1080
  parseJSON (Str "720p")  = pure HD720
  parseJSON (Str "480p")  = pure SD480
  parseJSON (Str "")      = pure Unknown

instance Serialize Series where
  toJSON (Series t s g r) = Dict
    [ ("title", toJSON t)
    , ("season", toJSON s)
    , ("group", toJSON g)
    , ("resolution", toJSON r)
    ]

  parseJSON o = Series <$> o .: "title"
                       <*> o .: "season"
                       <*> o .: "group"
                       <*> o .: "resolution"
