{-# LANGUAGE OverloadedStrings #-}
module Wanchan.Web.Dialog
  ( Dialog
  , createModalDialog, createChoiceDialog
  , showDialog, hideDialog
  ) where
import Haste.App
import Haste.DOM
import Haste.Events
import Control.Monad (forM, forM_, void)

newtype Dialog = Dialog Elem

createModalDialog :: String -> [Elem] -> Client Dialog
createModalDialog title content = do
  ttl <- newElem "h2" `with`
    [ "innerText" =: title
    , style "margin-top" =: "0"
    , style "text-overflow" =: "ellipsis"
    , style "overflow" =: "hidden"
    , style "white-space" =: "nowrap"
    , style "font-size" =: "14pt"
    , style "width" =: "100%"
    ]
  dlg <- newElem "div" `with` ["className" =: "dialog", children (ttl:content)]
  cover <- newElem "div" `with` ["className" =: "cover", children [dlg]]
  let dialog = Dialog cover
  void $ cover `onEvent` Click $ const (hideDialog dialog)
  return dialog

createChoiceDialog :: String -> [(String, Client ())] -> Client Dialog
createChoiceDialog title alts = do
  es <- forM alts $ \(s, m) -> do
    caption <- newTextElem s
    link <- newElem "a" `with`
      [ style "padding" =: "0.5em"
      , style "text-decoration" =: "none"
      , style "font-weight" =: "bold"
      , style "font-family" =: "sans-serif"
      , style "display" =: "block"
      , "href" =: "javascript:void(0)"
      , children [caption]
      ]
    alternative <- newElem "li" `with`
      [ style "padding" =: "0"
      , style "font-size" =: "large"
      , children [link]
      ]
    return (alternative, (link, m))
  menu <- newElem "ul" `with`
    [ style "list-style-type" =: "none"
    , style "margin-left" =: "1em"
    , style "padding" =: "0"
    , children (map fst es)
    ]
  dlg <- createModalDialog title [menu]
  forM_ (map snd es) $ \(link, m) -> do
    link `onEvent` Click $ \_ -> hideDialog dlg >> m
  return dlg

showDialog :: Dialog -> Client ()
showDialog (Dialog e) = appendChild documentBody e

hideDialog :: Dialog -> Client ()
hideDialog (Dialog e) = deleteChild documentBody e
