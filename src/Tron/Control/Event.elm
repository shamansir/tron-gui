module Tron.Control.Event exposing (..)


import Tron.Pages as Pages


type Event
    = Click
    | MouseDown
    | KeyDown Int
    | TextInput String
    | SwitchPage Pages.PageNum
