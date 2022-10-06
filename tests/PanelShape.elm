module PanelShape exposing (..)


import Json.Encode as E

import Tron exposing (Tron)
import Tron.Core as Core
import Tron.Tree as T exposing (..)
import Tron.Tree.Build.Unit as B


import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    todo "panel shape"
