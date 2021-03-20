module Tron.Paging exposing (..)


import Array exposing (Array)

import Tron.Control.Nest as Nest exposing (..)


maxPerPage : Int
maxPerPage = 9


atPage : PageNum -> Array a -> Array a
atPage page =
    Array.slice (page * maxPerPage) ((page + 1) * maxPerPage)

