module Tron.Paging exposing (..)


import Array exposing (Array)

import Tron.Control.Nest as Nest exposing (..)

import Size exposing (..)


type Page = Page Int


type Pages = Pages Int


type Paging
    = SinglePage
    | At Page Pages (Size Pixels) -- single page size


maxPerPage : Int
maxPerPage = 9


atPage : PageNum -> Array a -> Array a
atPage page =
    Array.slice (page * maxPerPage) ((page + 1) * maxPerPage)

