module Main exposing (..)

import Html

(~=) string1 string2 =
  String.left 1 string1 == String.left 1 string2


main =
  "Alice" ~= "Abel"
  |> toString
  |> Html.text
