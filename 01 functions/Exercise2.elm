module Main exposing (..)

import Html
import String


uppercaseLongNames name =
  let
    formatted_name = if String.length name > 10 then
      String.toUpper name
    else
      name
  in
    formatted_name ++ " - name length: " ++ toString (String.length name)


main =
  Html.text (uppercaseLongNames "Thomas Christopher Kane")
