module Main exposing (..)

import Html


--wordCount =
--  String.split " " >> List.length


wordCount =
  String.words >> List.length


main =
  wordCount "I have a dream"
  |> toString
  |> Html.text
