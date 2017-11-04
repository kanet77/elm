module Main exposing (..)

import List
import Html

type alias Item =
  { name: String
  , qty: Int
  , freeQty: Int
  }


cart : List Item
cart =
    [ { name = "Lemon", qty = 1, freeQty = 0 }
    , { name = "Apple", qty = 5, freeQty = 0 }
    , { name = "Pear", qty = 10, freeQty = 0 }
    ]


markFree : Int -> Int -> Item -> Item
markFree minQty newFreeQty item =
  if item.freeQty < newFreeQty && item.qty >= minQty then
    { item | freeQty = newFreeQty }
  else
    item


newCart : List Item
newCart =
  List.map ((markFree 5 1) >> (markFree 10 3)) cart



--numFree : Item -> Item
--numFree item =
--  if item.qty >= 10 then
--    { item |
--      freeQty = 3
--    }
--  else if item.qty >= 5 then
--    { item |
--      freeQty = 1
--    }
--  else
--    item


--newCart =
--  List.map numFree cart


main : Html.Html msg
main =
  newCart
  |> toString
  |> Html.text
