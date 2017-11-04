module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import String

-- model


type alias Model =
    { total: Int
    , delta: Int
    , error: Maybe String
    }


initModel : Model
initModel =
    { total = 0
    , delta = 0
    , error = Nothing
    }


-- update


type Msg
    = UpdateDelta String
    | AddCalories
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateDelta deltaStr ->
            case String.toInt deltaStr of
                Ok delta ->
                    { model
                        | delta = delta
                        , error = Nothing
                    }

                Err err ->
                    { model
                        | delta = 0
                        , error = Just err
                    }

        AddCalories ->
            { model
                | total = model.total + model.delta
                , delta = 0
                , error = Nothing
            }

        Clear ->
            initModel



-- view


view : Model -> Html Msg
view model =
    div []
        [ h3 []
            [ text ("Total Calories: " ++ (toString model.total)) ]
        , input
            [ type_ "text"
            , placeholder "Number of calories to add ..."
            , name "calorieCount"
            , value
                ( if model.delta == 0 then
                    ""
                else
                    toString model.delta
                )
            , onInput UpdateDelta
            ]
            []
        , div [] [ text (Maybe.withDefault "" model.error) ]
        , div []
            [ button
                [ type_ "button"
                , onClick AddCalories
                ]
                [ text "Add" ]
            , button
                [ type_ "button"
                , onClick Clear
                ]
                [ text "Clear" ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
