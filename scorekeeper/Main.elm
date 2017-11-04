module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- model


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


initModel : Model
initModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    }



-- update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input name ->
            { model | name = name }

        Edit player ->
            { model
            | name = player.name
            , playerId = Just player.id }

        Save ->
            if (String.isEmpty model.name) then
                model
            else
                save model

        Cancel ->
            { model | name = "" }

        Score player points ->
            score model player points

        _ ->
            model

save : Model -> Model
save model =
    case model.playerId of
        Nothing ->
            add model
        Just id ->
            edit model id

add : Model -> Model
add model =
    let
        player =
            Player (List.length model.players) model.name 0
        newPlayers = player :: model.players
    in
        { model
        | players = newPlayers
        , name = "" }

edit : Model -> Int -> Model
edit model id =
    let
        newPlayers = List.map (updatePlayerName model.name id) model.players
        newPlays = List.map (updatePlaysName model.name id) model.plays
    in
        { model
        | players = newPlayers
        , plays = newPlays
        , playerId = Nothing }

updatePlayerName : String -> Int -> Player -> Player
updatePlayerName name id player =
    if player.id == id then
        { player
        | name = name }
    else
        player

updatePlaysName : String -> Int -> Play -> Play
updatePlaysName name playerId play =
    if play.playerId == playerId then
        { play
        | name = name }
    else
        play

score : Model -> Player -> Int -> Model
score model player points =
    let
        newPlayers = List.map (updatePlayerPoints points player.id) model.players
        play =Play (List.length model.plays) player.id player.name points
    in
        { model
        | players = newPlayers
        , plays = play :: model.plays }

updatePlayerPoints : Int -> Int -> Player -> Player
updatePlayerPoints points playerId player =
    if player.id == playerId then
        { player
        | points = player.points + points }
    else
        player


-- view


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playerSection model
        , playerForm model
        , playsList model
        --, p [] [ text (toString model) ]
        ]

playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerHeader
        , playerList model
        , pointTotal model
        ]

playerHeader : Html Msg
playerHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]

playerList : Model -> Html Msg
playerList model =
        --ul []
        --    (List.map player model.players)
        model.players
            |> List.sortBy .name
            |> List.map player
            |> ul []

player : Player -> Html Msg
player player =
    li []
        [ i
            [ class "edit"
            , onClick (Edit player)
            ]
            []
        , div []
            [ text player.name ]
        , button
            [ type_ "button"
            , onClick (Score player 2)
            ]
            [ text "2pt" ]
        , button
            [ type_ "button"
            , onClick (Score player 3)
            ]
            [ text "3pt" ]
        , div []
            [ text (toString player.points) ]
        ]

pointTotal : Model -> Html Msg
pointTotal model =
    let
        total =
            List.map .points model.players
                |> List.sum
    in
        footer []
            [ div [] [ text "Total:" ]
            , div [] [ text (toString total) ]
            ]

playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "text"
            , placeholder "Add/Edit Player..."
            , onInput Input
            , value model.name
            ]
            []
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick Cancel ] [ text "Cancel" ]
        ]

playsList : Model -> Html Msg
playsList model =
    model.plays
        |> List.map play
        |> ul []


play : Play -> Html Msg
play play =
    li []
        [ div []
            [ text play.name ]
        , div []
            [ text (toString play.points) ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
