module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = 
    { username : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    { username = "", password = "", passwordAgain = "" }


-- UPDATE


type Msg
    = Username String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Username username ->
            { model | username = username }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Userame" model.username Username
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if String.length model.password < 8 then
        div [] [ text "Password to short" ]
    else if not (String.any Char.isUpper model.password) then
        div [] [ text "Password must contain uppercase letter" ]
    else if not (String.any Char.isLower model.password) then
        div [] [ text "Password must contain lowercase letter" ]
    else if not (String.any Char.isDigit model.password) then
        div [] [ text "Password must contain number" ]
    else if model.password /= model.passwordAgain then
        div [ style "color" "red" ] [ text "Passwords do not match!" ]
    else
        div [ style "color" "green" ] [ text "OK" ]