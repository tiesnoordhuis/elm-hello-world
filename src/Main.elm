module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type Temperature
    = Celius Float
    | Farenheid Float
    | Kelvin Float

toCelcius : Temperature -> Float
toCelcius temperature =
    case temperature of
        Celius tempInCelcius ->
            tempInCelcius
        Farenheid tempInFarenheid ->
            (tempInFarenheid - 32) / 1.8
        Kelvin tempInKelvin ->
            tempInKelvin + 273.15

toFarenheid : Temperature -> Float
toFarenheid temperature =
    case temperature of
        Celius tempInCelcius ->
            tempInCelcius * 1.8 + 32
        Farenheid tempInFarenheid ->
            tempInFarenheid
        Kelvin tempInKelvin ->
            (tempInKelvin + 273.15) * 1.8 + 32

toKelvin : Temperature -> Float
toKelvin temperature =
    case temperature of
        Celius tempInCelcius ->
            tempInCelcius - 273.15
        Farenheid tempInFarenheid ->
            ((tempInFarenheid - 32) / 1.8) - 273.15
        Kelvin tempInKelvin ->
            tempInKelvin

type alias Model = 
    { username : String
    , password : String
    , passwordAgain : String
    , temperature : Temperature
    , validTemperature: Bool
    }


init : Model
init =
    { username = "", password = "", passwordAgain = "" , temperature = Celius 25, validTemperature = True }


-- UPDATE


type Msg
    = Username String
    | Password String
    | PasswordAgain String
    | TemperatureUpdate String
    


update : Msg -> Model -> Model
update msg model =
    case msg of
        Username username ->
            { model | username = username }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        TemperatureUpdate temperatureInput ->
            (case String.toFloat temperatureInput of
                Just temperature -> 
                    { model | temperature = Celius temperature, validTemperature = True }

                Nothing ->
                    { model | temperature = Celius 0, validTemperature = False  })



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Userame" model.username Username
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        , input [ value (String.fromFloat (toCelcius model.temperature)), onInput TemperatureUpdate ] []
        , div [] [ text ("Celcius = " ++ String.fromFloat (toCelcius model.temperature))]
        , div [] [ text ("Farenheid = " ++ String.fromFloat (toFarenheid model.temperature))]
        , div [] [ text ("Kevin = " ++ String.fromFloat (toKelvin model.temperature))]
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