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
import Basics



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL
type TemperatureUnit
    = Celcius
    | Farenheid
    | Kelvin

toString : TemperatureUnit -> String
toString temperatureUnit =
    case temperatureUnit of
        Celcius ->
            "Celcius"
        Farenheid ->
            "Farenheid"
        Kelvin ->
            "Kelvin"

toTemperatureUnit : String -> Maybe TemperatureUnit
toTemperatureUnit temperatureUnit =
    if temperatureUnit == "Celcius" then
        Just Celcius
    else if temperatureUnit == "Farenheid" then
        Just Farenheid
    else if temperatureUnit == "Kelvin" then
        Just Kelvin
    else
        Nothing

type Temperature
    = Temperature TemperatureUnit Float

toCelcius : Temperature -> Float
toCelcius temperature =
    case temperature of
        Temperature temperatureUnit temperatureValue ->
            case temperatureUnit of
                Celcius ->
                    temperatureValue
                Farenheid ->
                    (temperatureValue - 32) / 1.8
                Kelvin ->
                    temperatureValue + 273.15

toFarenheid : Temperature -> Float
toFarenheid temperature =
    case temperature of
        Temperature temperatureUnit temperatureValue ->
            case temperatureUnit of
                Celcius ->
                    temperatureValue * 1.8 + 32
                Farenheid ->
                    temperatureValue
                Kelvin ->
                    (temperatureValue + 273.15) * 1.8 + 32

toKelvin : Temperature -> Float
toKelvin temperature =
    case temperature of
        Temperature temperatureUnit temperatureValue ->
            case temperatureUnit of
                Celcius ->
                    temperatureValue + 273.15
                Farenheid ->
                    ((temperatureValue - 32) / 1.8) + 273.15
                Kelvin ->
                    temperatureValue

dropdownOptions : Dropdown.Options Msg
dropdownOptions = 
    let
        defaultOptions =
            Dropdown.defaultOptions TemperatureUnitUpdate
    in
    { 
        defaultOptions
            | items =
                [ { value = toString Celcius, text = toString Celcius, enabled = True }
                , { value = toString Farenheid, text = toString Farenheid, enabled = True }
                , { value = toString Kelvin, text = toString Kelvin, enabled = True }
                ]
            , emptyItem = Nothing
    }
    

type alias Model = 
    { username : String
    , password : String
    , passwordAgain : String
    , temperature : Temperature
    , temperatureUnit: TemperatureUnit
    , validTemperature: Bool
    }


init : Model
init =
    { username = "", password = "", passwordAgain = "" , temperature = Temperature Celcius 25, temperatureUnit = Celcius, validTemperature = True }


-- UPDATE


type Msg
    = Username String
    | Password String
    | PasswordAgain String
    | TemperatureUpdate String
    | TemperatureUnitUpdate (Maybe String)
    

update : Msg -> Model -> Model
update msg model =
    case msg of
        Username username ->
            { model | username = username }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        TemperatureUpdate temperatureInputed ->
            (case String.toFloat temperatureInputed of
                Just temperature -> 
                    { model | temperature = Temperature model.temperatureUnit temperature, validTemperature = True }

                Nothing ->
                    { model | temperature = Temperature model.temperatureUnit 0, validTemperature = False  })

        TemperatureUnitUpdate selectedValue ->
            case selectedValue of
                Nothing ->
                    model
                Just value ->
                    case toTemperatureUnit value of
                        Nothing ->
                            model
                        Just temperatureUnitSelected ->
                            { model | temperatureUnit = temperatureUnitSelected}


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Username" model.username Username
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        , p []
            [ label []
                [ text "Dropdown: "
                , Dropdown.dropdown
                    dropdownOptions
                    []
                    (Just (toString model.temperatureUnit))
                ]
            ]
        , temperatureInput model.temperatureUnit model.temperature TemperatureUpdate
        , div [] [ text ("Celcius = " ++ String.fromFloat (toCelcius model.temperature))]
        , div [] [ text ("Farenheid = " ++ String.fromFloat (toFarenheid model.temperature))]
        , div [] [ text ("Kevin = " ++ String.fromFloat (toKelvin model.temperature))]
        ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []

temperatureUnitInput : TemperatureUnit -> (String -> msg) -> Html msg
temperatureUnitInput temperatureUnit temperatureUnitUpdate =
    input [ value (toString temperatureUnit), onInput temperatureUnitUpdate ] []

temperatureInput : TemperatureUnit -> Temperature -> (String -> msg) -> Html msg
temperatureInput temperatureUnit temperature temperatureUpdate = 
    case temperatureUnit of
        Celcius ->
            input [ value (String.fromFloat (toCelcius temperature)), onInput temperatureUpdate] []
        Farenheid ->
            input [ value (String.fromFloat (toFarenheid temperature)), onInput temperatureUpdate] []
        Kelvin ->
            input [ value (String.fromFloat (toKelvin temperature)), onInput temperatureUpdate] []


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