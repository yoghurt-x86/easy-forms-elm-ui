module Form.ElementFields exposing (..)

import Element exposing (Element)
import Element.Events exposing (onLoseFocus)
import Element.Input
import Form.Field as Form exposing (Field, FieldMsg(..), Value, ViewConfig)
import Form.SimpleFields exposing (TextType(..))


type alias TextFieldConfig =
    Form.SimpleFields.TextFieldConfig


type alias TextField form ctx =
    Field form String TextFieldConfig String String ctx ctx (Element (FieldMsg String))


type TextFieldState
    = Value String TextFieldConfig


textField : TextFieldConfig -> String -> TextField form ctx
textField config value =
    { view = textFieldView
    , state =
        { value = value
        , textType = config.textType
        }
    , update = Form.SimpleFields.textFieldUpdate
    , hints = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = identity
    , hinting = []
    , globalHinting = []
    }


textFieldView : ctx -> ViewConfig String TextFieldConfig String -> Element (FieldMsg String)
textFieldView _ field =
    let
        textType =
            case field.state.textType of
                Text ->
                    "text"

                TextArea ->
                    "text"

                Password ->
                    "password"

                Email ->
                    "email"

                Search ->
                    "search"
    in
    Element.column
        []
        [ Element.Input.text
            [ onLoseFocus Blur ]
            { onChange = FieldMsg
            , text = field.state.value
            , placeholder =
                Maybe.map
                    (Element.Input.placeholder [] << Element.text)
                    field.placeholder
            , label =
                Maybe.map
                    (Element.Input.labelAbove [] << Element.text)
                    field.label
                    |> Maybe.withDefault (Element.Input.labelHidden "")
            }
        , Maybe.map Element.text field.description
            |> Maybe.withDefault Element.none
        , Element.column []
            (List.map
                Element.text
                field.hints
            )
        ]
