module Main exposing (..)

import Form.Element as Form exposing (SimpleForm)
import Form.ElementFields as Field
import Form.SimpleFields 
import Form.Field as Field
import Browser
import Element exposing (Element)
import Element.Input


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , view = \ mdl -> Element.layout [] (view mdl)
    , subscriptions = \ _ -> Sub.none
    }


type alias Model = 
    { form : SimpleForm Input }


type alias Input = 
    { email : String
    , password : String
    , repeatPassword : String
    }

type Msg 
    = FormMsg (SimpleForm Input)
    | Submit 


form : SimpleForm Input
form = 
    let match p1 p2 =
            if p1 == p2 then
                Ok ()

            else 
                Err "Password has to match"

        email = 
            Field.textField 
                { textType = Form.SimpleFields.Email }
                ""
                |> Field.withLabel "Email"
                |> Field.withPlaceholder "email@mail.com"
                |> Field.withDescription "We'll never share your email with anyone else."
                |> Field.withHints 
                    [ Field.notEmpty "Input email"
                    , Field.isEmail "Input correct email"
                    ]

        password = 
            Field.textField 
                { textType = Form.SimpleFields.Password }
                ""
                |> Field.withLabel "Password"
                |> Field.withPlaceholder "Password"

        repeatPassword = 
            Field.textField 
                { textType = Form.SimpleFields.Password }
                ""
                |> Field.withLabel "Repeat password"
                |> Field.withPlaceholder "Repeat password"
                |> Field.withGlobalHints 
                    [ \ _ val ->
                        match val.password val.repeatPassword
                    ]
    in
    Form.succeed Input Input
        |> Form.append email
        |> Form.append password
        |> Form.append repeatPassword 


init : () -> (Model, Cmd Msg)
init _ = 
    ( { form = form }
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        FormMsg fmsg ->
            ( { model | form = fmsg }
            , Cmd.none
            )

        Submit ->  
            case Form.isValid () model.form of
                Ok _ -> 
                    (model, Cmd.none)

                Err f -> 
                    ( { model | form = f }
                    , Cmd.none
                    )


view : Model -> Element Msg
view model = 
    Element.column 
        [ Element.centerX 
        , Element.centerY
        ] 
        (List.map (Element.map FormMsg)
            (Form.view () 
                model.form
            )
            ++ 
            [ Element.Input.button []
                { onPress = Just Submit
                , label = Element.text "Submit"
                }
            ]
        )

    --div [ style "display" "flex"
    --    , style "justify-content" "center"
    --    , style "padding" "4em"
    --    ] 

    --    [ Html.form 
    --        [ onSubmit Submit
    --        , class "form" 
    --        ]    
    --        [ div [] 
    --            ( Form.view ()
    --                model.form
    --            )
    --            |> Html.map FormMsg 
    --        , button 
    --            [ type_ "submit" ] 
    --            [ text "Submit" ]
    --        ]
    --    ]
