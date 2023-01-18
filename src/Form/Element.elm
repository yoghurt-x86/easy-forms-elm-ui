module Form.Element exposing (..)

import Element exposing (Element)
import Form.Field
    exposing
        ( Field
        , FieldMsg(..)
        , Map
        )
import Form.Internals
    exposing
        ( formView
        , validateField
        )


type alias SimpleForm value =
    FormT value value value ()


type alias Form value validated context =
    FormT value value validated context


type FormT value valid validated context
    = Form (FormRecord value valid validated context)


type alias FormRecord value valid validated context =
    { value : value
    , validated : context -> valid -> Result (FormT value valid validated context) validated
    , view : context -> valid -> List (Element (FormT value valid validated context))
    }


succeed : value -> validated -> FormT value valid validated context
succeed value validated =
    Form
        { value = value
        , validated = \_ _ -> Ok validated
        , view = \_ _ -> []
        }


append :
    Field valid a state msg error context localCtx (Element (FieldMsg msg))
    -> FormT (a -> value) valid (a -> value) context
    -> FormT value valid value context
append field (Form form) =
    Form
        { value = form.value field.state.value
        , validated =
            validateField (Ok field.state.value) form Form field append
        , view =
            \ctx val ->
                formView field form Element.map Element.map Form append ctx val
        }


appendMap :
    Map valid b error
    -> Field valid a state msg error context localCtx (Element (FieldMsg msg))
    -> FormT (a -> value) valid (b -> validated) context
    -> FormT value valid validated context
appendMap validate field (Form form) =
    Form
        { value = form.value field.state.value
        , validated =
            \ctx val ->
                validateField (validate val) form Form field (appendMap validate) ctx val
        , view =
            \ctx val ->
                formView field form Element.map Element.map Form (appendMap validate) ctx val
        }


isValid : context -> FormT value value validated context -> Result (FormT value value validated context) validated
isValid context (Form form) =
    form.validated context form.value


view : context -> FormT value value validated context -> List (Element (FormT value value validated context))
view context (Form form) =
    form.view
        context
        form.value
        |> List.reverse
