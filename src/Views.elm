module Views exposing (..)

import Models exposing (..)
import Messages exposing (..)
import Updates exposing (..)
import ThingToDo exposing (..)
import Element exposing (..)
import Element.Background as Background exposing (..)
import Element.Font as Font exposing (..)
import Element.Border as Border
import Element.Input as Input
import Html exposing (..)
import Colors exposing (..)


view : Model -> Html Msg
view model =
    Element.layout [ padding 30, Background.color tertiary ] <| baseView model


baseView model =
    (column
        [ width (px 480)
        , height fill
        , centerX
        , alignTop
        , padding 20
        , spacing 10
        , Background.color secondary
        , Border.color primary
        , Border.width 8
        , Border.rounded 28
        ]
        [ row
            [ centerX
            , alignTop
            , width (px 400)
            , height (px 80)
            , Font.color primary
            , Font.size 42
            ]
            [ el
                [ centerX
                , centerY
                ]
                (Element.text "Listo pa Faena")
            ]
        , column
            [ width (px 320)
            , height fill
            , centerX
            , padding 12
            , spacing 12
            ]
            [ (case List.length model.tasks of
                0 ->
                    addTaskView model

                _ ->
                    (case List.any (\x -> x.editing == True) model.tasks of
                        True ->
                            let
                                currTask =
                                    case (List.head (List.filter (\x -> x.editing == True) model.tasks)) of
                                        Just task ->
                                            task

                                        _ ->
                                            newThingToDo model "" ""
                            in
                                editView model currTask

                        _ ->
                            el [ centerX, Font.color tertiary ] (Element.text (Debug.toString model.tasks))
                    )
              )
            ]
        ]
    )


button : Element Msg
button =
    el [] (Element.text "button")


addTaskView : Model -> Element Msg
addTaskView model =
    let
        newTask =
            newThingToDo model "" ""
    in
        row
            [ height (px 80)
            , width (px 120)
            , centerX
            , Font.color tertiary
            ]
            [ Input.button [ Background.color primary, Font.size 14, padding 10 ]
                { onPress = Just (AddThingToDo newTask), label = Element.text "Add a new task" }
            ]


editView : Model -> ThingToDo -> Element Msg
editView model currTask =
    row
        [ height (px 80)
        , width fill
        , paddingXY 10 0
        , centerX
        , Font.color tertiary
        ]
        [ Input.text []
            { onChange = \text -> AddThingToDo currTask
            , text = ""
            , placeholder = Nothing
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Add Task Name Here")
            }
        ]


confirmDelete : Model -> Element msg
confirmDelete model =
    el [] (Element.text "do you really want to hurt me?")


confirmClear : Element msg
confirmClear =
    el [] (Element.text "clearing state")


confirmRollback : Model -> Element msg
confirmRollback model =
    el [] (Element.text "Do you want to roll back the last change? This cannot be undone.")
