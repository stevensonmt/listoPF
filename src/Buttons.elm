module Buttons exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Events
import Colors exposing (..)
import Fonts exposing (..)


roundButton : Int -> Float -> List (Element msg) -> Element msg
roundButton radius rotation elements =
    row
        [ centerY
        , Element.width (px radius)
        , Element.height (px radius)
        , Border.rounded radius
        , Background.color (Colors.primary 1)
        , centerX
        ]
        [ row
            [ centerX
            , centerY
            , Element.rotate rotation
            ]
            elements
        ]


type MenuStatus
    = Open
    | Closed


roundMenu : Int -> Float -> Element msg
roundMenu radius rotation =
    roundButton radius rotation <|
        [ (el
            [ Element.width (px 8)
            , Element.height (px 26)
            , Background.color (Colors.secondary 1)
            , Border.rounded 3
            , Element.inFront
                (el
                    [ Element.width (px 26)
                    , Element.height (px 8)
                    , Background.color (Colors.secondary 1)
                    , centerX
                    , centerY
                    , Border.rounded 3
                    ]
                    (Element.text "")
                )
            ]
            (Element.text "")
          )
        ]


openRoundMenu : Int -> Element msg
openRoundMenu radius =
    roundMenu radius 0.79


closedRoundMenu : Int -> Element msg
closedRoundMenu radius =
    roundMenu radius 0


editButton : Int -> Float -> Element msg
editButton radius rotation =
    roundButton radius rotation <|
        [ el [ Element.scale 0.3, Element.moveUp 36, Element.rotate 3.8 ] (pencilIcon (Colors.secondary 1) True) ]


triangle color outline =
    let
        style =
            case outline of
                True ->
                    [ Border.width 6, Border.color color ]

                False ->
                    [ Background.color color ]
    in
        row
            [ Element.width (px 40)
            , Element.height (px 28)
            , Border.widthEach
                { bottom =
                    if outline then
                        7
                    else
                        0
                , left = 0
                , right = 0
                , top = 0
                }
            , Border.rounded 4
            , Border.color color
            , Element.clip
            ]
            [ el (List.append [ Element.moveDown 10, centerX, Border.rounded 4, Element.width (px 40), Element.height (px 40), Element.rotate 0.79 ] style) (Element.text "") ]


pencilIcon color outline =
    let
        style =
            (if outline then
                [ Border.width 6, Border.color color ]
             else
                [ Background.color color ]
            )
    in
        column [ Element.spacing 2, Element.width (px 40), Element.height (px 116) ]
            [ el [] (triangle color outline)
            , el [ Element.width Element.fill, Element.height Element.fill, Background.color color ] (Element.text "")
            , el
                (List.append
                    [ Element.width Element.fill
                    , Element.height (px 20)
                    , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 12, bottomRight = 12 }
                    ]
                    style
                )
                (Element.text "")
            ]
