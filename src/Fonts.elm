module Fonts exposing (..)

import Element.Font as Font


type MyFonts
    = Headline
    | PrimarySans
    | SecondarySans
    | PrimarySerif
    | SecondarySerif
    | Fallback


fontStack : MyFonts -> Font.Font
fontStack font =
    case font of
        Headline ->
            Font.external { url = "https://fonts.googleapis.com/css?family=Dosis", name = "dosis" }

        PrimarySans ->
            Font.external { url = "https://fonts.googleapis.com/css?family=Roboto", name = "roboto" }

        SecondarySans ->
            Font.sansSerif

        PrimarySerif ->
            Font.external { url = "https://fonts.googleapis.com/css?family=Esteban", name = "esteban" }

        SecondarySerif ->
            Font.serif

        Fallback ->
            Font.sansSerif
