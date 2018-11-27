module Models exposing (..)

import ThingToDo exposing (..)
import Dict exposing (..)
import Time exposing (..)


type alias Model =
    { tasks : Dict Int ThingToDo, active_task_id : Maybe Int, last_edit : Edit }


type Edit
    = Previous Model
    | NoEdit
