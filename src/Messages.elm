module Messages exposing (..)

import ThingToDo exposing (..)
import Time


type Msg
    = AddThingToDo ThingToDo
    | EditThingToDo ThingToDo
    | UpdateThingToDo ThingToDo
    | CompleteThingToDo ThingToDo
    | DeleteThingToDo ThingToDo
    | ClearDeleted
    | ClearCompleted
    | ClearAll
    | Rollback
    | GetNewId Time.Posix
