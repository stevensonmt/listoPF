module Updates exposing (..)

import Messages exposing (..)
import Models exposing (..)
import ThingToDo exposing (..)
import List.Extra exposing (updateIf)
import Dict exposing (..)
import Time exposing (now, posixToMillis)
import Task


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddThingToDo thing ->
            let
                id =
                    posixToMillis (Task.perform GetNewId Time.now)
            in
                { tasks = Dict.insert id thing, active_task_id = Just id, last_edit = Previous model }



{--EditThingToDo thing ->--}
--{ tasks = updateIf (\x -> x.id == thing.id) (\x -> { x | editing = True }) model.tasks, active_task_id = Just thing.id, last_edit = Previous model }
--UpdateThingToDo thing ->
--{ tasks = updateIf (\x -> x.id == thing.id) (\x -> thing) model.tasks, active_task_id = Just thing.id, last_edit = Previous model }
--CompleteThingToDo thing ->
--{ tasks = updateIf (\x -> x.id == thing.id) (\x -> { x | completed = True }) model.tasks, active_task_id = Just thing.id, last_edit = Previous model }
--DeleteThingToDo thing ->
--{ tasks = updateIf (\x -> x.id == thing.id) (\x -> { x | deleted = True }) model.tasks, active_task_id = Just thing.id, last_edit = Previous model }
--ClearDeleted ->
--{ tasks = (clearDeleted model.tasks), active_task_id = Nothing, last_edit = Previous model }
--ClearCompleted ->
--{ tasks = (clearCompleted model.tasks), active_task_id = Nothing, last_edit = Previous model }
--ClearAll ->
--{ tasks = [], active_task_id = Nothing, last_edit = Previous model }
--Rollback ->
--case model.last_edit of
--Previous x ->
--x
--_ ->
{--model--}


clearDeleted : List ThingToDo -> List ThingToDo
clearDeleted tasks =
    List.filter (\x -> x.deleted == True) tasks


clearCompleted : List ThingToDo -> List ThingToDo
clearCompleted tasks =
    List.filter (\x -> x.completed == True) tasks


newThingToDo : Model -> String -> String -> ThingToDo
newThingToDo model name desc =
    let
        lastTask =
            List.head (List.reverse (List.sortBy .id model.tasks))

        newId =
            case lastTask of
                Just x ->
                    x.id + 1

                _ ->
                    0
    in
        { id = newId, name = name, description = desc, completed = False, deleted = False, editing = True }
