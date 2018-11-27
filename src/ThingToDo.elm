module ThingToDo exposing (..)


type alias ThingToDo =
    { name : String, description : String, completed : Bool, deleted : Bool, editing : Bool }


new : ThingToDo
new =
    { name = "", description = "", completed = False, deleted = False, editing = True }


delete : ThingToDo -> ThingToDo
delete task =
    { task | deleted = True }


complete : ThingToDo -> ThingToDo
complete task =
    { task | completed = True }
