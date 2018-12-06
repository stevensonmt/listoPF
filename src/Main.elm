module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html
import Html.Attributes exposing (id)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Element.Events


--import List.Extra exposing (..)

import ThingToDo exposing (..)
import Task
import Colors exposing (..)
import Fonts exposing (..)
import Dict exposing (..)
import Time exposing (..)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions model =
    Sub.none



-- MODEL --


type alias Model =
    { tasks : Dict Int ThingToDo
    , currentToDoID : Maybe Int
    , currentView : View
    , last_edit : Edit
    , cacheName : String
    , cacheDesc : String
    , openMenu : Menu
    , focusedElement : String
    }


type Menu
    = MainMenu
    | ItemMenu
    | None


type Edit
    = Previous Model
    | NoEdit


type View
    = AddNew
    | ViewAll
    | ViewComplete
    | ViewDeleted
    | ViewActive
    | Editing
    | Statistics


getCurrentToDo : Model -> ThingToDo
getCurrentToDo model =
    Maybe.withDefault ThingToDo.new (Dict.get (Maybe.withDefault 0 model.currentToDoID) model.tasks)



-- INIT --


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tasks = Dict.empty, currentToDoID = Nothing, last_edit = NoEdit, cacheName = "", cacheDesc = "", openMenu = None, currentView = ViewAll, focusedElement = "mainMenu" }, Cmd.none )



-- VIEW --


view : Model -> Html.Html Msg
view model =
    Element.layoutWith { options = [ focusStyle { borderColor = Just (Colors.primary 0.5), backgroundColor = Just (Colors.secondary 1), shadow = Just { color = (Colors.primary 0.5), offset = ( 0, 0 ), blur = 2, size = 1 } } ] } [ padding 30, Background.color (Colors.tertiary 1.0) ] <|
        (baseView model)


baseView : Model -> Element Msg
baseView model =
    (column
        [ width (px 480)
        , height fill
        , centerX
        , alignTop
        , padding 20
        , spacing 20
        , Background.color (Colors.secondary 1.0)
        , Border.rounded 28
        , Border.shadow { offset = ( 1, 1 ), size = 1, blur = 6, color = (Colors.primary 0.3) }
        ]
        [ row
            [ centerX
            , alignTop
            , width (px 400)
            , height (px 80)
            , Font.color (Colors.primary 1.0)
            , Font.size 42
            ]
            [ el [ centerX, centerY ]
                (Element.text "LPF")
            ]
        , column
            [ width fill
            , height fill
            , centerX
            , padding 12
            , spacing 12
            ]
            [ (case model.currentView of
                ViewAll ->
                    listTasks model

                Editing ->
                    editView model (getCurrentToDo model)

                ViewDeleted ->
                    listTasks model

                ViewComplete ->
                    listTasks model

                ViewActive ->
                    listTasks model

                _ ->
                    Element.none
              )
            ]
        , column [ centerX, width fill ]
            [ el [ Element.scale 3, centerX, Element.moveUp 28, Element.moveLeft 30, Element.above (launchMainMenu model) ] Element.none
            , Input.button [ Border.rounded 16, width (px 16), height (px 16), alignBottom, Element.moveUp 32, centerX, Element.scale 3, Element.htmlAttribute (Html.Attributes.id "mainMenu") ] { onPress = Just LaunchMainMenu, label = Element.image [] { src = "src/Icons/burgermenuicon.svg", description = "open main menu icon" } }
            ]
        ]
    )


editView : Model -> ThingToDo -> Element Msg
editView model currTask =
    column
        [ height (px 80)
        , width fill
        , paddingXY 10 0
        , centerX
        , Font.color (Colors.primary 1)
        , spacing 10
        ]
        [ el [] (Element.text ("Editing current item id#: " ++ (Debug.toString model.currentToDoID)))
        , (Input.text [ Border.rounded 8, Background.color (Colors.tertiary 1), Element.focused [ Background.color (Element.rgb255 245 242 238), Border.shadow { offset = ( 0, 0 ), size = 1, blur = 8, color = (Colors.primary 0.5) } ], Input.focusedOnLoad, Element.htmlAttribute (Html.Attributes.id "task name input") ]
            { onChange = \text -> UpdateToDoName text
            , text = model.cacheName
            , placeholder = Nothing
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Add Task Name Here")
            }
          )
        , (Input.text [ Border.rounded 8, Background.color (Colors.tertiary 1), Element.focused [ Background.color (Element.rgb255 245 242 238), Border.shadow { offset = ( 0, 0 ), size = 1, blur = 8, color = (Colors.primary 0.5) } ] ]
            { onChange = \text -> UpdateToDoDesc text
            , text = model.cacheDesc
            , placeholder = Nothing
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Add Task Description Here")
            }
          )
        , Input.button
            [ Border.rounded 8
            , Background.color (Colors.primary 1.0)
            , Font.color
                (if String.isEmpty model.cacheName then
                    Colors.secondary 0.3
                 else
                    Element.rgb255 245 242 238
                )
            , Font.bold
            , Font.size 24
            , Font.variant Font.smallCaps
            , padding 10
            , Element.focused [ Background.color (Colors.primary 0.8) ]
            ]
            { onPress =
                (if String.isEmpty model.cacheName then
                    Nothing
                 else
                    Just UpdateThingToDo
                )
            , label = Element.text "Submit"
            }
        ]


listTasks : Model -> Element Msg
listTasks model =
    let
        ( title, tasklist, styleList ) =
            case model.currentView of
                ViewAll ->
                    ( "All Tasks", (Dict.toList model.tasks), ( listItemStyle Active, ( listItemStyle Deleted, listItemStyle Completed ) ) )

                ViewDeleted ->
                    ( "Deleted Tasks", (List.filter (\( id, task ) -> task.deleted) (Dict.toList model.tasks)), ( [], ( [], [] ) ) )

                ViewComplete ->
                    ( "Completed Tasks", (List.filter (\( id, task ) -> task.completed) (Dict.toList model.tasks)), ( [], ( [], [] ) ) )

                _ ->
                    ( "", (Dict.toList model.tasks), ( [], ( [], [] ) ) )
    in
        column [ width fill ]
            (List.append [ row [ centerX, paddingXY 12 8 ] [ Element.text title ] ]
                (List.map
                    (\( id, task ) ->
                        Input.button
                            (if Just id == model.currentToDoID then
                                (List.append [ Element.onLeft (launchItemMenu model) ]
                                    (Tuple.first styleList)
                                )
                             else if task.deleted then
                                Tuple.first (Tuple.second styleList)
                             else if task.completed then
                                Tuple.second (Tuple.second styleList)
                             else
                                []
                            )
                            (let
                                leader =
                                    if model.currentView /= ViewAll then
                                        " "
                                    else
                                        String.fromChar
                                            (if task.completed then
                                                '✓'
                                             else if task.deleted then
                                                '✘'
                                             else
                                                '☉'
                                            )
                             in
                                { onPress =
                                    (if Just id /= model.currentToDoID then
                                        Just (LaunchItemMenu id)
                                     else
                                        Nothing
                                    )
                                , label = Element.text (leader ++ " " ++ task.name)
                                }
                            )
                    )
                    tasklist
                )
            )



--listTasksView : Model -> Element Msg
--listTasksView model =
--listTasks "All Tasks" model.currentToDoID (Dict.toList model.tasks) model.openMenu
--listDeletedTasksView : Model -> Element Msg
--listDeletedTasksView model =
--listTasks "Deleted Tasks" model.currentToDoID (List.filter (\( id, task ) -> task.deleted) (Dict.toList model.tasks)) model.openMenu
--listCompletedTasksView : Model -> Element Msg
--listCompletedTasksView model =
--listTasks "Completed Tasks" model.currentToDoID (List.filter (\( id, task ) -> task.completed) (Dict.toList model.tasks)) model.openMenu
--listActiveTasksView : Model -> Element Msg
--listActiveTasksView model =
--column []
--((List.map
--(\( id, task ) ->
--Input.button [ width (px 120), height (px 36), Font.size 14, padding 10, mouseOver [ Background.color (Colors.primary 0.6) ] ] { onPress = Just (LaunchItemMenu id), label = Element.text task.name }
--)
--(Dict.toList (Dict.filter (\id task -> not (task.completed || task.deleted)) (model.tasks)))
--)
--)


type ListItemStyle
    = Active
    | Deleted
    | Completed


listItemStyle : ListItemStyle -> List (Attribute msg)
listItemStyle item =
    case item of
        Active ->
            [ Font.bold ]

        Deleted ->
            [ Font.strike, Font.color (Colors.primary 0.5) ]

        Completed ->
            [ Font.color (Colors.primary 0.7) ]


launchItemMenu : Model -> Element Msg
launchItemMenu model =
    case model.openMenu of
        ItemMenu ->
            (column
                [ centerX
                , centerY
                , Element.moveUp 14
                , Element.moveRight 30
                , Element.behindContent (Element.image [ Element.rotate -1.571, Element.scale 2 ] { src = "src/Icons/arcmenu.svg", description = "background for item menu" })
                , padding 10
                , spacing 4
                , width (px 100)
                , height (px 40)
                , Element.Events.onMouseLeave CloseModal
                ]
                [ Input.button
                    [ width (px 36) --48)
                    , height (px 36) --48)
                    , Border.rounded 36 --48
                    , Background.color (Colors.primary 0.0)
                    , Font.size 14
                    , Element.moveUp 90 --100
                    , Element.moveRight 32 --16
                    , padding 2 --10
                    , mouseOver [ Background.color (Colors.primary 0.6) ]
                    ]
                    { onPress = Just (EditToDo (Maybe.withDefault 0 model.currentToDoID))
                    , label = Element.image [ Element.scale 2 ] { src = "src/Icons/editicon.svg", description = "icon to edit item" } --Element.text "Edit ToDo"
                    }
                , Input.button
                    [ width (px 36) --48)
                    , height (px 36) --48)
                    , Border.rounded 36 --48
                    , Background.color (Colors.primary 0.0)
                    , Font.size 14
                    , padding 10
                    , Element.moveUp 64
                    , Element.moveLeft 1 --10
                    , mouseOver [ Background.color (Colors.primary 0.6) ]
                    ]
                    { onPress = Just (CompleteToDo (Maybe.withDefault 0 model.currentToDoID))
                    , label = Element.image [ Element.scale 2 ] { src = "src/Icons/checkmark.svg", description = "icon to mark item complete" } --Element.text "Mark Complete"
                    }
                , Input.button
                    [ width (px 36) --48)
                    , height (px 36) --48)
                    , Border.rounded 36 --48
                    , Background.color (Colors.primary 0.0)
                    , Font.size 14
                    , padding 10
                    , Element.moveUp 18 --42 --28
                    , Element.moveRight 32 --18
                    , mouseOver [ Background.color (Colors.primary 0.6) ]
                    ]
                    { onPress = Just (DeleteToDo (Maybe.withDefault 0 model.currentToDoID))
                    , label = Element.image [ Element.scale 2 ] { src = "src/Icons/trashicon.svg", description = "icon to delete item" } -- Element.text "Delete"
                    }
                ]
            )

        _ ->
            Element.none


launchMainMenu : Model -> Element Msg
launchMainMenu model =
    case model.openMenu of
        MainMenu ->
            case model.currentView of
                ViewAll ->
                    (row
                        [ width (px 100)
                        , height (px 40)
                        , Element.moveDown 8
                        , Element.moveLeft 40
                        , Element.behindContent
                            (Element.image [ Element.scale 1 ]
                                { src = "src/Icons/arcmenu.svg"
                                , description = "background for main menu"
                                }
                            )
                        , Element.Events.onMouseLeave CloseModal
                        ]
                        [ Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 4, Element.moveDown 8 ]
                            { onPress = Just AddNewToDo
                            , label = Element.image [ Element.scale 1 ] { src = "src/Icons/addicon.svg", description = "icon to add item" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 6, Element.moveUp 4 ]
                            { onPress =
                                (if not (Dict.isEmpty model.tasks) then
                                    Just ClearAll
                                 else
                                    Nothing
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , (if not (Dict.isEmpty model.tasks) then
                                        Element.alpha 1
                                       else
                                        Element.alpha 0.3
                                      )
                                    ]
                                    { src = "src/Icons/clearallicon.svg", description = "icon to clear all items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 10, Element.moveUp 8.2 ]
                            { onPress =
                                (if Dict.isEmpty (Dict.filter (\id task -> task.completed) model.tasks) then
                                    Nothing
                                 else
                                    Just ListCompleted
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if Dict.isEmpty (Dict.filter (\id task -> task.completed) model.tasks) then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/viewcompleted.svg", description = "icon to view completed items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 14, Element.moveUp 4 ]
                            { onPress =
                                (if Dict.isEmpty (Dict.filter (\id task -> task.deleted) model.tasks) then
                                    Nothing
                                 else
                                    Just ListDeleted
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if Dict.isEmpty (Dict.filter (\id task -> task.deleted) model.tasks) then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/viewdeleted.svg", description = "icon to view deleted items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 16, Element.moveDown 8 ]
                            { onPress =
                                (if model.last_edit /= NoEdit then
                                    Just Rollback
                                 else
                                    Nothing
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if model.last_edit == NoEdit then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/undoicon.svg", description = "icon to undo last change" }
                            }
                        ]
                    )

                ViewDeleted ->
                    (row [ width (px 100), height (px 40), Element.moveDown 8, Element.moveLeft 40, Element.behindContent (Element.image [ Element.scale 1 ] { src = "src/Icons/arcmenu.svg", description = "background for main menu" }), Element.Events.onClick CloseModal, Element.Events.onMouseLeave CloseModal ]
                        [ Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 4, Element.moveDown 8 ]
                            { onPress = Just AddNewToDo
                            , label = Element.image [ Element.scale 1 ] { src = "src/Icons/addicon.svg", description = "icon to add item" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 6, Element.moveUp 4 ]
                            { onPress = Just ListAll
                            , label = Element.image [ Element.scale 1 ] { src = "src/Icons/viewallicon.svg", description = "icon to view all items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 10, Element.moveUp 8.2 ]
                            { onPress =
                                (if Dict.isEmpty (Dict.filter (\id task -> task.completed) model.tasks) then
                                    Nothing
                                 else
                                    Just ListCompleted
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if Dict.isEmpty (Dict.filter (\id task -> task.completed) model.tasks) then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/viewcompleted.svg", description = "icon to view completed items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 14, Element.moveUp 4 ]
                            { onPress =
                                (if Dict.isEmpty (Dict.filter (\id task -> task.deleted) model.tasks) then
                                    Nothing
                                 else
                                    Just ClearDeleted
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if Dict.isEmpty (Dict.filter (\id task -> task.deleted) model.tasks) then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/cleardeletedicon.svg", description = "icon to clear deleted items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 16, Element.moveDown 8 ]
                            { onPress =
                                (if model.last_edit /= NoEdit then
                                    Just Rollback
                                 else
                                    Nothing
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if model.last_edit == NoEdit then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/undoicon.svg", description = "icon to undo last change" }
                            }
                        ]
                    )

                ViewComplete ->
                    (row [ width (px 100), height (px 40), Element.moveDown 8, Element.moveLeft 40, Element.behindContent (Element.image [ Element.scale 1 ] { src = "src/Icons/arcmenu.svg", description = "background for main menu" }), Element.Events.onClick CloseModal, Element.Events.onMouseLeave CloseModal ]
                        [ Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 4, Element.moveDown 8 ]
                            { onPress = Just AddNewToDo
                            , label = Element.image [ Element.scale 1 ] { src = "src/Icons/addicon.svg", description = "icon to add item" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 6, Element.moveUp 4 ]
                            { onPress = Just ListAll
                            , label = Element.image [ Element.scale 1 ] { src = "src/Icons/viewallicon.svg", description = "icon to view all items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 10, Element.moveUp 8.2 ]
                            { onPress =
                                (if Dict.isEmpty (Dict.filter (\id task -> task.completed) model.tasks) then
                                    Nothing
                                 else
                                    Just ClearComplete
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if Dict.isEmpty (Dict.filter (\id task -> task.completed) model.tasks) then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/clearcompleteicon.svg", description = "icon to clear completed items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 14, Element.moveUp 4 ]
                            { onPress =
                                (if Dict.isEmpty (Dict.filter (\id task -> task.deleted) (model.tasks)) then
                                    Nothing
                                 else
                                    Just ListDeleted
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if Dict.isEmpty (Dict.filter (\id task -> task.deleted) (model.tasks)) then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/viewdeleted.svg", description = "icon to view deleted items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 16, Element.moveDown 8 ]
                            { onPress =
                                (if model.last_edit /= NoEdit then
                                    Just Rollback
                                 else
                                    Nothing
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if model.last_edit == NoEdit then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/undoicon.svg", description = "icon to undo last change" }
                            }
                        ]
                    )

                Editing ->
                    (row
                        [ width (px 100)
                        , height (px 40)
                        , Element.moveDown 8
                        , Element.moveLeft 40
                        , Element.behindContent
                            (Element.image [ Element.scale 1 ]
                                { src = "src/Icons/arcmenu.svg"
                                , description = "background for main menu"
                                }
                            )
                        , Element.Events.onMouseLeave CloseModal
                        ]
                        [ Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 4, Element.moveDown 8 ]
                            { onPress = Nothing --Just AddNewToDo
                            , label = Element.image [ Element.scale 1, Element.alpha 0.4 ] { src = "src/Icons/addicon.svg", description = "icon to add item" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 6, Element.moveUp 4 ]
                            { onPress =
                                (if (Dict.isEmpty model.tasks) then
                                    Nothing
                                 else
                                    Just ListAll
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , (if (Dict.isEmpty model.tasks) then
                                        Element.alpha 0.3
                                       else
                                        Element.alpha 1
                                      )
                                    ]
                                    { src = "src/Icons/viewallicon.svg", description = "icon to list all items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 10, Element.moveUp 8.2 ]
                            { onPress =
                                (if Dict.isEmpty (Dict.filter (\id task -> task.completed) model.tasks) then
                                    Nothing
                                 else
                                    Just ListCompleted
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if Dict.isEmpty (Dict.filter (\id task -> task.completed) model.tasks) then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/viewcompleted.svg", description = "icon to view completed items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 14, Element.moveUp 4 ]
                            { onPress =
                                (if Dict.isEmpty (Dict.filter (\id task -> task.deleted) model.tasks) then
                                    Nothing
                                 else
                                    Just ListDeleted
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if Dict.isEmpty (Dict.filter (\id task -> task.deleted) model.tasks) then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/viewdeleted.svg", description = "icon to view deleted items" }
                            }
                        , Input.button [ width (px 16), height (px 16), Border.rounded 16, Element.moveRight 16, Element.moveDown 8 ]
                            { onPress =
                                (if model.last_edit /= NoEdit then
                                    Just Rollback
                                 else
                                    Nothing
                                )
                            , label =
                                Element.image
                                    [ Element.scale 1
                                    , Element.alpha
                                        (if model.last_edit == NoEdit then
                                            0.4
                                         else
                                            1
                                        )
                                    ]
                                    { src = "src/Icons/undoicon.svg", description = "icon to undo last change" }
                            }
                        ]
                    )

                _ ->
                    Element.none

        _ ->
            Element.none



-- UPDATE --


type Msg
    = AddNewToDo
    | GetID Time.Posix
    | LaunchItemMenu Int
    | LaunchMainMenu
    | EditToDo Int
    | UpdateToDoName String
    | UpdateToDoDesc String
    | UpdateThingToDo
    | DeleteToDo Int
    | CompleteToDo Int
    | ClearComplete
    | ClearDeleted
    | ClearAll
    | Rollback
    | ListDeleted
    | ListCompleted
    | ListAll
      --| ListNotDeleted
    | ListActive
    | CloseModal
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNewToDo ->
            ( { model | openMenu = None }, Task.perform GetID Time.now )

        GetID time ->
            let
                id =
                    Time.posixToMillis time
            in
                ( { model
                    | currentToDoID = Just id
                    , openMenu = None
                    , currentView = Editing
                    , focusedElement = "task name input"
                    , cacheDesc = ""
                    , cacheName = ""
                  }
                , (Task.attempt (\_ -> NoOp) (Dom.focus model.focusedElement))
                  --Cmd.none
                )

        LaunchItemMenu id ->
            ( { model | currentToDoID = Just id, openMenu = ItemMenu }, Cmd.none )

        LaunchMainMenu ->
            ( { model
                | openMenu = MainMenu
                , currentToDoID =
                    (if model.currentView /= Editing then
                        Nothing
                     else
                        model.currentToDoID
                    )
              }
            , Cmd.none
            )

        EditToDo id ->
            let
                task =
                    Dict.get id model.tasks
            in
                case task of
                    Just thingtodo ->
                        ( { model
                            | currentToDoID = Just id
                            , cacheDesc = thingtodo.description
                            , cacheName = thingtodo.name
                            , openMenu = None
                            , currentView = Editing
                          }
                          --, Cmd.none
                        , (Task.attempt (\_ -> NoOp) (Dom.focus model.focusedElement))
                        )

                    _ ->
                        ( { model | tasks = Dict.insert id ThingToDo.new model.tasks, currentToDoID = Just id, openMenu = None, currentView = Editing }, Cmd.none )

        UpdateToDoName name ->
            ( { model | cacheName = name }, Cmd.none )

        UpdateToDoDesc desc ->
            ( { model | cacheDesc = desc }, Cmd.none )

        UpdateThingToDo ->
            let
                newToDo =
                    let
                        currToDo =
                            (Dict.get (Maybe.withDefault 0 model.currentToDoID) model.tasks)
                    in
                        case currToDo of
                            Just thingToDo ->
                                { thingToDo | description = model.cacheDesc, name = model.cacheName, editing = False }

                            _ ->
                                let
                                    tmp =
                                        ThingToDo.new
                                in
                                    { tmp | description = model.cacheDesc, name = model.cacheName, editing = False }
            in
                ( { model
                    | tasks = Dict.update (Maybe.withDefault 0 model.currentToDoID) (\_ -> Just newToDo) model.tasks
                    , currentToDoID = Nothing
                    , cacheName = ""
                    , cacheDesc = ""
                    , openMenu = None
                    , currentView = ViewAll
                    , last_edit = Previous model
                  }
                , Cmd.none
                )

        CompleteToDo id ->
            let
                compTask =
                    ThingToDo.complete (getCurrentToDo model)
            in
                ( { model
                    | tasks = Dict.update id (\_ -> Just compTask) model.tasks
                    , currentToDoID = Nothing
                    , cacheName = ""
                    , cacheDesc = ""
                    , openMenu = None
                    , currentView = ViewAll
                    , last_edit = Previous model
                  }
                , Cmd.none
                )

        DeleteToDo id ->
            let
                delTask =
                    ThingToDo.delete (getCurrentToDo model)
            in
                ( { model
                    | tasks = Dict.update id (\_ -> Just delTask) model.tasks
                    , currentToDoID = Nothing
                    , cacheName = ""
                    , cacheDesc = ""
                    , openMenu = None
                    , currentView = ViewAll
                    , last_edit = Previous model
                  }
                , Cmd.none
                )

        Rollback ->
            let
                mdl =
                    case model.last_edit of
                        Previous mod ->
                            mod

                        NoEdit ->
                            model
            in
                ( { mdl
                    | currentToDoID = Nothing
                    , cacheName = ""
                    , cacheDesc = ""
                    , currentView =
                        (if (mdl.last_edit == NoEdit || model.currentView == Editing) then
                            ViewAll
                         else
                            model.currentView
                        )
                    , openMenu = MainMenu --None
                  }
                , Cmd.none
                )

        ListDeleted ->
            ( { model | currentView = ViewDeleted, openMenu = None }, Cmd.none )

        ListCompleted ->
            ( { model | currentView = ViewComplete, openMenu = None }, Cmd.none )

        ListActive ->
            ( { model | currentView = ViewActive, openMenu = None }, Cmd.none )

        ListAll ->
            ( { model | currentView = ViewAll, openMenu = None, currentToDoID = Nothing }, (Task.attempt (\_ -> NoOp) (Dom.focus model.focusedElement)) )

        ClearAll ->
            ( { model | tasks = (Dict.empty), openMenu = None, currentView = ViewAll, last_edit = Previous model }, Cmd.none )

        ClearComplete ->
            let
                completed =
                    Dict.filter (\id task -> (not task.completed)) model.tasks
            in
                ( { model | tasks = completed, openMenu = None, currentView = ViewAll, last_edit = Previous model }, Cmd.none )

        ClearDeleted ->
            let
                deleted =
                    Dict.filter (\id task -> (not task.deleted)) model.tasks
            in
                ( { model | tasks = deleted, openMenu = None, currentView = ViewAll, last_edit = Previous model }, Cmd.none )

        CloseModal ->
            case model.openMenu of
                ItemMenu ->
                    ( { model | openMenu = None, currentToDoID = Nothing }, Cmd.none )

                MainMenu ->
                    ( { model | openMenu = None }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



{--_ ->--}
{--( model, Cmd.none )--}
