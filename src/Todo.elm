module Todo exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, span, text, input, h1, ul, li, label)
import Html.Events exposing (on, onClick, onInput, keyCode)
import Html.Attributes exposing (checked, style, placeholder, value, type_)
import Json.Decode as Json

import Events exposing (onEnter)

-- MAIN

main =
  Browser.element 
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


-- MODEL

type TodoStatus = Complete | Incomplete

notTodo : TodoStatus -> TodoStatus
notTodo status =
  case status of
    Complete -> Incomplete
    Incomplete -> Complete

type alias Todo =
  { id : Int
  , label : String
  , status : TodoStatus
  }

type alias Model =
  { todos : List Todo
  , newTodo : String
  }

init : () -> (Model, Cmd Msg)
init _ =
  ({ todos = [], newTodo = "" }
  , Cmd.none
  )


-- UPDATE

-- Incremental IDs for now. Depends on Todos being prepended to list.
nextId : List Todo -> Int
nextId todos =
  case List.head todos of
    Just todo ->
      todo.id + 1
    
    Nothing ->
      1

type Msg 
  = UpdateNewTodo String
  | ToggleTodo Int
  | CreateTodo
  | ClearComplete

toggleTodo : Int -> List Todo -> List Todo
toggleTodo idToToggle list =
  let
    toggle : Todo -> Todo
    toggle todo =
      if todo.id == idToToggle then
        { todo | status = notTodo todo.status }
      else
        todo
  in
    List.map toggle list

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateNewTodo str ->
      ({ model | newTodo = str }, Cmd.none)
    
    CreateTodo ->
      let
        newTodo = ""
        todo =
          { id = nextId model.todos
          , label = model.newTodo
          , status = Incomplete }
      in
      ({ model | newTodo = newTodo, todos = todo :: model.todos }, Cmd.none)

    ToggleTodo id ->
      let
        todos = toggleTodo id model.todos
      in
      ({ model | todos = todos }, Cmd.none)
    
    ClearComplete ->
      let
          incomplete = List.filter (\todo -> todo.status == Incomplete) model.todos
      in
      ({ model | todos = incomplete }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Todo?" ]
    , input [ placeholder "What do you need to do?", value model.newTodo, onInput UpdateNewTodo, onEnter CreateTodo ] []
    , button [ onClick CreateTodo ] [ text "Create" ]
    , ul [] (List.map renderTodo model.todos)
    , clearTasksButton model
    ]

statusToBool : TodoStatus -> Bool
statusToBool status =
  case status of
    Complete -> True
    Incomplete -> False

renderTodo : Todo -> Html Msg
renderTodo todo =
  li []
    [ label []
      [ input [ type_ "checkbox", statusToBool todo.status |> checked, onClick (ToggleTodo todo.id)  ] []
      , span [] [ text todo.label ]
      ]
    ]

clearTasksButton : Model -> Html Msg
clearTasksButton model =
  if List.length (List.filter (\todo -> todo.status == Complete) model.todos) > 0 then
    button [ onClick ClearComplete ] [ text "Clear completed tasks" ]
  else
    text ""
