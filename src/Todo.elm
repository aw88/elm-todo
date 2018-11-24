import Browser
import Html exposing (Html, button, div, span, text, input, h1, ul, li, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (checked, style, placeholder, value, type_)

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

not : TodoStatus -> TodoStatus
not status =
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

toggleTodo : List Todo -> Int -> List Todo
toggleTodo list idToToggle =
  let
    toggle : Todo -> Todo
    toggle todo =
      if todo.id == idToToggle then
        { todo | status = not todo.status }
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
        todos = toggleTodo model.todos id
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
    , input [ placeholder "What do you need to do?", value model.newTodo, onInput UpdateNewTodo ] []
    , button [ onClick CreateTodo ] [ text "Create" ]
    , ul [] (List.map renderTodo model.todos)
    , button [ onClick ClearComplete ] [ text "Clear completed tasks" ]
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