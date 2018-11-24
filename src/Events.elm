module Events exposing (onEnter, onKeyDown)

import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json

onKeyDown key msg =
  let
    isKey code =
      if code == key then
        Json.succeed msg
      else
        Json.fail "not ENTER"
  in
  on "keydown" (Json.andThen isKey keyCode)
  

onEnter : msg -> Attribute msg
onEnter =
  onKeyDown 13
