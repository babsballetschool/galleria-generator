module GalleriaGenerator.Events exposing (onKeyDown, whenEnter)

import Html
import Html.Events as Event
import Json.Decode as Json

onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    Event.on "keydown" (Json.map tagger Event.keyCode)

whenEnter : msg -> msg -> Int -> msg
whenEnter onEnterMessage defaultMessage index =
    if index == 13 then
        onEnterMessage
    else
        defaultMessage
