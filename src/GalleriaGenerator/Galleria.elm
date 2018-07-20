module GalleriaGenerator.Galleria exposing (Gallery, Message, update, view)

import Html
import Html.Attributes as Attribute


-- Model


type alias Gallery =
    { title : String
    , photos : List Photo
    }


type alias Photo =
    { src : String
    , title : Maybe String
    , description : Maybe String
    }



-- Update


type Message
    = ChangeTitle String


update : Message -> Gallery -> ( Gallery, Cmd msg )
update message gallery =
    case message of
        ChangeTitle newTitle ->
            ( { gallery | title = newTitle }, Cmd.none )



-- View


view : Gallery -> Html.Html Message
view gallery =
    Html.div [ Attribute.class "gallery" ]
        [ Html.h1 [ Attribute.class "title" ] [ Html.text gallery.title ]
        ]
