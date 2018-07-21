module GalleriaGenerator.Galleria exposing (Gallery, Message, update, view)

import Html
import Html.Attributes as Attribute


-- Model


type alias Gallery =
    { title : String
    , changingTitle : Bool
    , photos : List Photo
    }


type alias Photo =
    { src : String
    , title : Maybe String
    , description : Maybe String
    }



-- Update


type Message
    = ChangeTitle
    | UpdateTitle String


update : Message -> Gallery -> ( Gallery, Cmd msg )
update message gallery =
    case message of
        ChangeTitle ->
            ( { gallery | changingTitle = True }, Cmd.none )

        UpdateTitle newTitle ->
            ( { gallery | changingTitle = False, title = newTitle }, Cmd.none )



-- View


view : Gallery -> Html.Html Message
view gallery =
    Html.div [ Attribute.class "gallery" ]
        [ Html.h1 [ Attribute.class "title" ] [ Html.text gallery.title ]
        ]
