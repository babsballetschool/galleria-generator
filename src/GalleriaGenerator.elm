module GalleriaGenerator exposing (..)

import Html exposing (program)
import Html.Attributes as Attribute
import GalleriaGenerator.Galleria as Galleria


main : Program Never Galleria.Gallery Galleria.Message
main =
    program
        { init = init
        , view = Galleria.view
        , update = Galleria.update
        , subscriptions = \_ -> Sub.none
        }


init : ( Galleria.Gallery, Cmd Galleria.Message )
init =
    ( emptyGallery
    , Cmd.none
    )


emptyGallery : Galleria.Gallery
emptyGallery =
    { title = "Change me"
    , changingTitle = False
    , photos = []
    }



-- View


view : Galleria.Gallery -> Html.Html Galleria.Message
view gallery =
    Html.div [ Attribute.class "application" ]
        [ Galleria.view gallery
        ]
