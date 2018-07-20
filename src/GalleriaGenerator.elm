module GalleriaGenerator exposing (..)

import Html exposing (program)
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
    , photos = []
    }
