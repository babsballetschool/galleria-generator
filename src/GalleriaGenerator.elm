module GalleriaGenerator exposing (..)

import Html exposing (program)
import Html.Attributes as Attribute
import Html.Events as Event
import Json.Decode as Json
import GalleriaGenerator.Galleria as Galleria


main : Program Never Application Message
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Application, Cmd Message )
init =
    ( emptyApplication
    , Cmd.none
    )


emptyApplication : Application
emptyApplication =
    { photoSource = ""
    , gallery = emptyGallery
    }


emptyGallery : Galleria.Gallery
emptyGallery =
    { title = "Change me"
    , changingTitle = False
    , photos = []
    }



-- Model


type alias Application =
    { photoSource : String
    , gallery : Galleria.Gallery
    }



-- Update


type Message
    = UpdatePhotoSource String
    | GalleryMessage Galleria.Message


update : Message -> Application -> ( Application, Cmd Message )
update message application =
    case message of
        UpdatePhotoSource photoSource ->
            ( { application | photoSource = photoSource }, Cmd.none )

        GalleryMessage galleryMessage ->
            let
                ( nextGallery, command ) =
                    Galleria.update galleryMessage application.gallery
            in
                ( { application | gallery = nextGallery }, Cmd.none )



-- View


view : Application -> Html.Html Message
view application =
    Html.div [ Attribute.class "application" ]
        [ Html.input
            [ Attribute.type_ "input"
            , Attribute.placeholder "source"
            , Event.onInput UpdatePhotoSource
            , Event.onBlur (GalleryMessage (Galleria.AddPhoto application.photoSource))
            , onKeyDown (whenEnter ((GalleryMessage (Galleria.AddPhoto application.photoSource))))
            ]
            []
        , Html.map (\message -> GalleryMessage message) (Galleria.view application.gallery)
        ]


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    Event.on "keydown" (Json.map tagger Event.keyCode)


whenEnter : Message -> Int -> Message
whenEnter message index =
    if index == 13 then
        message
    else
        GalleryMessage Galleria.DoNothing
