module GalleriaGenerator exposing (..)

import Html exposing (program)
import Html.Attributes as Attribute
import Html.Events as Event
import Json.Decode as Json
import GalleriaGenerator.Galleria as Galleria
import GalleriaGenerator.Events exposing (onKeyDown, whenEnter)


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
    let
        makeGalleryMessage : Galleria.Message -> Message
        makeGalleryMessage message =
            GalleryMessage message

        addPhotoOnEnter =
            (transform makeGalleryMessage whenEnter) (Galleria.AddPhoto application.photoSource) Galleria.DoNothing
    in
        Html.div [ Attribute.class "application" ]
            [ Html.input
                [ Attribute.type_ "input"
                , Attribute.placeholder "source"
                , Event.onInput UpdatePhotoSource
                , Event.onBlur (GalleryMessage (Galleria.AddPhoto application.photoSource))
                , onKeyDown addPhotoOnEnter
                ]
                []
            , Html.map makeGalleryMessage (Galleria.view application.gallery)
            ]


transform : (a -> b) -> (b -> b -> Int -> c) -> (a -> a -> Int -> c)
transform mapper function =
    \left right index -> function (mapper left) (mapper right) index
