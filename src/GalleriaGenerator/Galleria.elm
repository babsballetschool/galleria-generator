module GalleriaGenerator.Galleria exposing (Gallery, Message(..), update, view, encodeGallery)

import Html
import Html.Attributes as Attribute
import Html.Events as Event
import Json.Encode as Encode
import GalleriaGenerator.Events exposing (onKeyDown, whenEnter)
import GalleriaGenerator.Photo as Photo


-- Model


type alias Gallery =
    { title : String
    , changingTitle : Bool
    , photos : List Photo.Photo
    }



-- Encode


encodeGallery : Gallery -> Encode.Value
encodeGallery gallery =
    let
        photos =
            List.map Photo.encode gallery.photos
    in
        Encode.object
            [ ( "title", Encode.string gallery.title )
            , ( "photos", Encode.list photos )
            ]



-- Update


type Message
    = DoNothing
    | ChangeTitle
    | UpdateTitle String
    | ChooseTitle
    | AddPhoto String


update : Message -> Gallery -> ( Gallery, Cmd msg )
update message gallery =
    let
        nextGallery =
            case message of
                DoNothing ->
                    gallery

                ChangeTitle ->
                    { gallery | changingTitle = True }

                UpdateTitle newTitle ->
                    { gallery | title = newTitle }

                ChooseTitle ->
                    { gallery | changingTitle = False }

                AddPhoto src ->
                    if String.isEmpty src then
                        gallery
                    else
                        let
                            photo =
                                { src = src
                                , title = Nothing
                                , changingTitle = False
                                , description = Nothing
                                , changingDescription = False
                                }
                        in
                            { gallery | photos = gallery.photos ++ [ photo ] }
    in
        ( nextGallery, Cmd.none )



-- View


view : Gallery -> Html.Html Message
view gallery =
    Html.div [ Attribute.class "gallery" ]
        [ (title gallery)
        , Html.div [ Attribute.class "photos" ] (photosView gallery.photos)
        ]


title : Gallery -> Html.Html Message
title gallery =
    if not gallery.changingTitle then
        Html.h1
            [ Attribute.class "title"
            , Event.onClick ChangeTitle
            ]
            [ Html.text gallery.title ]
    else
        Html.span
            [ Attribute.classList
                [ ( "title", True )
                , ( "changing", True )
                ]
            ]
            [ Html.input
                [ Attribute.type_ "input"
                , Attribute.defaultValue gallery.title
                , Event.onInput (\newTitle -> UpdateTitle newTitle)
                , Event.onBlur ChooseTitle
                , onKeyDown (whenEnter ChooseTitle DoNothing)
                ]
                []
            ]


photosView : List Photo.Photo -> List (Html.Html Message)
photosView photos =
    List.map Photo.view photos
