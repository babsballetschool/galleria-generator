module GalleriaGenerator.Galleria exposing (Gallery, Message(..), update, view, encodeGallery)

import Html
import Html.Attributes as Attribute
import Html.Events as Event
import Json.Encode as Encode
import GalleriaGenerator.Events exposing (onKeyDown, whenEnter)


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



-- Encode


encodeGallery : Gallery -> Encode.Value
encodeGallery gallery =
    let
        photos =
            List.map encodePhoto gallery.photos
    in
        Encode.object
            [ ( "title", Encode.string gallery.title )
            , ( "photos", Encode.list photos )
            ]


encodePhoto : Photo -> Encode.Value
encodePhoto photo =
    let
        encodeOptionalString wrappedValue =
            case wrappedValue of
                Just value ->
                    Encode.string value

                Nothing ->
                    Encode.null
    in
        Encode.object
            [ ( "src", Encode.string photo.src )
            , ( "title", encodeOptionalString photo.title )
            , ( "description", encodeOptionalString photo.description )
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
                            photo : Photo
                            photo =
                                { src = src, title = Nothing, description = Nothing }
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


photosView : List Photo -> List (Html.Html Message)
photosView photos =
    List.map photoView photos


photoView : Photo -> Html.Html Message
photoView photo =
    let
        photoTitle =
            Maybe.withDefault "" photo.title

        photoDescription =
            Maybe.withDefault "" photo.description
    in
        Html.div [ Attribute.class "photo" ]
            [ Html.span [ Attribute.class "source" ] [ Html.text photo.src ]
            , Html.label [ Attribute.class "title-label", Attribute.for "photo-title" ] [ Html.text "title:" ]
            , Html.input [ Attribute.type_ "input", Attribute.placeholder "title", Attribute.value photoTitle, Attribute.name "photo-title" ] []
            , Html.label [ Attribute.class "description-label", Attribute.for "photo-description" ] [ Html.text "description:" ]
            , Html.input [ Attribute.type_ "input", Attribute.placeholder "description", Attribute.value photoDescription, Attribute.name "photo-description" ] []
            ]
