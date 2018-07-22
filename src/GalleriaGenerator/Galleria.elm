module GalleriaGenerator.Galleria exposing (Gallery, Message(..), update, view)

import Html
import Html.Attributes as Attribute
import Html.Events as Event
import Json.Decode as Json


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
                , onKeyDown (whenEnter ChooseTitle)
                ]
                []
            ]


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    Event.on "keydown" (Json.map tagger Event.keyCode)


whenEnter : Message -> Int -> Message
whenEnter message index =
    if index == 13 then
        message
    else
        DoNothing


photosView : List Photo -> List (Html.Html Message)
photosView photos =
    List.map photoView photos


photoView : Photo -> Html.Html Message
photoView photo =
    Html.div [ Attribute.class "photo" ]
        [ Html.span [ Attribute.class "source" ] [ Html.text photo.src ]
        ]
