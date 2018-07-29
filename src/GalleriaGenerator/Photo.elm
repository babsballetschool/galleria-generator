module GalleriaGenerator.Photo exposing (Photo, encode, view, Message, update, new)

import Html
import Html.Attributes as Attribute
import Html.Events as Event
import Json.Encode as Encode
import GalleriaGenerator.Events exposing (onKeyDown, whenEnter)


-- Model


type alias Photo =
    { src : String
    , title : Maybe String
    , changingTitle : Bool
    , description : Maybe String
    , changingDescription : Bool
    }


new : String -> Photo
new src =
    { src = src
    , title = Nothing
    , changingTitle = False
    , description = Nothing
    , changingDescription = False
    }



-- Encoding


encode : Photo -> Encode.Value
encode photo =
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
    | ChangeDescription
    | UpdateDescription String
    | ChooseDescription


update : Message -> Photo -> ( Photo, Cmd msg )
update message photo =
    let
        nextPhoto =
            case message of
                DoNothing ->
                    photo

                ChangeTitle ->
                    { photo | changingTitle = True }

                UpdateTitle title ->
                    if String.isEmpty title then
                        { photo | title = Nothing }
                    else
                        { photo | title = Just title }

                ChooseTitle ->
                    { photo | changingTitle = False }

                ChangeDescription ->
                    { photo | changingDescription = True }

                UpdateDescription description ->
                    if String.isEmpty description then
                        { photo | description = Nothing }
                    else
                        { photo | description = Just description }

                ChooseDescription ->
                    { photo | changingDescription = False }
    in
        ( nextPhoto, Cmd.none )



-- View


view : Photo -> Html.Html Message
view photo =
    let
        titleValue photo =
            Maybe.withDefault "" photo.title

        descriptionValue photo =
            Maybe.withDefault "" photo.description
    in
        Html.div [ Attribute.class "photo" ]
            [ Html.span [ Attribute.class "source" ] [ Html.text photo.src ]
            , Html.label
                [ Attribute.class "title-label"
                , Attribute.for "photo-title"
                ]
                [ Html.text "title:" ]
            , (photoAttribute .changingTitle
                titleValue
                photo
                "title"
                "photo-title"
                ChangeTitle
                ChooseTitle
                UpdateTitle
              )
            , Html.label
                [ Attribute.class "description-label"
                , Attribute.for "photo-description"
                ]
                [ Html.text "description:" ]
            , (photoAttribute
                .changingDescription
                descriptionValue
                photo
                "description"
                "photo-description"
                ChangeDescription
                ChooseDescription
                UpdateDescription
              )
            ]


photoAttribute : (Photo -> Bool) -> (Photo -> String) -> Photo -> String -> String -> Message -> Message -> (String -> Message) -> Html.Html Message
photoAttribute changing valueOf photo placeholder name startMessage stopMessage updateMessage =
    let
        value =
            valueOf photo
    in
        if changing photo then
            Html.input
                [ Attribute.type_ "input"
                , Attribute.placeholder placeholder
                , Attribute.name name
                , Attribute.value value
                , Event.onInput updateMessage
                , onKeyDown (whenEnter stopMessage DoNothing)
                ]
                []
        else
            Html.span
                [ Attribute.class "photo title"
                , Event.onClick startMessage
                ]
                [ Html.text ("'" ++ value ++ "'") ]
