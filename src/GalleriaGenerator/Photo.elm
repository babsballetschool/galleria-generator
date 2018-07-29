module GalleriaGenerator.Photo exposing (Photo, encode, view)

import Html
import Html.Attributes as Attribute
import Json.Encode as Encode


-- Model


type alias Photo =
    { src : String
    , title : Maybe String
    , changingTitle : Bool
    , description : Maybe String
    , changingDescription : Bool
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



-- View


view : Photo -> Html.Html msg
view photo =
    let
        titleValue photo =
            Maybe.withDefault "" photo.title

        descriptionValue photo =
            Maybe.withDefault "" photo.description
    in
        Html.div [ Attribute.class "photo" ]
            [ Html.span [ Attribute.class "source" ] [ Html.text photo.src ]
            , Html.label [ Attribute.class "title-label", Attribute.for "photo-title" ] [ Html.text "title:" ]
            , (photoAttribute .changingTitle titleValue photo "title" "photo-title")
            , Html.label [ Attribute.class "description-label", Attribute.for "photo-description" ] [ Html.text "description:" ]
            , (photoAttribute .changingDescription descriptionValue photo "description" "photo-description")
            ]


photoAttribute : (Photo -> Bool) -> (Photo -> String) -> Photo -> String -> String -> Html.Html msg
photoAttribute changing valueOf photo placeholder name =
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
                ]
                []
        else
            Html.text ("'" ++ value ++ "'")
