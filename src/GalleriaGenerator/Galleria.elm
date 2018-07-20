module GalleriaGenerator.Galleria exposing (Gallery)

-- Model


type alias Gallery =
    { title : String
    , photos : List Photo
    }


type alias Photo =
    { src : String
    , title : Maybe String
    , description : Maybe String
    }



-- Update


type GalleryMessage
    = ChangeTitle String

update : GalleryMessage -> Gallery -> (Gallery, Cmd msg)
update message gallery =
    case message of
        ChangeTitle newTitle -> ({ gallery | title = newTitle }, Cmd.none)

