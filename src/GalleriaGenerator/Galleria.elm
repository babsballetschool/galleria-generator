module GalleriaGenerator.Galleria exposing (Gallery)


type alias Gallery =
    { title : String
    , photos : List Photo
    }


type alias Photo =
    { src : String
    , title : Maybe String
    , description : Maybe String
    }
