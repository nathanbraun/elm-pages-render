module MailerLite exposing
    ( MailerLiteResponse
    , apiUrl
    , emailResponseDecoder
    , submitEmailEncoder
    )

import Http
import Json.Decode exposing (Decoder, field, int, map2, string)
import Json.Encode as Encode


apiUrl : String
apiUrl =
    "https://us-central1-custom-plating-146421.cloudfunctions.net/function-2"



-- groupId : String
-- groupId =
--     "106262005"


submitEmailEncoder : String -> String -> Encode.Value
submitEmailEncoder group email =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "group", Encode.string group )
        ]


type alias MailerLiteResponse =
    { id : Int
    , email : String
    }


emailResponseDecoder : Decoder MailerLiteResponse
emailResponseDecoder =
    map2 MailerLiteResponse
        (field "id" int)
        (field "email" string)
