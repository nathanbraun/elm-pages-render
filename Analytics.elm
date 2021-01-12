port module Analytics exposing (Event(..), trackEvent, trackPageNavigation)

import AB exposing (TestVersion)
import Json.Decode as Decode
import Json.Encode as Encode
import Types exposing (Msg(..))


port trackAnalytics : Decode.Value -> Cmd a


type Event
    = NavigateToPage (List TestVersion) String
    | ClickSendOwlButton (List TestVersion) String
    | SubmitEmailEvent (List TestVersion) String String


trackEvent : Event -> Cmd a
trackEvent =
    trackAnalytics << analyticsPayload


trackPageNavigation : List TestVersion -> String -> Cmd a
trackPageNavigation tests =
    NavigateToPage tests >> trackEvent


analyticsPayload : Event -> Decode.Value
analyticsPayload event =
    Encode.object
        [ ( "action", Encode.string <| eventToAction event )
        , ( "data", eventToPayload event )
        ]


eventToAction : Event -> String
eventToAction event =
    case event of
        NavigateToPage _ _ ->
            "navigateToPage"

        ClickSendOwlButton _ _ ->
            "clickSendOwlButton"

        SubmitEmailEvent _ _ _ ->
            "submitEmail"


eventToPayload : Event -> Encode.Value
eventToPayload event =
    case event of
        NavigateToPage tests url ->
            Encode.object
                [ ( "tests", AB.testsEncoder tests )
                , ( "url", Encode.string url )
                ]

        ClickSendOwlButton tests id ->
            Encode.object
                [ ( "sendOwlTests", AB.testsEncoder tests )
                , ( "section", Encode.string id )
                ]

        SubmitEmailEvent tests id email ->
            Encode.object
                [ ( "id", Encode.string id )
                , ( "email", Encode.string email )
                , ( "emailTests", AB.testsEncoder tests )
                ]
