port module AB exposing
    ( Test
    , TestVersion
    , Version(..)
    , intToVersion
    , loadTests
    , saveTests
    , testsDecoder
    , testsEncoder
    , versionToString
    , versionToVersionId
    , versions
    )

import Json.Decode as Decode exposing (Decoder, field, int, map4, string)
import Json.Encode as Encode
import List.Extra as Extra
import Random exposing (Generator)


port loadTests : (Decode.Value -> msg) -> Sub msg


port storeTests : Decode.Value -> Cmd a



-- types


type Version
    = A
    | B


type alias Test =
    { dimension : Int
    , testId : String
    , aId : String
    , bId : String
    }


type alias TestVersion =
    { testId : String
    , dimension : Int
    , version : Version
    , versionId : String
    }



-- helper functions


saveTests : List TestVersion -> Cmd a
saveTests =
    storeTests << testsEncoder


intToVersion : Int -> Version
intToVersion i =
    case i of
        1 ->
            A

        2 ->
            B

        _ ->
            A


versions : Int -> Generator (List Version)
versions size =
    Random.list size (Random.map intToVersion (Random.int 1 2))


versionToVersionId : List Test -> String -> Version -> String
versionToVersionId tests testId version =
    let
        workingTest =
            Extra.find (\x -> x.testId == testId) tests
    in
    case ( version, workingTest ) of
        ( A, Just test ) ->
            test.aId

        ( B, Just test ) ->
            test.bId

        ( _, Nothing ) ->
            ""


versionToString : Version -> String
versionToString version =
    case version of
        A ->
            "A"

        B ->
            "B"


testDecoder : Decoder TestVersion
testDecoder =
    map4 TestVersion
        (field "testId" string)
        (field "dimension" int)
        (field "version" versionDecoder)
        (field "versionId" string)


testsDecoder : Decoder (List TestVersion)
testsDecoder =
    Decode.list testDecoder


versionDecoder : Decoder Version
versionDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "A" ->
                        Decode.succeed A

                    "B" ->
                        Decode.succeed B

                    _ ->
                        Decode.fail "Unknown version."
            )


testsEncoder : List TestVersion -> Decode.Value
testsEncoder ts =
    Encode.list testEncoder ts


testEncoder : TestVersion -> Decode.Value
testEncoder test =
    Encode.object
        [ ( "testId", Encode.string test.testId )
        , ( "dimension", Encode.int test.dimension )
        , ( "version", Encode.string (versionToString test.version) )
        , ( "versionId", Encode.string test.versionId )
        ]
