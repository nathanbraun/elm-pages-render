module Render exposing (render, renderer)

import AB exposing (TestVersion, Version(..))
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import Element.Region
import FlatColors.FlatUIPalette as FlatColors
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra as Extra
import Loading exposing (LoaderType(..), defaultConfig, render)
import Markdown.Block as Block exposing (Block, Inline, ListItem(..), Task(..))
import Markdown.Html
import Markdown.Renderer
import RemoteData exposing (RemoteData(..))
import Types exposing (Model, Msg(..))


render : List Block -> Result String (Model -> Element Msg)
render blocks =
    blocks
        |> Markdown.Renderer.render renderer
        |> Result.map
            (\blockViews model ->
                blockViews
                    |> renderAll model
                    |> Element.column [ Element.spacing 8 ]
            )


renderAll : model -> List (model -> view) -> List view
renderAll model =
    List.map ((|>) model)


codeBlock : { body : String, language : Maybe String } -> Model -> Element msg
codeBlock details _ =
    Element.el
        [ Element.Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.padding 20
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text details.body)


code : String -> Model -> Element msg
code snippet _ =
    Element.el
        [ Element.Background.color
            (Element.rgba 0 0 0 0.04)
        , Element.Border.rounded 2
        , Element.paddingXY 5 3
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text snippet)


heading :
    { level : Block.HeadingLevel
    , rawText : String
    , children :
        List (Model -> Element msg)
    }
    -> Model
    -> Element msg
heading { level, rawText, children } model =
    Element.paragraph
        [ Font.size
            (case level of
                Block.H1 ->
                    48

                Block.H2 ->
                    32

                Block.H3 ->
                    28

                Block.H4 ->
                    24

                _ ->
                    20
            )
        , Font.bold
        , Font.family []
        , headingPadding level

        -- , Element.paddingEach { bottom = 0, left = 0, right = 0, top = 15 }
        -- , Element.paddingXY 0 15
        , Element.Region.heading (Block.headingLevelToInt level)
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        (renderAll model children)


rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


renderer : Markdown.Renderer.Renderer (Model -> Element Msg)
renderer =
    { html =
        Markdown.Html.oneOf
            [ Markdown.Html.tag "center"
                (\spacing children model ->
                    Element.column
                        [ fill
                        , Element.spacing
                            (spacing
                                |> Maybe.andThen
                                    String.toInt
                                |> Maybe.withDefault 0
                            )
                        ]
                        (List.map
                            (\x ->
                                Element.row [ Element.centerX ] [ x ]
                            )
                            (renderAll model
                                children
                            )
                        )
                )
                |> Markdown.Html.withOptionalAttribute "spacing"
            , Markdown.Html.tag "email-input"
                (\id group text children model ->
                    case model.submittedEmail of
                        Success _ ->
                            Element.paragraph
                                [ fill
                                , Element.spacing 8
                                , Element.paddingXY 0 10
                                ]
                                [ Element.text "Thanks! Check your email for a link to the sample chapter. "
                                ]

                        Loading ->
                            viewLoading

                        Failure _ ->
                            Element.textColumn [ fill ]
                                [ Element.paragraph
                                    [ fill
                                    , Element.centerX
                                    , Element.spacing 8
                                    , Element.paddingXY 0 10
                                    ]
                                    [ Element.text "Dang! There was an error getting your email. This has been happening occasionally. I'm working on figuring out why, but in the meantime you can grab the preview here:" ]
                                , Element.el
                                    [ Element.centerX
                                    , Element.spacing 8
                                    , Element.paddingXY 0 10
                                    , Font.color (Element.rgb255 7 81 219)
                                    , Font.underline
                                    ]
                                    (Element.link [] { url = "http://static.fantasycoding.com/ltcwbb-preview-BLJUZ.pdf", label = Element.text "Learn to Code with Baseball - Preview" })
                                , Element.paragraph
                                    [ fill
                                    , Element.spacing 8
                                    , Element.paddingXY 0 10
                                    ]
                                    [ Element.text "If you wouldn't mind, email me ("
                                    , Element.link
                                        [ Font.color (Element.rgb255 7 81 219)
                                        ]
                                        { url = "mailto:nate@nathanbraun.com"
                                        , label = Element.text "nate@nathanbraun.com"
                                        }
                                    , Element.text ") and let me know I missed you. Or with any questions, feedback or details on your situation. Always happy to talk!"
                                    ]
                                ]

                        _ ->
                            Element.wrappedRow
                                [ fill
                                , Element.spacing 20
                                , Element.paddingXY 0 10
                                ]
                                [ Element.Input.email
                                    [ Element.width
                                        (Element.fillPortion 1 |> Element.minimum 250)
                                    , onEnter (EmailEnterPressed group)
                                    ]
                                    { onChange = UpdateEmail
                                    , label =
                                        Element.Input.labelHidden "Email"
                                    , text = model.email
                                    , placeholder =
                                        Just
                                            (Element.Input.placeholder []
                                                (Element.text "Email")
                                            )
                                    }
                                , Element.Input.button
                                    [ Element.centerX ]
                                    { onPress =
                                        Just (SubmitEmail group id model.email)
                                    , label =
                                        Element.el
                                            [ Element.Border.width 1
                                            , Element.Border.rounded 5
                                            , Element.Background.color FlatColors.peterRiver
                                            , Element.paddingXY 15 10
                                            ]
                                            (Element.el
                                                [ Font.color
                                                    (Element.rgb255
                                                        255
                                                        255
                                                        255
                                                    )
                                                , Font.regular
                                                ]
                                                (Element.text text)
                                            )
                                    }
                                ]
                )
                |> Markdown.Html.withAttribute "id"
                |> Markdown.Html.withAttribute "group"
                |> Markdown.Html.withAttribute "text"
            , Markdown.Html.tag "callout"
                (\children model ->
                    Element.column
                        [ Element.centerX
                        ]
                        [ Element.paragraph [ Font.size 24 ]
                            (renderAll model
                                children
                            )
                        ]
                )
            , Markdown.Html.tag "plan-container"
                (\children model ->
                    Element.wrappedRow
                        [ Element.spacing 10
                        , Element.paddingXY 0 50

                        -- , Element.explain Debug.todo
                        , Element.width (Element.fill |> Element.maximum 850)
                        ]
                        (renderAll model
                            children
                        )
                )
            , Markdown.Html.tag "plan"
                (\title plan price text url children model ->
                    Element.column
                        [ Element.Border.width 1
                        , Element.Border.rounded 5
                        , Element.padding 10
                        , Element.spacing 5
                        , Element.alignTop

                        -- , Element.width (Element.px 250)
                        , Element.width Element.fill

                        -- , Element.width Element.fill
                        , Element.height Element.fill
                        ]
                        ((Element.el
                            [ Element.centerX
                            , Font.size 24
                            , Font.bold
                            , Font.italic
                            , Element.paddingXY 10 20
                            ]
                            (Element.text title)
                            :: renderAll model
                                children
                         )
                            ++ [ Element.Input.button
                                    [ Element.alignBottom, Element.centerX, Element.paddingXY 0 30 ]
                                    { onPress = Nothing
                                    , label =
                                        Element.link []
                                            { url = url
                                            , label =
                                                Element.column
                                                    [ Element.Border.width 1
                                                    , Element.Border.rounded 5
                                                    , Element.Background.color
                                                        FlatColors.peterRiver
                                                    , Element.paddingXY 12 15
                                                    , Font.color
                                                        (Element.rgb255
                                                            255
                                                            255
                                                            255
                                                        )
                                                    , Font.regular
                                                    , Element.spacing 10
                                                    ]
                                                    [ Element.el
                                                        [ Element.centerX
                                                        ]
                                                        (Element.text text)
                                                    , Element.el
                                                        [ Element.centerX
                                                        , Font.bold
                                                        ]
                                                        (Element.text
                                                            price
                                                        )
                                                    ]
                                            }
                                    }
                               ]
                        )
                )
                |> Markdown.Html.withAttribute "title"
                |> Markdown.Html.withAttribute "plan"
                |> Markdown.Html.withAttribute "price"
                |> Markdown.Html.withAttribute "text"
                |> Markdown.Html.withAttribute "url"
            , Markdown.Html.tag "stars"
                (\children model ->
                    stars
                )
            , Markdown.Html.tag "link"
                (\url label children model ->
                    let
                        source =
                            Element.link
                                [ Element.htmlAttribute
                                    (Html.Attributes.style "display"
                                        "inline-flex"
                                    )
                                ]
                                { url = url
                                , label =
                                    Element.el
                                        [ Font.color
                                            (Element.rgb255 7
                                                81
                                                219
                                            )

                                        -- , Font.underline
                                        ]
                                        (Element.text label)
                                }
                    in
                    Element.paragraph []
                        [ Element.Input.button []
                            { onPress =
                                Just
                                    (ShowGumroad "buy")
                            , label = source
                            }
                        , Element.text "? — 30 day money back guarantee!"
                        ]
                )
                |> Markdown.Html.withAttribute "url"
                |> Markdown.Html.withAttribute "label"
            , Markdown.Html.tag "quote"
                (\name link children model ->
                    let
                        source =
                            case link of
                                Just url ->
                                    Element.link
                                        [ Element.htmlAttribute
                                            (Html.Attributes.style "display"
                                                "inline-flex"
                                            )
                                        ]
                                        { url = url
                                        , label =
                                            Element.el
                                                [ Font.color
                                                    (Element.rgb255 7
                                                        81
                                                        219
                                                    )
                                                , Font.italic
                                                ]
                                                (Element.text name)
                                        }

                                Nothing ->
                                    Element.el [ Font.italic ]
                                        (Element.text name)
                    in
                    Element.paragraph []
                        (renderAll model
                            children
                            ++ [ Element.text " — "
                               , source
                               ]
                        )
                )
                |> Markdown.Html.withAttribute "name"
                |> Markdown.Html.withOptionalAttribute "link"
            , Markdown.Html.tag "grid"
                (\spacing children model ->
                    Element.wrappedRow
                        [ Element.width Element.fill
                        , Element.spacing
                            (spacing
                                |> Maybe.andThen
                                    String.toInt
                                |> Maybe.withDefault 0
                            )
                        ]
                        (renderAll model
                            children
                        )
                )
                |> Markdown.Html.withOptionalAttribute "spacing"
            , Markdown.Html.tag "gift"
                (\label url id children model ->
                    Element.el [ Element.width Element.fill ]
                        (Element.Input.button
                            [ Element.centerX ]
                            { onPress = Nothing
                            , label =
                                Element.link
                                    []
                                    { url = url
                                    , label =
                                        Element.el
                                            [ Element.Border.width 1
                                            , Element.Border.rounded 5
                                            , Element.Background.color
                                                FlatColors.nephritis
                                            , Element.paddingXY 20 15
                                            ]
                                            (Element.el
                                                [ Font.color
                                                    (Element.rgb255
                                                        255
                                                        255
                                                        255
                                                    )
                                                , Font.regular
                                                ]
                                                (Element.text label)
                                            )
                                    }
                            }
                        )
                )
                |> Markdown.Html.withAttribute "label"
                |> Markdown.Html.withAttribute "url"
                |> Markdown.Html.withAttribute "id"
            , Markdown.Html.tag "test"
                (\id version name children model ->
                    let
                        test =
                            Extra.find
                                (\x -> x.testId == id)
                                model.tests
                                |> Maybe.map .version
                    in
                    case ( version, test ) of
                        ( "A", Just A ) ->
                            Element.column [ Element.width Element.fill ]
                                (renderAll model
                                    children
                                )

                        ( "B", Just B ) ->
                            Element.column [ Element.width Element.fill ]
                                (renderAll model
                                    children
                                )

                        _ ->
                            Element.none
                )
                |> Markdown.Html.withAttribute "id"
                |> Markdown.Html.withAttribute "version"
                |> Markdown.Html.withAttribute "name"
            , Markdown.Html.tag "stripe"
                (\children model ->
                    Element.row [ Element.width Element.fill, Element.padding 40 ]
                        [ Element.Input.button
                            [ Element.centerX ]
                            { onPress = Just ShowStripe
                            , label =
                                Element.el
                                    [ Element.Border.width 1
                                    , Element.Border.rounded 5
                                    , Element.Background.color
                                        FlatColors.peterRiver
                                    , Element.paddingXY 15 10
                                    ]
                                    (Element.el
                                        [ Font.color
                                            (Element.rgb255
                                                255
                                                255
                                                255
                                            )
                                        , Font.regular
                                        ]
                                        (Element.text "Buy as a gift!")
                                    )
                            }
                        ]
                )
            , Markdown.Html.tag "section"
                (\width pad_x pad_y spacing background fontColor children model ->
                    let
                        spacing_ =
                            spacing
                                |> Maybe.andThen String.toInt
                                |> Maybe.withDefault 20

                        width_ =
                            width
                                |> Maybe.andThen String.toInt
                                |> Maybe.withDefault 850

                        padX =
                            pad_x
                                |> Maybe.andThen String.toInt
                                |> Maybe.withDefault 30

                        padY =
                            pad_y
                                |> Maybe.andThen String.toInt
                                |> Maybe.withDefault 20

                        background_ =
                            case background of
                                Just "clouds" ->
                                    FlatColors.clouds

                                Just "midnightBlue" ->
                                    FlatColors.midnightBlue

                                _ ->
                                    Element.rgb255 255 255 255

                        fontColor_ =
                            case fontColor of
                                Just "clouds" ->
                                    FlatColors.clouds

                                Just "midnightBlue" ->
                                    FlatColors.midnightBlue

                                _ ->
                                    Element.rgba255 0 0 0 0.8
                    in
                    Element.column
                        [ Element.width
                            (Element.fill
                                |> Element.maximum
                                    width_
                            )
                        , Element.paddingXY padX padY
                        , Element.centerX
                        , Element.spacing spacing_
                        , Element.Background.color background_
                        , Font.color fontColor_
                        ]
                        (renderAll model
                            children
                        )
                )
                |> Markdown.Html.withOptionalAttribute "width"
                |> Markdown.Html.withOptionalAttribute "pad_x"
                |> Markdown.Html.withOptionalAttribute "pad_y"
                |> Markdown.Html.withOptionalAttribute "spacing"
                |> Markdown.Html.withOptionalAttribute "background"
                |> Markdown.Html.withOptionalAttribute "fontColor"
            , Markdown.Html.tag "quote-section"
                (\children model ->
                    Element.column
                        [ fill
                        , Element.spacing 20
                        , Element.paddingXY 0 40
                        ]
                        (renderAll model
                            children
                        )
                )
            , Markdown.Html.tag "image"
                (\src desc max children model ->
                    Element.image
                        [ Element.width
                            (Element.fillPortion 1
                                |> Element.minimum 300
                                |> Element.maximum
                                    (max
                                        |> Maybe.andThen
                                            String.toInt
                                        |> Maybe.withDefault 850
                                    )
                            )
                        ]
                        { src = src
                        , description = desc
                        }
                )
                |> Markdown.Html.withAttribute "src"
                |> Markdown.Html.withAttribute "desc"
                |> Markdown.Html.withOptionalAttribute "max"
            , Markdown.Html.tag "quote"
                (\name link children model ->
                    let
                        source =
                            case link of
                                Just url ->
                                    Element.link
                                        [ Element.htmlAttribute
                                            (Html.Attributes.style "display"
                                                "inline-flex"
                                            )
                                        ]
                                        { url = url
                                        , label =
                                            Element.el
                                                [ Font.color
                                                    (Element.rgb255 7
                                                        81
                                                        219
                                                    )
                                                , Font.italic
                                                ]
                                                (Element.text name)
                                        }

                                Nothing ->
                                    Element.el [ Font.italic ]
                                        (Element.text name)
                    in
                    Element.paragraph []
                        (renderAll model
                            children
                            ++ [ Element.text " — "
                               , source
                               ]
                        )
                )
                |> Markdown.Html.withAttribute "name"
                |> Markdown.Html.withOptionalAttribute "link"
            , Markdown.Html.tag "button"
                (\label url id children model ->
                    Element.el [ Element.width Element.fill ]
                        (Element.Input.button
                            [ Element.centerX
                            ]
                            { onPress = Just (ShowGumroad id)
                            , label =
                                Element.link
                                    []
                                    { url = url
                                    , label =
                                        Element.el
                                            [ Element.Border.width 1
                                            , Element.Border.rounded 5
                                            , Element.Background.color
                                                FlatColors.peterRiver
                                            , Element.paddingXY 20 15
                                            ]
                                            (Element.el
                                                [ Font.color
                                                    (Element.rgb255
                                                        255
                                                        255
                                                        255
                                                    )
                                                , Font.regular
                                                ]
                                                (Element.text label)
                                            )
                                    }
                            }
                        )
                )
                |> Markdown.Html.withAttribute "label"
                |> Markdown.Html.withAttribute "url"
                |> Markdown.Html.withAttribute "id"
            , Markdown.Html.tag "grid"
                (\spacing children model ->
                    Element.wrappedRow
                        [ Element.width Element.fill
                        , Element.spacing
                            (spacing
                                |> Maybe.andThen
                                    String.toInt
                                |> Maybe.withDefault 0
                            )
                        ]
                        (renderAll model
                            children
                        )
                )
                |> Markdown.Html.withOptionalAttribute "spacing"
            , Markdown.Html.tag "image_text2"
                (\src fill1 fill2 children model ->
                    Element.wrappedRow
                        [ Element.spacing 30
                        , Element.paddingXY 30 10
                        ]
                        [ Element.column
                            [ Element.width
                                (Element.fillPortion
                                    (fill2
                                        |> String.toInt
                                        |> Maybe.withDefault 1
                                    )
                                )
                            , Element.alignTop
                            , Element.spacing 8
                            ]
                            (renderAll model
                                children
                            )
                        , Element.image
                            [ Element.centerY
                            , Element.padding 15

                            -- [ Element.centerY
                            , Element.width
                                (Element.fillPortion
                                    (fill1
                                        |> String.toInt
                                        |> Maybe.withDefault 1
                                    )
                                    |> Element.minimum 250
                                    |> Element.maximum 300
                                )
                            ]
                            { src = src
                            , description = "my image"
                            }
                        ]
                )
                |> Markdown.Html.withAttribute "src"
                |> Markdown.Html.withAttribute "fill1"
                |> Markdown.Html.withAttribute "fill2"
            , Markdown.Html.tag "image_text"
                (\src fill1 fill2 children model ->
                    Element.wrappedRow
                        [ Element.spacing 30
                        , Element.paddingXY 30 10
                        ]
                        [ Element.image
                            [ Element.alignTop

                            -- [ Element.centerY
                            , Element.width
                                (Element.fillPortion
                                    (fill1
                                        |> String.toInt
                                        |> Maybe.withDefault 1
                                    )
                                    |> Element.minimum 250
                                 -- |> Element.maximum 400
                                )
                            ]
                            { src = src
                            , description = "my image"
                            }
                        , Element.column
                            [ Element.width
                                (Element.fillPortion
                                    (fill2
                                        |> String.toInt
                                        |> Maybe.withDefault 1
                                    )
                                )
                            , Element.alignTop
                            , Element.spacing 8
                            ]
                            (renderAll model
                                children
                            )
                        ]
                )
                |> Markdown.Html.withAttribute "src"
                |> Markdown.Html.withAttribute "fill1"
                |> Markdown.Html.withAttribute "fill2"
            ]
    , heading = heading
    , paragraph =
        \children model ->
            Element.paragraph
                [ Element.paddingXY 0 5
                , Element.spacing 8
                , Element.width
                    Element.fill

                -- , Font.light
                ]
                (renderAll model
                    children
                )
    , thematicBreak =
        \_ ->
            Element.row
                [ fill
                , Element.Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Element.paddingXY 0 0
                , Element.Border.color (Element.rgb255 145 145 145)
                , Element.Background.color (Element.rgb255 255 255 255)
                ]
                [ Element.none ]
    , text = \children _ -> Element.text children
    , strong =
        \content model ->
            Element.row [ Font.bold ]
                (renderAll model
                    content
                )
    , emphasis =
        \content model ->
            Element.row [ Font.italic ]
                (renderAll model
                    content
                )
    , codeSpan = \_ _ -> Element.none
    , link =
        \{ title, destination } body model ->
            Element.link
                [ Element.htmlAttribute
                    (Html.Attributes.style "display"
                        "inline-flex"
                    )
                ]
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 7 81 219)
                        ]
                        (renderAll model body)
                }
    , hardLineBreak = \_ -> Html.br [] [] |> Element.html
    , image =
        \image _ ->
            Element.image [ fill ]
                { src = image.src
                , description = image.alt
                }
    , blockQuote =
        \children model ->
            Element.column
                [ Element.Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 0
                    , left = 10
                    }
                , Element.padding 10
                , Element.Border.color (Element.rgb255 145 145 145)
                , Element.Background.color (Element.rgb255 245 245 245)
                ]
                (renderAll model children)
    , unorderedList =
        \items model ->
            Element.column
                [ Element.spacing 8
                , Element.paddingXY 5 0
                , fill
                ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            Element.row
                                [ Element.alignTop, fill ]
                                ((case task of
                                    IncompleteTask ->
                                        Element.Input.defaultCheckbox False

                                    CompletedTask ->
                                        Element.Input.defaultCheckbox True

                                    NoTask ->
                                        Element.el
                                            [ Font.size 24
                                            , Element.paddingXY 10 0
                                            , Element.alignTop
                                            ]
                                            (Element.text "•")
                                 )
                                    :: Element.text " "
                                    :: [ Element.paragraph
                                            [ Element.spacing
                                                8
                                            ]
                                            (renderAll model children)
                                       ]
                                )
                        )
                )
    , orderedList =
        \startingIndex items model ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row [ Element.alignTop ]
                                    (Element.text
                                        (String.fromInt
                                            (index
                                                + startingIndex
                                            )
                                            ++ " "
                                        )
                                        :: renderAll model
                                            itemBlocks
                                    )
                                ]
                        )
                )
    , codeBlock = codeBlock
    , table = \children model -> Element.column [] (renderAll model children)
    , tableHeader =
        \children model ->
            Element.column []
                (renderAll model
                    children
                )
    , tableBody =
        \children model ->
            Element.column []
                (renderAll model
                    children
                )
    , tableRow = \children model -> Element.row [] (renderAll model children)
    , tableHeaderCell =
        \maybeAlignment children model ->
            Element.paragraph [] (renderAll model children)
    , strikethrough = \_ _ -> Element.none
    , tableCell =
        \_ children model ->
            Element.paragraph []
                (renderAll model
                    children
                )
    }


fill : Element.Attribute msg
fill =
    Element.width Element.fill


headingPadding : Block.HeadingLevel -> Element.Attribute msg
headingPadding level =
    case level of
        Block.H1 ->
            Element.paddingEach { bottom = 10, left = 0, right = 0, top = 15 }

        Block.H2 ->
            Element.paddingEach { bottom = 0, left = 0, right = 0, top = 15 }

        Block.H3 ->
            Element.paddingXY 0 10

        Block.H4 ->
            Element.paddingXY 0 5

        _ ->
            Element.paddingXY 0 5


stars : Element msg
stars =
    Element.wrappedRow
        [ fill
        , Element.spacing 10
        , Element.paddingEach { top = 15, bottom = 5, left = 0, right = 0 }
        ]
        [ Element.paragraph []
            [ Element.el
                [ Font.color FlatColors.sunFlower
                , Font.size 36
                ]
                (Element.text "⭑⭑⭑⭑⭑")
            , Element.el [ Font.size 20 ] (Element.text " average, 28 ratings on ")
            , Element.link
                [ Element.htmlAttribute
                    (Html.Attributes.style "display" "inline-flex")
                ]
                { url = "https://gumroad.com/l/VvjqSj"
                , label =
                    Element.el
                        [ Font.color (Element.rgb255 7 81 219)
                        , Font.size 20
                        ]
                        (Element.text
                            "Gumroad"
                        )
                }
            ]
        ]


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


viewLoading : Element msg
viewLoading =
    Element.column [ Element.width Element.fill ]
        [ Element.el [ Element.centerX, Element.centerY ]
            (Loading.render
                Circle
                -- LoaderType
                { defaultConfig | color = "#333", size = 75, speed = 0.7 }
                -- Config
                Loading.On
                |> Element.html
            )
        ]
