module PieChart exposing (MouseEvent, pieChart, mouseEnter, mouseLeave)

import Visualization.Shape as Shape exposing (defaultPieConfig, Arc)
import Array exposing (Array)
import Svg exposing (Svg, svg, g, path, animateTransform)
import Svg.Attributes exposing (d, transform, style, width, height)
import Html exposing (Html, Attribute, div, h4, text)
import Html.Events as Events
import Json.Decode exposing (field, int, bool, float, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import List exposing (map)


colors : Array String
colors =
    Array.fromList
        [ "rgba(31, 119, 180, 0.5)"
        , "rgba(255, 127, 14, 0.5)"
        , "rgba(44, 159, 44, 0.5)"
        , "rgba(214, 39, 40, 0.5)"
        , "rgba(148, 103, 189, 0.5)"
        , "rgba(140, 86, 75, 0.5)"
        , "rgba(227, 119, 194, 0.5)"
        , "rgba(128, 128, 128, 0.5)"
        , "rgba(188, 189, 34, 0.5)"
        , "rgba(23, 190, 207, 0.5)"
        ]


svgPadding : Float
svgPadding =
    1


mouseEvent : String -> a -> (MouseEvent a -> msg) -> Attribute msg
mouseEvent event obj tag =
    let
        evt =
            Json.Decode.map
                tag
                (mouseEventDecoder obj)
    in
        evt
            |> Events.onWithOptions event Events.defaultOptions


mouseEnter : a -> (MouseEvent a -> msg) -> Attribute msg
mouseEnter obj tag =
    mouseEvent "mouseenter" obj tag


mouseLeave : a -> (MouseEvent a -> msg) -> Attribute msg
mouseLeave obj tag =
    mouseEvent "mouseleave" obj tag


getStroke : Maybe Int -> Int -> String
getStroke selectedIndex index =
    case selectedIndex of
        Nothing ->
            ""

        Just i ->
            if i == index then
                "; stroke: #000;"
            else
                ""


getLabel : Maybe Int -> List Element -> Html msg
getLabel selectedIndex list =
    case selectedIndex of
        Nothing ->
            div [] []

        Just i ->
            case (Array.get i (Array.fromList list)) of
                Nothing ->
                    div [] []

                Just el ->
                    div [] [ text ((toString el.label) ++ ": " ++ (toString el.value)) ]


type alias Element =
    { value : Float
    , label : String
    }


pieChart : String -> List Element -> Maybe Int -> Float -> Float -> (Int -> Attribute msg) -> (Int -> Attribute msg) -> Svg msg
pieChart title list selectedIndex radius innerRadius mouseEnterHandler mouseLeaveHandler =
    let
        makeSlice index datum =
            path
                [ d (Shape.arc { datum | innerRadius = innerRadius })
                , style ("fill:" ++ (Maybe.withDefault "#000" <| Array.get index colors) ++ getStroke selectedIndex index)
                , mouseEnterHandler index
                , mouseLeaveHandler index
                ]
                []

        pieData =
            map (\e -> e.value) list |> Shape.pie { defaultPieConfig | outerRadius = radius, padAngle = 0.02 }
    in
        div []
            [ h4 [] [ text title ]
            , svg [ width (toString (2 * (radius + svgPadding)) ++ "px"), height (toString (2 * (radius + svgPadding)) ++ "px") ]
                [ g
                    [ transform ("translate(" ++ toString (radius + svgPadding) ++ "," ++ toString (radius + svgPadding) ++ ")") ]
                    [ g [] <| List.indexedMap makeSlice pieData ]
                ]
            , getLabel selectedIndex list
            ]


type alias MouseEvent a =
    { obj : a
    , alt : Bool
    , ctrl : Bool
    , shift : Bool
    , meta : Bool
    , clientX : Float
    , clientY : Float
    , offsetX : Float
    , offsetY : Float
    , mouseButtons : Int
    }


mouseEventDecoder : a -> Decoder (MouseEvent a)
mouseEventDecoder a =
    decode MouseEvent
        |> hardcoded a
        |> required "altKey" bool
        |> required "ctrlKey" bool
        |> required "shiftKey" bool
        |> required "metaKey" bool
        |> required "clientX" float
        |> required "clientY" float
        |> required "offsetX" float
        |> required "offsetY" float
        |> required "buttons" int
