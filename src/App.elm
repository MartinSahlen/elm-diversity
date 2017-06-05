module App exposing (..)

import PieChart exposing (..)
import Html exposing (Html, Attribute, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Mouse exposing (..)
import List exposing (filter, map, length, sort)
import List.Extra exposing (unique)


--var initialData =


allEmployees : List Employee
allEmployees =
    [ Employee 1 "Male" "Oslo" "Biracial" "25-34" Tech
    , Employee 2 "Male" "Oslo" "White" "25-34" Tech
    , Employee 3 "Female" "New York" "Hispanic/Latino" "25-34" Sales
    , Employee 4 "Male" "Oslo" "White" "25-34" Tech
    , Employee 5 "Male" "Oslo" "White" "25-34" Tech
    , Employee 6 "Male" "Oslo" "White" "25-34" Sales
    , Employee 7 "Male" "Oslo" "White" "25-34" Tech
    , Employee 8 "Female" "Oslo" "White" "25-34" Sales
    , Employee 9 "Male" "Oslo" "White" "35-44" Tech
    , Employee 10 "Male" "Oslo" "White" "35-44" Leadership
    , Employee 11 "Male" "Oslo" "White" "35-44" Tech
    , Employee 12 "Male" "Oslo" "White" "35-44" Leadership
    , Employee 13 "Male" "New York" "White" "25-34" Tech
    , Employee 14 "Male" "New York" "White" "25-34" Leadership
    , Employee 15 "Female" "New York" "White" "25-34" Sales
    , Employee 16 "Male" "New York" "White" "35-44" Leadership
    , Employee 17 "Female" "Oslo" "White" "25-34" Leadership
    , Employee 18 "Female" "New York" "White" "35-44" Sales
    ]


type alias EventData =
    { id : Int
    }


type FilterType
    = All
    | Tech
    | Sales
    | Leadership


type alias Employee =
    { id : Int
    , gender : String
    , location : String
    , ethnicity : String
    , ageRange : String
    , focus : FilterType
    }


type Msg
    = MouseEnter (MouseEvent (Maybe EventData))
    | MouseLeave (MouseEvent (Maybe EventData))
    | Position Int Int
    | Filter FilterType


enterHandler : Int -> Attribute Msg
enterHandler index =
    (mouseEnter (Just (EventData index))) MouseEnter


leaveHandler : Int -> Attribute Msg
leaveHandler index =
    (mouseLeave (Just (EventData index))) MouseLeave


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick (Filter All) ] [ text "All" ]
            , button [ onClick (Filter Tech) ] [ text "Tech" ]
            , button [ onClick (Filter Sales) ] [ text "Sales/Marketing" ]
            , button [ onClick (Filter Leadership) ] [ text "Leadership" ]
            ]
        , div []
            [ div [ style [ ( "display", "inline-block" ) ] ] [ pieChart "Gender" (groupEmployeesByGender model.employees) model.selectedObjectIndex 150 80 enterHandler leaveHandler ]
            ]
        ]


type alias Model =
    { list : List { value : Float, label : String }
    , selectedObjectIndex : Maybe Int
    , pointer : Maybe ( Int, Int )
    , currentFilter : FilterType
    , employees : List Employee
    }


groupEmployeesByGender : List Employee -> List { value : Float, label : String }
groupEmployeesByGender list =
    let
        labels =
            unique (map (\e -> e.gender) list)
    in
        map
            (\l -> ({ value = (toFloat (length (filter (\e -> e.gender == l) list))), label = l }))
            labels


model : Model
model =
    Model
        [ { value = 1, label = "First" }
        , { value = 1, label = "Second" }
        ]
        Nothing
        Nothing
        All
        allEmployees


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseEnter evt ->
            case evt.obj of
                Nothing ->
                    ( { model | selectedObjectIndex = Nothing }, Cmd.none )

                Just data ->
                    ( { model | selectedObjectIndex = Just data.id }, Cmd.none )

        MouseLeave evt ->
            case evt.obj of
                Nothing ->
                    ( { model | selectedObjectIndex = Nothing }, Cmd.none )

                Just data ->
                    ( { model | selectedObjectIndex = Nothing }, Cmd.none )

        Filter f ->
            case f of
                All ->
                    ( { model | currentFilter = f, employees = allEmployees }, Cmd.none )

                _ ->
                    ( { model | currentFilter = f, employees = (filter (\e -> e.focus == f) allEmployees) }, Cmd.none )

        Position x y ->
            case model.selectedObjectIndex of
                Nothing ->
                    ( { model | pointer = Just ( x, y ) }, Cmd.none )

                Just index ->
                    ( { model | pointer = Just ( x, y ) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    --Mouse.moves (\{ x, y } -> Position x y)
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }
