module Main exposing (..)

import Html exposing (Html)
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Style.Border as Border
import Style.Shadow as Shadow
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Color exposing (..)
import Style.Scale as Scale
import List 
import Dict exposing (Dict)
import String
import Maybe exposing (withDefault)
import Set exposing (Set)
import Char

scale = Scale.modular 16 1.618

setToggle : comparable -> Set comparable -> Set comparable
setToggle el set = 
    if Set.member el set then Set.remove el set
    else Set.insert el set

listShift : Int -> List a -> List a
listShift n lst = 
    case (n, lst) of
        (0, _) -> lst
        (n, h::t)  -> listShift (n-1) (t ++ [h])
        (_, []) -> []

notesInOrder = String.split "," "C,C#,D,D#,E,F,F#,G,G#,A,A#,B"

intervalNames = Dict.fromList [ ( 0, "R")
                              , ( 1, "b2")
                              , ( 2, "2") 
                              , ( 3, "b3")
                              , ( 4, "3") 
                              , ( 5, "4") 
                              , ( 6, "b5")
                              , ( 7, "5") 
                              , ( 8, "#5")
                              , ( 9, "6") 
                              , (10, "b7")
                              , (11, "7") 
                              , (13, "b9") 
                              , (14, "9") 
                              , (15, "#9") 
                              , (16, "b11") 
                              , (17, "11") 
                              , (18, "#11") 
                              , (19, "b13") 
                              , (20, "13") 
                              , (21, "#13") 
                             ] 

intervalColors = Dict.fromList [ ( 0, Color.black) -- "R"
                               , ( 1, Color.lightOrange) -- "m2"
                               , ( 2, Color.orange) -- "2"
                               , ( 3, Color.lightBlue) -- "m3"
                               , ( 4, Color.blue) -- "3"
                               , ( 5, Color.purple) -- "4"
                               , ( 6, Color.lightGreen) -- "b5"
                               , ( 7, Color.green) -- "5"
                               , ( 8, Color.darkGreen) -- "#5"
                               , ( 9, Color.yellow) -- "6"
                               , (10, Color.lightRed) -- "b7"
                               , (11, Color.red)  -- "7"
                               , (13, Color.lightOrange)  -- "b9"
                               , (14, Color.orange)  -- "9"
                               , (15, Color.darkOrange)  -- "#9"
                               , (16, Color.lightPurple)  -- "b11"
                               , (17, Color.purple)  -- "11"
                               , (18, Color.darkPurple)  -- "#11"
                               , (19, Color.lightBrown)  -- "b13"
                               , (20, Color.brown)  -- "13"
                               , (21, Color.darkBrown)  -- "#13" 
                               ] 

-- Interval in semitones between two notes, if valid names are provided.
intervalBetween : String -> String -> Maybe Int
intervalBetween root other =
    let
        notes = notesInOrder |> List.indexedMap (\i e -> (e,i)) |> Dict.fromList
    in
        case (Dict.get root notes, Dict.get other notes) of
            (Just i, Just j) -> 
                if j >= i then Just(j-i)
                else Just(j-i+12)
            _ -> Nothing

-- Note (name) obtained by going interval semitones up from root.
intervalToNote : String -> Int -> String
intervalToNote root interval =
    let
        noteToIndex = notesInOrder |> List.indexedMap (\i e -> (e,i)) |> Dict.fromList
        indexToNote = notesInOrder |> List.indexedMap (,) |> Dict.fromList
    in
    case Dict.get root noteToIndex of
        Just i -> withDefault "?" (Dict.get ((i + interval) % 12) indexToNote)
        _ -> "?"
            

type MyStyles
    = Default
    | Headline
    | Button
    | Title
    | Colored Int Bool
    | ColoredLabel 
    | Test Int

type Variations = Selected | Highlight

defaultType = [Font.font "Helvetica"]
darkStroke = Color.darkGray

noteCircleStyles selected = 
    let
        makeStyle interval =
            let 
                intervalColor = Dict.get interval intervalColors |> withDefault Color.black
                backgroundColor = if selected then intervalColor else Color.white
                textColor = if selected then Color.white else darkStroke
            in
            style 
                (Colored interval selected) 
                [ Color.background backgroundColor
                , Color.border darkStroke
                , Color.text textColor
                , Border.all 1
                , Font.size (scale 2) 
                , Border.rounded 20
                , Style.cursor "pointer"
                , variation Highlight [Shadow.glow darkStroke 3] 
                ]
    in
    List.map makeStyle (Dict.keys intervalColors)

stylesheet =
    Style.styleSheet (
        [ style Default
            [ Color.text darkStroke
            , Color.background white
            , Font.size (scale 2) 
            , Font.typeface defaultType
            ]
        , style Headline 
            [ Font.size (scale 4)
            , Color.text black
            ]
        , style Button
            [ Color.text darkStroke
            , Color.background white
            , Color.border darkStroke
            , Font.size (scale 2) 
            , Font.typeface defaultType
            , Style.cursor "pointer"
            , Border.rounded 20
            , variation Selected [ Color.text white, Color.background black ]
            ]
        , style ColoredLabel 
            [ Color.text darkStroke
            , Font.size (scale 1) 
            , Style.cursor "pointer"
            , variation Selected [Color.text white]
            ]
        ] 
        ++ (noteCircleStyles True) 
        ++ (noteCircleStyles False) )

-- Display interval or note name in note circles.
type NoteLabels
    = Interval
    | Note

type alias Model = 
    { highlight : Int 
    , selection : Set Int 
    , rootNote : String 
    , noteLabels : NoteLabels
    }

init : ( Model, Cmd Msg )
init = ( 
    { highlight = -1 
    , selection = Set.fromList [0] 
    , rootNote = "E" 
    , noteLabels = Interval
    }, 
    Cmd.none )

type Msg
    = IntervalHighlightOn Int
    | IntervalHighlightOff
    | ToggleIntervalSelection Int
    | SetRoot String
    | ToggleNoteLabelType
    | ClearIntervalSelection

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IntervalHighlightOn interval -> 
            { model | highlight = interval} ! []

        IntervalHighlightOff -> 
            { model | highlight = -1 } ! []

        ToggleIntervalSelection iv -> 
            { model | selection = setToggle iv model.selection } ! []
        
        SetRoot root -> 
            { model | rootNote = root } ! []

        ToggleNoteLabelType -> 
            { model | noteLabels = if model.noteLabels == Note then Interval else Note } ! []

        ClearIntervalSelection -> 
            { model | selection = Set.fromList [0] } ! []



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view model =
    let
        noteCircle size noteName = 
            let 
                minInterval = intervalBetween model.rootNote noteName |> withDefault -100 
                interval = if Set.member (minInterval + 12) model.selection then minInterval + 12 else minInterval 
                isSelected = Set.member interval model.selection || Set.member (interval + 12) model.selection
                label = case model.noteLabels of
                    Note -> noteName
                    Interval -> withDefault "?" (Dict.get interval intervalNames)
            in
            circle size 
                (Colored interval isSelected)
                [ onMouseOver (IntervalHighlightOn interval)
                , onMouseOut IntervalHighlightOff
                , onClick (ToggleIntervalSelection interval)
                , vary Highlight (model.highlight==interval || model.highlight==interval + 12) ] 
                (el ColoredLabel [center, verticalCenter, vary Selected isSelected] (text label))
        
        guitarString rootNoteShift =
            let
                circles = notesInOrder |> listShift rootNoteShift |> List.map (noteCircle 25)
            in
            row Default [padding 0, spacingXY 50 0, center, verticalCenter] circles
        
        rootNoteShifts = [0, 5, 10, 15, 19, 24] |> List.map (\n -> n + 4) |> List.reverse

        rootNoteButton rootNote = 
            el Button 
                [spacing 20, padding (scale 1), onClick (SetRoot rootNote), vary Selected (rootNote==model.rootNote)] 
                (text rootNote)

        rootNoteRow = 
            row Default 
                [padding 50, spacing 20, center, verticalCenter] 
                ((el Default [] (text "Root note is ")) :: (List.map rootNoteButton notesInOrder))

        intervalButton interval = 
            el (Colored interval (Set.member interval model.selection)) 
                [ spacing 20, padding (scale 1)
                , onClick (ToggleIntervalSelection interval)
                , onMouseOver (IntervalHighlightOn interval)
                , onMouseOut IntervalHighlightOff 
                , center
                , verticalCenter] 
                (text <| withDefault "?" <| Dict.get interval intervalNames)

        clearIntervalButton = 
            el (Colored 0 False) 
                [spacing 20, padding (scale 1), onClick ClearIntervalSelection] 
                (text "clear")

        intervalButtonRow1 =
            let ivls = intervalNames |> Dict.keys |> List.filter (\iv -> iv <= 12)
            in
            row Default 
                [padding (scale 1), spacing 20, center, verticalCenter] 
                (List.map intervalButton ivls)

        intervalButtonRow2 =
            let ivls = intervalNames |> Dict.keys |> List.filter (\iv -> iv > 12)
            in
            row Default 
                [padding (scale 1), spacing 20, center, verticalCenter] 
                ((List.map intervalButton ivls) ++ [clearIntervalButton])

        noteDisplayMode = 
            let
                label = case model.noteLabels of
                    Interval -> "show notes"
                    Note -> "show intervals"
            in
            row Default [padding (scale 1), spacing 20, center, verticalCenter] [
                el Button [spacing 20, padding (scale 1), onClick ToggleNoteLabelType] (text label)
            ]

        intervalButtonCells1 =
            let 
                intervals = intervalNames |> Dict.keys |> List.filter (\iv -> iv <= 12)
                buttons = List.map intervalButton intervals
            in
            buttons 
                |> List.indexedMap 
                    (\col btn -> 
                        cell 
                            { start = (col, 1)
                            , width = 1, height = 1
                            , content = row Default [center, padding 5] [btn] }
                    )

        intervalButtonCells2 =
            let 
                intervals = intervalNames |> Dict.keys |> List.filter (\iv -> iv > 12)
                buttons = (empty :: (List.map intervalButton intervals)) ++ [empty, empty, clearIntervalButton]
            in
            buttons 
                |> List.indexedMap 
                    (\col btn -> 
                        cell 
                            { start = (col, 2)
                            , width = 1 , height = 1
                            , content = row Default [center, padding 5] [btn] }
                    )


        intervalButtonGrid = 
            grid Default [center, verticalCenter]
                { columns = []-- List.repeat 13 (px 100) -- px 100, px 100, px 100, px 100
                , rows =
                    [ 
                    ]
                , cells = intervalButtonCells1 ++ intervalButtonCells2
                }

    in
            
    Element.layout stylesheet <| row Default [center, verticalCenter, width fill, height fill] [
            column Default [padding (scale 1), spacing 5, center, verticalCenter , width fill, height fill ] (
                [el Headline [padding (scale 2), center] (text <| String.fromChar <| Char.fromCode  <| 0x266C)] ++ 
                (List.map guitarString rootNoteShifts) ++ 
                [ rootNoteRow
                , intervalButtonGrid
                --, intervalButtonRow1
                --, intervalButtonRow2
                , noteDisplayMode]
            )
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
