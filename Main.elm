module Main exposing (..)

import Html exposing (..)
import Html.Attributes
import Html.Events
import List
import Tree exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = viewTree
        , update = update
        }


type alias Model =
    CollapsibleTree String


type Msg
    = Expand NodeId
    | Collapse NodeId


initialModel : Model
initialModel =
    makeTree <|
        Node "Mathematics"
            [ Node "Foundations"
                [ Node "Mathematical logic" []
                ]
            , Node "Discrete"
                [ Node "Combinatorics" []
                , Node "Graph Theory" []
                , Node "Algebra"
                    [ Node "Group theory" []
                    ]
                ]
            , Node "Continuous"
                [ Node "Calculus" []
                ]
            , Node "Applied mathematics" []
            ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Expand nodeId ->
            toggleNode nodeId True model

        Collapse nodeId ->
            toggleNode nodeId False model


viewTree : CollapsibleTree String -> Html Msg
viewTree (Node root children) =
    let
        ( nodeData, expanded, nodeId ) =
            root

        rootView =
            Html.span [ expandOrCollapse ] [ Html.text rootText ]

        expandOrCollapse =
            Html.Events.onClick <|
                if expanded then
                    Collapse nodeId
                else
                    Expand nodeId

        rootText =
            plusOrMinus ++ nodeData

        childrenListView =
            if expanded then
                viewForest children
            else
                []

        plusOrMinus =
            if List.isEmpty children then
                ""
            else if expanded then
                "▾ "
            else
                "▸ "
    in
        Html.ul [ noBullets ] (rootView :: childrenListView)


viewForest : List (CollapsibleTree String) -> List (Html Msg)
viewForest children =
    List.map (\childTree -> Html.li [] [ viewTree childTree ]) children


noBullets : Attribute Msg
noBullets =
    Html.Attributes.style [ ( "list-style-type", "none" ) ]
