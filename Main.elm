module Main exposing (..)

import Html exposing (..)
import Html.Attributes
import Html.Events
import List
import Set
import Tree exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


type alias Model =
    { tree : Tree String
    , expanded : Set.Set String
    }


type Msg
    = Expand String
    | Collapse String


initialModel : Model
initialModel =
    { tree =
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
    , expanded = Set.empty
    }


addCounts : Tree a -> Tree ( a, Int )
addCounts (Node a subtrees) =
    case subtrees of
        [] ->
            Node ( a, 0 ) []

        ts ->
            let
                getOffspringCount : Tree ( a, Int ) -> Int
                getOffspringCount (Node ( _, cnt ) _) =
                    cnt

                subtreesWithCount =
                    List.map addCounts ts
                        |> List.sortBy getOffspringCount
                        |> List.reverse

                offspringCount =
                    List.map (\t -> getOffspringCount t + 1) subtreesWithCount
                        |> List.sum
            in
                Node ( a, offspringCount ) subtreesWithCount


update : Msg -> Model -> Model
update msg model =
    case msg of
        Expand node ->
            { model | expanded = Set.insert node model.expanded }

        Collapse node ->
            { model | expanded = Set.remove node model.expanded }


view : Model -> Html Msg
view { tree, expanded } =
    let
        trWithCounts =
            addCounts tree
    in
        Html.div []
            [ Html.ul [ Html.Attributes.class "tree" ] [ viewTree expanded trWithCounts ]
            , Html.hr [] []
            , viewExpanded expanded
            ]


viewExpanded : Set.Set String -> Html Msg
viewExpanded expanded =
    Html.div [] [ Html.text <| "Expanded nodes: " ++ toString (Set.toList expanded) ]


viewTree : Set.Set String -> Tree ( String, Int ) -> Html Msg
viewTree expanded (Node ( root, offspringCount ) children) =
    let
        renderSubtree =
            Set.member root expanded

        rootView =
            Html.span [ expandOrCollapse ] [ Html.text rootText ]

        expandOrCollapse =
            Html.Events.onClick <|
                if renderSubtree then
                    Collapse root
                else
                    Expand root

        rootText =
            plusOrMinus
                ++ root
                ++ if offspringCount > 0 then
                    " (" ++ toString offspringCount ++ ")"
                   else
                    ""

        childrenListView =
            if renderSubtree then
                viewForest expanded children
            else
                []

        plusOrMinus =
            if offspringCount <= 0 then
                ""
            else if renderSubtree then
                "⊟ "
            else
                "⊞ "
    in
        Html.li [] (rootView :: childrenListView)


viewForest : Set.Set String -> List (Tree ( String, Int )) -> List (Html Msg)
viewForest expanded children =
    if List.isEmpty children then
        []
    else
        [ Html.ul [] (List.map (viewTree expanded) children) ]
