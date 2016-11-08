module Main exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes
import Html.Events
import List
import Set


main : Program Never
main =
    Html.App.beginnerProgram
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


type Tree a
    = N a (List (Tree a))


initialModel : Model
initialModel =
    { tree =
        N "Object"
            [ N "Animals"
                [ N "Birds" []
                , N "Mamals"
                    [ N "Elephant" []
                    , N "Mouse" []
                    ]
                , N "Reptiles" []
                ]
            , N "Plants"
                [ N "Flowers"
                    [ N "Rose" []
                    , N "Tulip" []
                    ]
                ]
            , N "Trees" []
            ]
    , expanded = Set.empty
    }


addCounts : Tree a -> Tree ( a, Int )
addCounts (N a subtrees) =
    case subtrees of
        [] ->
            N ( a, 0 ) []

        ts ->
            let
                getOffspringCount : Tree ( a, Int ) -> Int
                getOffspringCount (N ( _, cnt ) _) =
                    cnt

                subtreesWithCount =
                    List.map addCounts ts
                        |> List.sortBy getOffspringCount
                        |> List.reverse

                offspringCount =
                    List.map (\t -> getOffspringCount t + 1) subtreesWithCount
                        |> List.sum
            in
                N ( a, offspringCount ) subtreesWithCount


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
        t' =
            addCounts tree
    in
        Html.div []
            [ Html.ul [ Html.Attributes.class "tree" ] [ viewTree expanded t' ]
            , Html.hr [] []
            , Html.div [] [ Html.text <| toString expanded ]
            ]


viewTree : Set.Set String -> Tree ( String, Int ) -> Html Msg
viewTree expanded (N ( root, offspringCount ) children) =
    let
        renderSubtree =
            Set.member root expanded

        expandOrCollapse =
            Html.Events.onClick <|
                if renderSubtree then
                    Collapse root
                else
                    Expand root

        plusOrMinus =
            if offspringCount <= 0 then
                ""
            else if renderSubtree then
                "[-] "
            else
                "[+] "

        label =
            plusOrMinus
                ++ root
                ++ if offspringCount > 0 then
                    " (" ++ toString offspringCount ++ ")"
                   else
                    ""
    in
        Html.li []
            ((Html.span [ expandOrCollapse ] [ Html.text label ])
                :: if renderSubtree then
                    viewForest expanded children
                   else
                    []
            )


viewForest : Set.Set String -> List (Tree ( String, Int )) -> List (Html Msg)
viewForest expanded children =
    if List.isEmpty children then
        []
    else
        [ Html.ul [] (List.map (viewTree expanded) children) ]
