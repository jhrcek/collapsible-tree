module Tree exposing (Tree(N), treeDecoder)

import Json.Decode exposing (..)


type Tree a
    = N a (List (Tree a))


treeDecoder : Decoder (Tree String)
treeDecoder =
    map2 N
        (index 0 string)
        (index 1 (list (lazy (\_ -> treeDecoder))))
