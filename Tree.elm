module Tree exposing (Tree(Node))


type Tree a
    = Node a (List (Tree a))
