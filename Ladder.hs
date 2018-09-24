#!/usr/bin/env stack
-- stack runhaskell --


import           Data.List          ((\\))
import           Data.Maybe
import           System.Environment

main = getArgs >>= checkArgs >>= readWords >>= printLadder
  where
    checkArgs args | length args == 3 = return args
                   | otherwise        = error "usage: ladder <wordlistfile> <start> <end>"

    readWords [f,s,t] = readFile f >>= \cs -> return ( filter (\w -> length w == length s) $ lines cs, s, t)

    printLadder (ws,s,t) = putStrLn $ unwords $ ladder ws s t

ladder :: [String] -> String -> String -> [ String ]
ladder = undefined

-- |
--
-- >>> neighbor "cat" "dog"
-- False
-- >>> neighbor "cat" "bat"
-- True
-- >>> neighbor "cat" "cot"
-- True
-- >>> neighbor "cat" "cob"
-- False
-- >>> neighbor "cat" "cab"
-- True
-- >>> neighbor "dog" "do"
-- False
-- >>> neighbor "dog" "du"
-- False
-- >>> neighbor "at" "cat"
-- False
neighbor :: String -> String -> Bool
neighbor (x:xs) (y:ys)
  | x == y    = neighbor xs ys
  | otherwise = xs == ys
neighbor _ _ = False

type Dictionary = [String]

-- |
-- >>> let ws = words "bag bat bog dog fog"
-- >>> neighbors "fog" ws
-- ["bog","dog"]
--
-- >>> neighbors "bog" ws
-- ["bag","dog","fog"]
neighbors :: String -> Dictionary -> [String]
neighbors w = filter (neighbor w)

type Neighbors = (String,String)

-- |
-- >>> let ws = words "bag bat bog dog fog"
--
-- >>> neighborsTo "fog" ws
-- [("bog","fog"),("dog","fog")]
--
-- >>> neighborsTo "bog" ws
-- [("bag","bog"),("dog","bog"),("fog","bog")]
neighborsTo :: String -> Dictionary -> [Neighbors]
neighborsTo w ws = zip (neighbors w ws) (repeat w)

-- |
-- >>> let ns = [("cat","bat"),("bat","bag"),("bag","bog"),("bog","dog"),("dog","***")]
-- >>> path "cat" ns
-- ["cat","bat","bag","bog","dog"]
-- >>> path "foo" ns
-- []
path :: String -> Tree -> [String]
path w ns = case lookupEdge w ns of
  Just found -> w : path found ns
  Nothing    -> []


type Tree = [Neighbors]

-- |
--
-- >>> initialTree "dog"
-- [("dog","")]
initialTree :: String -> Tree
initialTree w = [(w,"")]


-- |
-- >>> insertEdge "fog" "dog" [("dog","")]
-- [("fog","dog"),("dog","")]
-- >>> insertEdge "fog" "dog" [("fog","dog")]
-- [("fog","dog")]
-- >>> insertEdge "bog" "dog" [("fog","dog"),("dog","")]
-- [("bog","dog"),("fog","dog"),("dog","")]
--
-- Prevents construction of a graph:
-- >>> insertEdge "fog" "fig" [("fog","dog"),("dog","")]
-- [("fog","dog"),("dog","")]
--
-- Prevents construction of a forest
-- >>> insertEdge "bog" "bag" [("fog","dog"),("dog","")]
-- [("fog","dog"),("dog","")]
insertEdge :: String -> String -> Tree -> Tree
insertEdge n w t = case lookup n t of
  Just _  -> t
  Nothing -> case lookup w t of
    Just _  -> (n,w) : t
    Nothing -> t

lookupEdge :: String -> Tree -> Maybe String
lookupEdge = lookup


type State =([String],Tree)

-- |
-- >>> let ws = words "bag bat bog cat cog dog fog"
-- >>> let (vs,t) = explore ws "fog" $ initialTree "fog"
-- >>> vs
-- ["bog","cog","dog"]
-- >>> lookupEdge "fog" t
-- Just ""
-- >>> lookupEdge "bog" t
-- Just "fog"
-- >>> lookupEdge "cog" t
-- Just "fog"
-- >>> lookupEdge "dog" t
-- Just "fog"
--
-- >>> let (vt,u) = explore ws "bog" t
-- >>> lookupEdge "bag" u
-- Just "bog"
--
-- >>> fst $ explore ws "fog" u
-- []
explore :: Dictionary -> String -> Tree -> State
explore dict word tree =
  let nextWords = neighborsToFromNotIn :: [ String ]
      newTree = addEdgesFromTo nextWords :: Tree
      neighborsToFromNotIn = filter notInTree (fmap fst (neighborsTo word dict))
      notInTree w = w `notElem` wordsInTree
      wordsInTree = fmap fst tree
      addEdgesFromTo = foldr (flip insertEdge word) tree
  in  (nextWords, newTree)
