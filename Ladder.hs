#!/usr/bin/env stack
-- stack runhaskell --

-- Retro
-- * les tuples c'est pas forcement top comme encodage
-- * c'est alambiqué...
-- * nommage des variables avec 1/2 lettres ?
-- * difficulté avec l'exposition -> on part la big picture. on lutte avec
--   la présentation autant qu'avec le sujet
-- * on suit un chemin précis, on a l'impression que le kata a été codé
--    pas à pas dans le REPL
-- * on a pas l'intuition du résultat final, on suit une recette de cuisine
-- * manque hypothèses explicites sur les structures (on construit pas une forêt mais un arbre)
-- * globalement ça manque de types...  les types aident à appuyer le raisonnement
-- * ça marche à la fin et c'est cohérent
-- * le sujet est intéressant

import           Data.List          (sort, (\\))
import           Data.Maybe
import           Data.Monoid
import           System.Environment


main = getArgs >>= checkArgs >>= readWords >>= printLadder
  where
    checkArgs args | length args == 3 = return args
                   | otherwise        = error "usage: ladder <wordlistfile> <start> <end>"

    readWords [f,s,t] = readFile f >>= \cs -> return ( filter (\w -> length w == length s) $ lines cs, s, t)

    printLadder (ws,s,t) = putStrLn $ unwords $ ladder ws s t

ladder :: [String] -> String -> String -> [ String ]
ladder dict source target =
  let (ws, tree) = breadthSearch dict target $ initialState source
  in case ws of
    [] -> []
    _  -> reverse $ path target tree

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
      neighborsToFromNotIn = filter notInTree (fmap fst (neighborsTo word dict))
      notInTree w = w `notElem` wordsInTree

      newTree = addEdgesFromTo nextWords :: Tree
      addEdgesFromTo = foldr (flip insertEdge word) tree
      wordsInTree = fmap fst tree

  in  (nextWords, newTree)


initialState :: String -> State
initialState w = ([w],[(w, "")])

-- |
-- >>> let ws = words "dog fog fig"
-- >>> let search = breadthSearch ws "dog"
-- >>> fst $ search $ initialState "fog"
-- ["dog","fig"]
--
-- >>> fst $ search $ search $ initialState "fog"
-- ["dog","fig"]
--
-- >>> fst $ search $ search $ search $ initialState "fog"
-- ["dog","fig"]
--
-- >>> let ws' = words "bog dog fog fig bag bat cat"
-- >>> let (vs,t) = breadthSearch ws' "dog" $ initialState "fig"
--
-- >>> unwords $ sort $ map fst t
-- "bag bog dog fig fog"
-- >>> unwords $ path "dog" t
-- "dog fog fig"
breadthSearch :: Dictionary -> String -> State -> State
breadthSearch dict stopWord st@ (w:ws,tree)
 | w == stopWord = st
 | otherwise = let (newWords, newTree) = explore dict w tree
               in breadthSearch dict stopWord (ws <> newWords, newTree)
breadthSearch _dict _ st = st
