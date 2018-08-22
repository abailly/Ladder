#!/usr/bin/env stack
-- stack runhaskell --package async --package unix --

import           Data.List          ((\\))
import           System.Environment

main = getArgs >>= checkArgs >>= readWords >>= printLadder
    where
    checkArgs args | length args == 3 = return args
    checkArgs args | otherwise       = error "usage: ladder <wordlistfile> <start> <end>"

    readWords [f,s,t] = readFile f >>= \cs -> return ( filter (\w -> length w == length s) $ words cs, s, t)

    printLadder (ws,s,t) = putStrLn $ unwords $ ladder ws s t

ladder :: [ String ] -> String -> String -> [ String ]
ladder = undefined

-- | Neighbor
-- >>> neighbor "cat" "dog"
-- False
--
-- >>> neighbor "cat" "bat"
-- True
--
-- >>> neighbor "cat" "cot"
-- True
--
-- >>> neighbor "cat" "cob"
-- False
--
-- >>> neighbor "cat" "cab"
-- True
--
-- >>> neighbor "dog" "do"
-- False
--
-- >>> neighbor "at" "cat"
-- False
--
neighbor :: String -> String -> Bool
neighbor v w = length (filter not $ zipWith (==) v w) == 1

type Neighbors = (String,String)

-- |
--
-- >>> let ws = words "bag bat bog dog fog"
-- >>> neighbors ws "fog"
-- [("bog","fog"),("dog","fog")]
--
neighbors :: [String] -> String -> [Neighbors]
neighbors words w = filter (uncurry neighbor) $ zip words (repeat w)


-- |
--
-- >>> let ns = [("cat","bat"),("bat","bag"),("bag","bog"),("bog","dog"),("dog","***")]
-- >>> path "cat" ns
-- ["cat","bat","bag","bog","dog"]
--
-- >>> path "foo" ns
-- []
--
path ::  String -> [Neighbors] -> [String]
path w ns = reverse $ link w []
  where
    link :: String -> [String] -> [String]
    link v p = case lookup v ns of
                    Nothing    -> p
                    Just "***" -> v:p
                    Just v'    -> link v' (v:p)


type Tree = [Neighbors]
type State =([String],Tree)

-- | Explore
-- >>> let ws = words "bag bat bog cat cog dog fog"
-- >>> explore ws "fog" [("fog","")]
-- (["bog","cog","dog"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog")])
--
-- >>> explore ws "bag" [("fog",""),("bog","fog"),("cog","fog"),("dog","fog")]
-- (["bat"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bat","bag")])
--
-- >>> explore ws "fog" [("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bat","bag")]
-- ([],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bat","bag")])
--
explore :: [String] -> String -> Tree -> State
explore ws w t = (fmap fst ns, t')
  where
    ns = filter (notInNeighbors t . fst) $ neighbors ws w
    t' = t ++ ns

notInNeighbors t = not . (`elem` fmap fst t)

-- | BFS
--
-- >>> let ws = words "bag bat bog cat cog dog fog"
--
-- >>> breadthSearch ws (["fog"],[("fog","")])
-- (["bog","cog","dog"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog")])
--
-- >>> breadthSearch ws (["bog","cog","dog"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog")])
-- (["cog","dog","bag"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bag","bog")])
--
-- >>> breadthSearch ws (["cog","dog","bag"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bag","bog")])
-- (["dog","bag"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bag","bog")])
--
-- >>> breadthSearch ws (["dog","bag"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bag","bog")])
-- (["bag"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bag","bog")])
--
-- >>> breadthSearch ws (["bag"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bag","bog")])
-- (["bat"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bag","bog"),("bat","bag")])
--
-- >>> breadthSearch ws (["bat"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bag","bog"),("bat","bag")])
-- (["cat"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bag","bog"),("bat","bag"),("cat","bat")])
--
-- >>> breadthSearch ws (["cat"],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bag","bog"),("bat","bag"),("cat","bat")])
-- ([],[("fog",""),("bog","fog"),("cog","fog"),("dog","fog"),("bag","bog"),("bat","bag"),("cat","bat")])
breadthSearch :: [String] -> State -> State
breadthSearch ws (w:vs, tree) = (vs ++ vs', tree')
  where
    (vs',tree') = explore ws w tree
