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
    ns = filter (not . (`elem` fmap fst t) . fst) $ neighbors ws w
    t' = t ++ ns
