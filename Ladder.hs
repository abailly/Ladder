#!/usr/bin/env stack
-- stack runhaskell --package async --package unix --

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
