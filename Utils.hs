module Utils (maybeToEither, splitAtMaybe, listToChunksMaybe, listToChunks, transformLeft, getImmediatelyAvailableLines)
where

import Data.Maybe
import System.IO

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither _ (Just right) = Right right
maybeToEither left Nothing = Left left

splitAtMaybe :: Int -> [a] -> Maybe ([a], [a])
splitAtMaybe i l =
 if length l >= i && i >= 0
 then Just $ splitAt i l
 else Nothing

listToChunksMaybe :: Int -> [a] -> Maybe [[a]]
listToChunksMaybe i l@(_:_) =
 do (chunk, rest) <- splitAtMaybe i l
    otherChunks <- listToChunksMaybe i rest
    return $ chunk : otherChunks
listToChunksMaybe _ _ = Just []

listToChunks :: Int -> [a] -> [[a]]
listToChunks i l = fromJust $ listToChunksMaybe i l

transformLeft :: (a -> a) -> Either a b -> Either a b
transformLeft f (Left l) = Left $ f l
transformLeft _ r = r

getImmediatelyAvailableLines :: IO [String]
getImmediatelyAvailableLines = hGetImmediatelyAvailableLines stdin

hGetImmediatelyAvailableLines :: Handle -> IO [String]
hGetImmediatelyAvailableLines h =
 do canRead <- hWaitForInput h 0
    if canRead
    then do l <- hGetLine h
            furtherLines <- hGetImmediatelyAvailableLines h
            return $ l : furtherLines
    else return []
