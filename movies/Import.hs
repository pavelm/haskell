import System.IO
import Data.String.Utils
import Data.List
import Text.ParserCombinators.Parsec

main = do
    rawLines <- getLines 
    let actorsStart = readUntil "THE ACTORS LIST" rawLines
    return $ process actorsStart


process = map (parseLine "") . filter (not . null) {-. take 10 -}


getLines :: IO [String]
getLines = do
    inh <- openFile "actors.list" ReadMode
    inpStr <- hGetContents inh
    let rawLines = lines inpStr
    return rawLines

readUntil :: String -> [String] -> [String]
readUntil target (h:tail) = 
    if (startswith target h) then 
        drop 4 tail
    else 
        readUntil target tail
readUntil _ [] = []

parseLine :: String -> String -> (String, Entry)
parseLine prevActor line = 
    let actor:movieline = split "\t" line in
        (actor,createEntry $ strip $ head $ filter (not . null) movieline)


data WorkType = Movie | Series | MiniSeries 
    deriving Show 

data Entry = Entry {
    entryType :: WorkType,
    entryTitle   :: String,
    entryYear :: Int,
    entryCharacter :: String
} deriving Show

indexOf :: Eq a => [a] -> [a] -> Int 
indexOf search items = loop 0 items
    where   loop counter [] = -1
            loop counter items@(x:xs) =     
                if startswith search items then
                    counter
                else 
                    loop (counter+1) xs

substring start length = (take length) . (drop start)

findBetween startToken endToken = takeWhile (/= endToken ) . drop 1 . (dropWhile (/= startToken))

createEntry :: String -> Entry
createEntry title = Entry { 
        entryType = eType,
        entryTitle = eTitle,
        entryYear = year,
        entryCharacter = character
    }
    where isSeries = startswith "\"" title
          isMini = "mini" `isInfixOf` title 
          eTitle 
            | isSeries = findBetween '\"' '\"' title
            | otherwise = strip $ takeWhile (/= '(') title
          eType
            | isSeries && isMini = MiniSeries
            | isSeries = Series
            | otherwise = Movie
          year = read $ findBetween '(' ')' title
          character = findBetween '[' ']' title
