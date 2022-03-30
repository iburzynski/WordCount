{-# LANGUAGE  OverloadedStrings #-}

module Lib where

import Data.Char
import Data.List
import Data.Text (Text)
import Fmt
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import Text.Read
import Control.Applicative (liftA2)

data Entry = Entry { runWord  :: T.Text
                   , runCount :: Int
                   }

instance Show Entry where
  show (Entry w c) = fmt "" +| w |+ " (" +| c |+ ")"

instance Buildable Entry where
  build = build . show

type Vocabulary = [Entry]
type StopWords = [Text]

extractVocab :: StopWords -> Text -> Vocabulary
extractVocab sws t  = sortVocab . makeEntries . foldr clean [] $ T.words t
  where
    clean :: Text -> [Text] -> [Text]
    clean w ws = let w' = T.toCaseFold . T.replace "’s" "" $ T.dropAround (not . isLetter) w
                 in if not (T.null w') && T.filter isLetter w' `notElem` sws
                    then w':ws
                    else ws

    makeEntries :: [Text] -> [Entry]
    makeEntries ws = map (\ws@(w:_) -> Entry w (length ws)) . group $ sort ws

    sortVocab :: Vocabulary -> Vocabulary
    sortVocab = sortBy (\e e' -> compare (runCount e') (runCount e))

stopWords :: String -> IO StopWords
stopWords s = fmap (ws ++) (T.words <$> TIO.readFile "assets/stop-words.txt")
  where
    ws = T.filter isLetter <$> T.words (T.pack s)

processTextFile :: FilePath -> Int -> String -> IO ()
processTextFile fname n sws = do
  text <- TIO.readFile fname
  sws' <- stopWords sws
  let v = take n $ extractVocab sws' text
  fmt $ nameF (fmt "Top " +| n |+ " Words in " +| fname |+ "") $ unlinesF v

requestFile :: IO String
requestFile = do
  putStrLn "Enter file name: "
  fname <- getLine
  validateFile fname

validateFile :: FilePath -> IO FilePath
validateFile fname = do
  exists <- doesFileExist fname
  if exists then pure fname
  else putStrLn "Error: File doesn't exist!" *> requestFile

requestCount :: IO Int
requestCount = do
  putStrLn "Show how many top words? "
  count <- getLine
  validateCount count

validateCount :: String -> IO Int
validateCount c = do
  let mc = readMaybe c :: Maybe Int
  case mc of
    Just c' -> pure c'
    Nothing -> putStrLn "Error: Please enter a number!" *> requestCount
