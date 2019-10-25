{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.List
import           Data.Maybe
import           Text.HTML.Scalpel


data ServerEntry = ServerEntry {version :: String, url :: String}         deriving (Eq)
data ServerList  = ServerList {group :: String, entries :: [ServerEntry]} deriving (Eq)

instance Show ServerEntry where
  show (ServerEntry v u) = "Version: " ++ v ++ "\nUrl: " ++ u

instance Show ServerList where
  show (ServerList v e) = v ++ "\n" ++ formatLines e
    where
      formatLines :: [ServerEntry] -> String
      formatLines = concatMap (concat . (fmap (\x -> " - " ++ x ++ "\n")) . lines . show)

serverTitle :: Scraper String String
serverTitle = text $ "span" @: [hasClass "btn"]

serverDownloadUrl :: Scraper String String
serverDownloadUrl = attr "href" $ "a" @: [hasClass "btn", hasClass "server"]

serverEntries :: Scraper String [ServerEntry]
serverEntries = chroots ("li" @: [hasClass "list-group-item", hasClass "release"]) entry
  where
    entry = do
      v <- text $ "strong" @: [hasClass "version"]
      ServerEntry v <$> serverDownloadUrl

serverLists :: IO [ServerList]
serverLists = take 2 . fromJust <$> scrapeURL "http://mcversions.net/" serverLists'
  where
    serverList :: Scraper String ServerList
    serverList = do
      title <- serverTitle
      ServerList title <$> serverEntries
    serverLists' :: Scraper String [ServerList]
    serverLists' = chroots ("div" @: [hasClass "col-xs-12", hasClass "col-sm-6", hasClass "col-md-3"]) serverList


findMatchingVersion :: ServerList -> String -> Maybe ServerEntry
findMatchingVersion (ServerList _ e) v = find (\x -> (version x) == v) e


getDownload :: Int -> String -> IO (Maybe String)
getDownload g v = do
  lists <- serverLists
  (return . fmap url) $ findMatchingVersion (lists !! g) v


printLists = do
  servers <- serverLists
  mapM_ print servers
