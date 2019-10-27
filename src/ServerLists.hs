{-# LANGUAGE OverloadedStrings #-}

module ServerLists where

import           Data.List
import           Text.HTML.Scalpel
import           Control.Monad

data ServerEntry = ServerEntry {version :: String, url :: String}         deriving (Eq)
data ServerList  = ServerList  {group :: String, entries :: [ServerEntry]} deriving (Eq)

instance Show ServerEntry where
  show (ServerEntry v u) = "Version: " ++ v ++ "\nUrl: " ++ u

instance Show ServerList where
  show (ServerList v e) = v ++ "\n" ++ formatLines e
    where formatLines = concatMap (concat . (fmap (\x -> " - " ++ x ++ "\n")) . lines . show)

extractServerEntries :: Scraper String [ServerEntry]
extractServerEntries = chroots ("li" @: [hasClass "list-group-item", hasClass "release"]) extractor
  where
    extractVersion = text $ "strong" @: [hasClass "version"]
    extractUrl     = attr "href" $ "a" @: [hasClass "btn", hasClass "server"]
    extractor      = ServerEntry <$> extractVersion <*> extractUrl

extractServerLists :: Scraper String [ServerList]
extractServerLists = chroots ("div" @: [hasClass "col-xs-12", hasClass "col-sm-6", hasClass "col-md-3"]) extractor
  where
    extractTitle = text $ "span" @: [hasClass "btn"]
    extractor    = ServerList <$> extractTitle <*> extractServerEntries

serverLists :: IO (Maybe [ServerList])
serverLists = fmap (take 2) <$> scrapeURL "http://mcversions.net/" extractServerLists

findMatchingVersion :: String -> ServerList -> Maybe ServerEntry
findMatchingVersion version' (ServerList _ entries') = find (\x -> (version x) == version') entries'

getDownload :: String -> IO (Maybe ServerEntry)
getDownload versionString = do
  maybeServers <- serverLists
  return $ case maybeServers of
    Nothing      -> Nothing
    Just servers -> msum $ fmap (findMatchingVersion versionString) servers 

printLists = do
  servers <- serverLists
  mapM_ print servers
