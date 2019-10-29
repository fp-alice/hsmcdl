{-# LANGUAGE OverloadedStrings #-}

module ServerLists where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List
import           Network.HTTP.Client
import qualified Network.HTTP.Simple        as H
import           Text.HTML.Scalpel

data ServerEntry = ServerEntry {version :: String, url :: String}
data ServerList  = ServerList  {group :: String, entries :: [ServerEntry]}

instance Show ServerEntry where
  show (ServerEntry v u) = "Version: " ++ v ++ "\nUrl: " ++ u

instance Show ServerList where
  show (ServerList v e) = v ++ "\n" ++ formatLines e
    where formatLines = concatMap (concatMap (\x -> " - " ++ x ++ "\n") . lines . show)

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

getDownloadUrl :: String -> IO (Maybe ServerEntry)
getDownloadUrl versionString = do
  maybeServers <- serverLists
  return $ case maybeServers of
    Nothing      -> Nothing
    Just servers -> msum $ fmap (findMatchingVersion versionString) servers

downloadFile :: ServerEntry -> IO ()
downloadFile (ServerEntry _ location) = do
  i <- H.parseRequest location
  let req = i { method = "GET" }
  res <- H.httpLBS req
  let out = H.getResponseBody res
  L8.writeFile "server.jar" out

printLists :: IO ()
printLists = do
  servers <- serverLists
  case servers of
    Nothing   -> return ()
    Just list -> mapM_ (mapM_ putStrLn) versions
      where
        es       = fmap entries list
        versions = fmap (fmap version) es
