module Main where

import           Data.Monoid         ((<>))
import           Options.Applicative
import           ServerLists

data Options = Options
  { version :: Maybe String
  , list    :: Bool }

options :: Parser Options
options = Options
  <$> (optional $ strOption
      ( long    "version"
     <> short   'v'
     <> metavar "VERSION"
     <> help    "Version to download (eg. 1.14.4, 19w34a)"))
  <*> switch
      ( long    "list"
     <> short   'l'
     <> help    "Whether to list all available versions or not")

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Downloads and lists minecraft server versions"
     <> header   "hsmcdl: Haskell Minecraft Server Downloader")

run :: Options -> IO ()
run (Options Nothing True) = printLists
run (Options (Just v) _)   = do
  maybeUrl <- getDownloadUrl v
  case maybeUrl of
    Just u  -> downloadFile u
    Nothing -> putStrLn "No such version"
run _                      = return ()
