{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Issues where

import Data.Aeson (FromJSON, ToJSON, Value, decode, encode)
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import System.IO

data User =
  User
    { login :: String
    }
  deriving (Generic, Show)

data GithubIssue =
  GithubIssue
    { url :: String
    , number :: Int
    , title :: String
    , user :: User
    , state :: String
    , created_at :: String
    , body :: String
    }
  deriving (Generic, Show)

instance FromJSON User

instance FromJSON GithubIssue

getJSONFromFile :: String -> IO (Maybe [Maybe GithubIssue])
getJSONFromFile f = do
  content <- B.readFile f
  return (decode content :: Maybe [Maybe GithubIssue])

getJSONFromUrl :: String -> IO (Maybe [Maybe GithubIssue])
getJSONFromUrl u = do
  manager <- newManager tlsManagerSettings
  urlRequest <- parseRequest u
  let request =
        addRequestHeader "User-Agent" "Haskell" $
        setRequestManager manager urlRequest
  response <- httpJSON request
  return (getResponseBody response :: Maybe [Maybe GithubIssue])

displayGithubIssue :: GithubIssue -> IO ()
displayGithubIssue x = print (show (number x) ++ " " ++ title x)

displayMultipleIssues :: [Maybe GithubIssue] -> IO ()
displayMultipleIssues x =
  let dealWithMaybe a =
        case a of
          Just i -> displayGithubIssue i
          Nothing -> print ""
   in sequence_ (map dealWithMaybe x)

someFunc :: IO ()
someFunc = do
  json <- getJSONFromFile "github.json"
  case json of
    Just a -> displayMultipleIssues a
    Nothing -> putStrLn "Nothing"
