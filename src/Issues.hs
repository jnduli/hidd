{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Issues where

import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(..)
  , Value
  , (.:)
  , (.:?)
  , decode
  , encode
  , parseJSON
  )
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import System.IO

data User
  = GitlabUser
      { name :: String
      , username :: String
      }
  | GithubUser
      { login :: String
      }
  deriving (Generic, Show)

data Issue
  = GitlabIssue
      { title :: String
      , iid :: Int
      , description :: String
      , author :: User
      , created_at :: String
      , state :: String
      }
  | GithubIssue
      { url :: String
      , number :: Int
      , title :: String
      , user :: User
      , state :: String
      , created_at :: String
      , body :: String
      }
  deriving (Generic, Show)

instance FromJSON User where
  parseJSON (Object v) =
    case H.lookup "username" v of
      Just a -> GitlabUser <$> v .: "name" <*> v .: "username"
      Nothing ->
        case H.lookup "login" v of
          Just a -> GithubUser <$> v .: "login"
          Nothing -> fail "Not a user"

instance FromJSON Issue where
  parseJSON (Object v) =
    case H.lookup "number" v of
      Just a ->
        GithubIssue <$> v .: "url" <*> v .: "number" <*> v .: "title" <*>
        v .: "user" <*>
        v .: "state" <*>
        v .: "created_at" <*>
        v .: "body"
      Nothing ->
        case H.lookup "iid" v of
          Just a ->
            GitlabIssue <$> v .: "title" <*> v .: "iid" <*> v .: "description" <*>
            v .: "author" <*>
            v .: "created_at" <*>
            v .: "state"
          Nothing -> fail "Json not known"

getJSONFromFile :: String -> IO (Maybe [Maybe Issue])
getJSONFromFile f = do
  content <- B.readFile f
  return (decode content :: Maybe [Maybe Issue])

getJSONFromUrl :: String -> IO (Maybe [Maybe Issue])
getJSONFromUrl u = do
  manager <- newManager tlsManagerSettings
  urlRequest <- parseRequest u
  let request =
        addRequestHeader "User-Agent" "Haskell" $
        setRequestManager manager urlRequest
  response <- httpJSON request
  return (getResponseBody response :: Maybe [Maybe Issue])

issueSummary :: Issue -> String
issueSummary (GitlabIssue title iid _ _ _ _) = show iid ++ " " ++ title
issueSummary (GithubIssue _ number title _ _ _ _) = show number ++ " " ++ title

issueDetails :: Issue -> String
issueDetails (GitlabIssue title iid description author created_at body) =
  show iid ++ "\n" ++ title ++ "\n" ++ description
issueDetails (GithubIssue _ number title user state created_at body) =
  show number ++ "\n" ++ title ++ "\n" ++ body
