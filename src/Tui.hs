{-# LANGUAGE OverloadedStrings #-}

module Tui where

import Brick
import Brick.AttrMap
import Brick.Main (ViewportScroll, vScrollBy)
import Brick.Types (BrickEvent(VtyEvent), ViewportType(Vertical))
import Brick.Util (fg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core (str, vBox, viewport, withAttr)
import Cursor.Simple.List.NonEmpty
  ( NonEmptyCursor
  , makeNonEmptyCursor
  , nonEmptyCursorCurrent
  , nonEmptyCursorNext
  , nonEmptyCursorPrev
  , nonEmptyCursorSelectNext
  , nonEmptyCursorSelectPrev
  )
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Maybe
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar, KDown, KUp))
import Issues
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (die)
import System.Process (readProcess)

data Flag
  = FileFlag String
  | UrlFlag String
  | Help
  deriving (Show, Eq)

data AutoUrl
  = AutoUrl String
  | Fail
  deriving (Show, Eq)

-- This will automatically fail if directory is not a git repository
getUrlFromGitRepository :: IO AutoUrl
getUrlFromGitRepository = do
  remote <- readProcess "git" ["remote", "-v"] []
  let url = head . tail . words $ head $ lines remote
  case "github" `isInfixOf` url of
    True -> return $ AutoUrl $ formGithubUrl url
    False ->
      case "gitlab" `isInfixOf` url of
        True -> return $ AutoUrl $ formGitlabUrl url
        False -> return Fail

gitlabIssuesUrlPrefix = "https://gitlab.com/api/v4/projects/"

-- /commento%2Fcommento/issues?state=opened
formGitlabUrl :: String -> String
formGitlabUrl ('g':'i':'t':rest) =
  gitlabIssuesUrlPrefix ++
  (replaceBackSlash . giturl) rest ++ "/issues?state=opened"
formGitlabUrl ('h':'t':'t':'p':rest) =
  gitlabIssuesUrlPrefix ++
  (replaceBackSlash . httpurl) rest ++ "/issues?state=opened"

replaceBackSlash :: String -> String
replaceBackSlash ('/':xs) = "%2F" ++ xs
replaceBackSlash (x:xs) = x : replaceBackSlash xs

-- assumes url is of form 'git@github.com:user/repo.git' or 'git@gitlab.com:user/repo.git`
giturl :: String -> String
giturl (':':xs) = removeGitSuffix xs
giturl (x:xs) = giturl xs

-- assumes url is of form 'https://github.com/user/repo.git
httpurl ('m':'/':xs) = removeGitSuffix xs
httpurl (x:xs) = httpurl xs

-- /repos/:owner/:repo/issues
-- https://api.github.com/repos/vmg/redcarpet/issues
--
githubIssuesUrlPrefix = "https://api.github.com/repos/" -- vmg/redcarpet/issues

formGithubUrl :: String -> String
formGithubUrl ('g':'i':'t':rest) =
  githubIssuesUrlPrefix ++ giturl rest ++ "/issues"
formGithubUrl ('h':'t':'t':'p':rest) =
  githubIssuesUrlPrefix ++ httpurl rest ++ "/issues"

removeGitSuffix :: String -> String
removeGitSuffix xs
  | ".git" `isSuffixOf` xs = iterate init xs !! 4
  | otherwise = xs

options :: [OptDescr Flag]
options =
  [ Option ['f'] ["file"] (ReqArg FileFlag "FILE") "input FILE"
  , Option ['u'] ["url"] (OptArg defaultString "URL") "url for issues"
  , Option ['h', '?'] ["help"] (NoArg Help) "help details"
  ]

defaultFileFlag :: Maybe String -> Flag
defaultFileFlag = FileFlag . fromMaybe ""

defaultString :: Maybe String -> Flag
defaultString = UrlFlag . fromMaybe ""

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, errs) ->
      ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: ic [OPTION...] files..."

tui :: IO ()
tui = do
  args <- getArgs
  (actions, nonOptions) <- compilerOpts args
  -- If theres help in actions, then print usage and die
  if null actions
    then do
      giturl <- getUrlFromGitRepository
      case giturl of
        Fail -> die "Not a git repository"
        AutoUrl url -> urlTui url
    else if Help `elem` actions
           then die $ usageInfo "Usage: hidd [OPTION...] " options
           else case head actions of
                  FileFlag a -> do
                    initialState <- buildInitialState a
                    endState <- defaultMain tuiApp initialState
                    print endState
                  UrlFlag a -> urlTui a

urlTui :: String -> IO ()
urlTui a = do
  initialState <- buildInitialUrlState a
  endState <- defaultMain tuiApp initialState
  print endState

data TuiState =
  TuiState
    { tuiStateIssues :: NonEmptyCursor Issue
    }
  deriving (Show)

data ResourceName =
  IssuesList
  deriving (Ord, Show, Eq)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("selected", fg red)]
    }

buildInitialUrlState :: String -> IO TuiState
buildInitialUrlState u = do
  issues <- getJSONFromUrl u
  case issues of
    Nothing -> die "Problem with json from url"
    Just is ->
      case NE.nonEmpty $ catMaybes is of
        Nothing -> die "There are no contents"
        Just ne -> pure TuiState {tuiStateIssues = makeNonEmptyCursor ne}

buildInitialState :: String -> IO TuiState
buildInitialState f = do
  issues <- getJSONFromFile f
  case issues of
    Nothing -> die "There are no contents"
    Just is ->
      case NE.nonEmpty $ catMaybes is of
        Nothing -> die "There are no contents"
        Just ne -> pure TuiState {tuiStateIssues = makeNonEmptyCursor ne}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  let nec = tuiStateIssues ts
   in [ border $
        viewport IssuesList Vertical $
        vBox $
        concat
          [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec
          , [drawPath True $ nonEmptyCursorCurrent nec]
          , map (drawPath False) $ nonEmptyCursorNext nec
          ]
      ]

-- drawTui ts = [vBox $ map str $ tuiStateIssues ts]
drawPath :: Bool -> Issue -> Widget n
drawPath b issue =
  (if b
     then withAttr "selected"
     else id) .
  str $
  issueSummary issue

handleTuiEvent ::
     TuiState
  -> BrickEvent ResourceName e
  -> EventM ResourceName (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey KDown [] -> do
          case nonEmptyCursorSelectNext $ tuiStateIssues s of
            Nothing -> continue s
            Just nec -> do
              let vp1Scroll = viewportScroll IssuesList
              vScrollBy vp1Scroll 1
              continue $ s {tuiStateIssues = nec}
              -- scroll <- vScrollBy vp1Scroll 1 -- >> continue $ s {tuiStateIssues = nec}
        EvKey KUp [] -> do
          case nonEmptyCursorSelectPrev $ tuiStateIssues s of
            Nothing -> continue s
            Just nec -> do
              let vp1Scroll = viewportScroll IssuesList
              vScrollBy vp1Scroll (-1)
              continue $ s {tuiStateIssues = nec}
        _ -> continue s
    _ -> continue s
