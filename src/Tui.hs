{-# LANGUAGE OverloadedStrings #-}

module Tui where

import Brick
import Brick.AttrMap
import Brick.Types (BrickEvent(VtyEvent))
import Brick.Util (fg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core (str, vBox, withAttr)
import Cursor.Simple.List.NonEmpty
  ( NonEmptyCursor
  , makeNonEmptyCursor
  , nonEmptyCursorCurrent
  , nonEmptyCursorNext
  , nonEmptyCursorPrev
  , nonEmptyCursorSelectNext
  , nonEmptyCursorSelectPrev
  )
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Maybe
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar, KDown, KUp))
import Issues
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (die)

data Flag
  = File String
  | Url String
  | Help
  deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option ['f'] ["file"] (ReqArg File "FILE") "input FILE"
  , Option ['u'] ["url"] (OptArg defaultString "URL") "url for issues"
  , Option ['h', '?'] ["help"] (NoArg Help) "help details"
  ]

defaultFile :: Maybe String -> Flag
defaultFile = File . fromMaybe ""

defaultString :: Maybe String -> Flag
defaultString = Url . fromMaybe ""

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
  if Help `elem` actions
    then die $ usageInfo "Usage: hidd [OPTION...] " options
    else case head actions of
           File a -> do
             initialState <- buildInitialState a
             endState <- defaultMain tuiApp initialState
             print endState
           Url a -> die "Url not supported yet"

data TuiState =
  TuiState
    { tuiStateIssues :: NonEmptyCursor GithubIssue
    }
  deriving (Show)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("selected", fg red)]
    }

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
        vBox $
        concat
          [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec
          , [drawPath True $ nonEmptyCursorCurrent nec]
          , map (drawPath False) $ nonEmptyCursorNext nec
          ]
      ]

-- drawTui ts = [vBox $ map str $ tuiStateIssues ts]
drawPath :: Bool -> GithubIssue -> Widget n
drawPath b issue =
  (if b
     then withAttr "selected"
     else id) .
  str $
  (show (number issue) ++ " " ++ title issue)

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey KDown [] -> do
          case nonEmptyCursorSelectNext $ tuiStateIssues s of
            Nothing -> continue s
            Just nec -> continue $ s {tuiStateIssues = nec}
        EvKey KUp [] -> do
          case nonEmptyCursorSelectPrev $ tuiStateIssues s of
            Nothing -> continue s
            Just nec -> continue $ s {tuiStateIssues = nec}
        _ -> continue s
    _ -> continue s
