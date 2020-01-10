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
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar, KDown, KUp))
import Issues
import System.Exit (die)

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

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

buildInitialState :: IO TuiState
buildInitialState = do
  issues <- getJSONFromFile "github.json"
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
