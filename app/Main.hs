{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Monad                 (void)
import Data.Functor                  (($>))
import Data.Text                     (Text)
import qualified Data.Text           as Text
import GI.Gtk                        (Box (..), Button (..)
                                     ,Label (..), Orientation (..)
                                     ,Window (..), CheckButton (..)
                                     ,Image (..), ListBoxRow (..)
                                     , ListBox (..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import System.Random (randomRIO)
import Data.Char (toUpper)
import Data.List (isPrefixOf, sort, (\\))
import System.Directory (getDirectoryContents)

data State = Game { correct :: [Char]
                  , wrongs  :: [Char]
                  , hints   :: [Char]
                  , secret  :: String
                  , chances :: Int
                  , try     :: Int
                  , hangman :: [Text]
                  }

data Event a = Guess a | Wrong a | GetHint a | Quit | PlayAgain

playAgainBtn :: Widget Button
playAgainBtn = _

quitBtn :: Widget Button
playAgainBtn = _

hintBtn :: Widget Button
hintBtn = _

hintLabel :: Widget Label 
hintLabel = _

wrongGuessesLabel :: Widget Box
wrongGuessesLabel = _

entry :: Widget Box
entry = _

patternLabel :: Widget Label
patternLabel = "Pattern: "


getSecret :: [String] -> IO String
getSecret xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

getHint :: State -> Char
getHint (Game c w h s ch t p) = head $ (s \\ c) \\ h

view' :: State -> AppView Window (Event Char)
view' Game {..} = 
  bin
    Window [ #title := "Hangman"
           , on #deleteEvent (const (True, Quit))
           , #widthRequest := 640
           , #heightRequest := 480
           ]
    $ paned
        [#wideHandle := True]
        (pane defaultPaneProperties { resize = True } $
          widget Image [#file := (hangman !! try) ])
        (pane defaultPaneProperties { resize = True, shrink = False } $
          container Box []
            [ widget Button []
            , widget CheckButton []
            , widget Label [#label := (hangman !! try)]
            ])

update' :: State -> Event Char -> Transition State (Event Char)
update' (Game c w h s ch t p) (Guess a) =
    Transition (Game (c ++ [a]) w h s ch (t + 1) p) (return Nothing)
update' (Game c w h s ch t p) (Wrong a) = 
    Transition (Game c (w ++ [a]) h s (ch - 1) (t + 1) p) (return Nothing)
update' (Game c w h s ch t p) (GetHint a) = 
    Transition (Game c w (h ++ [a]) s (ch - 1) (t + 1) p) (return Nothing)
update' _ Quit = Exit
update' _ PlayAgain = Exit

main :: IO ()
main = do
  words  <-  lines <$> readFile "assets/dictionary"
  secret <-  getSecret words
  paths  <-  sort . filter (isPrefixOf "Hangman") <$> getDirectoryContents "assets/"
  void $ run App
    { view = view'
    , update = update'
    , inputs = []
    , initialState = Game "" "" "" secret (length secret) 0 $
      (Text.pack . ((++) "assets/") <$> paths)
    }
