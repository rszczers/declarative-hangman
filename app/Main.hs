{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Utils                
import Control.Monad                      (void)
import Data.Text                          (Text, snoc, null)
import Data.List                          (intersperse, intercalate)
import qualified Data.Text                as Text
import GI.Gtk                             ( entryGetText, entrySetText
                                          ,Box (..), Button (..)
                                          ,Label (..), Orientation (..)
                                          ,Window (..), CheckButton (..)
                                          ,Image (..), ListBoxRow (..)
                                          ,ListBox (..), Entry (..))
import qualified GI.Gtk                   as Gtk
import qualified GI.Gdk                   as Gdk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Data.Char                          (toUpper)
import Data.List                          (isPrefixOf, sort, (\\))
import System.Directory                   (getDirectoryContents)
import Data.ByteString                    (ByteString)
import Control.Concurrent.Async           (async)

data State = Game { correct    :: [Char]
                  , wrongs     :: [Char]
                  , secret     :: [Char]
                  , hint       :: [Char]
                  , chances    :: Int
                  , hangman    :: [Text]
                  , dictionary :: [[Char]]
                  }

data Event = Guess Text
           | GetHint Char
           | PlayAgain [Char]
           | Quit

stateBox :: State -> Widget Event
stateBox s@(Game {..}) | chances > 1 = gameBox s
                       | otherwise   = endBox s
 
gameBox :: State -> Widget Event
gameBox s = container Box [#orientation := OrientationVertical]
  [ BoxChild defaultBoxChildProperties { padding = 10 } guessBox
  , BoxChild defaultBoxChildProperties { padding = 10 } $ hintLabel s
  , BoxChild defaultBoxChildProperties { padding = 5 } $ hintBtn s
  ]

endBox :: State -> Widget Event
endBox s = container Box [#orientation := OrientationVertical]
  [ BoxChild defaultBoxChildProperties { padding = 10 } $ failLabel s
  , BoxChild defaultBoxChildProperties { padding = 5 } $ playAgainBtn s
  , BoxChild defaultBoxChildProperties { padding = 5 } quitBtn
  ]

guessBox :: Widget Event
guessBox = container Box [#orientation := OrientationHorizontal]
  [ BoxChild defaultBoxChildProperties { expand = True } choseLabel
  , BoxChild defaultBoxChildProperties { expand = True } guessEntry]

playAgainBtn :: State -> Widget Event
playAgainBtn Game {..} = widget Button
  [#label := "Play Again!", onM #clicked playAgain']
  where playAgain' = \b -> PlayAgain <$> getRandom dictionary

quitBtn :: Widget Event
quitBtn = widget Button [#label := "No More!", on #clicked Quit]

hintBtn :: State -> Widget Event
hintBtn Game {..} = widget Button
  [#label := "Get a hint!", onM #clicked hintEvent]
  where hintEvent = \b -> GetHint <$> getRandom (secret \\ correct)

hintLabel :: State -> Widget Event
hintLabel Game {..} = case (Prelude.null hint) of
  True -> widget Label [#label := ""]
  False -> widget Label
    [ classes ["blue"]
    , #label := ("Consider choosing: " <> Text.pack hint)]

wrongGuessesLabel :: State -> Widget Event
wrongGuessesLabel Game {..} = widget Label
  [#label := ("Wrong guesses: " <> (Text.pack wrongs'))]
  where wrongs' = intercalate ", " ((\c -> [c]) <$> wrongs)

choseLabel:: Widget Event
choseLabel = widget Label [#label := "Choose a letter: "]

guessEntry :: Widget Event
guessEntry = widget Entry [#text := "", onM #activate toGuessEvent]
  where toGuessEvent w = do
          input <- Guess <$> entryGetText w
          entrySetText w ""
          return input

patternLabel :: State -> Widget Event
patternLabel s = widget Label [ Classes ["pattern"]
                              , #label := ("Pattern: " <>
                                          (Text.pack $ getPattern s))]

failLabel :: State -> Widget Event
failLabel Game {..} = widget Label
  [ classes ["red"]
  , #label := ("You have run out of guessess,\n the word was: " <>
              (Text.pack secret))]

getPattern :: State -> String
getPattern Game {..} = intersperse ' ' $ f <$> secret
  where f x | (toUpper x) `elem` correct = toUpper x
--          | (toUpper x) `elem` hints = toUpper x -- hint button adds
            | otherwise = '_'                      -- answer

checkGuess :: State -> Char -> State
checkGuess (Game c w s hi ch h words) g
  | g' `elem` c = Game c w s "" (ch - 1) h words
  | g' `elem` w = Game c w s "" (ch - 1) h words
  | g' `elem` s = Game (c ++ [g']) w s "" ch h words
  | otherwise = Game c (w ++ [g']) s "" (ch - 1) h words
  where g' = toUpper g

view' :: State -> AppView Window Event
view' s@(Game {..}) =
  bin
    Window [ #title := "Hangman"
           , on #deleteEvent (const (True, Quit))
           , #widthRequest  := 640
           , #heightRequest := 480
           ]
    $ container Box [#orientation := OrientationHorizontal]
        [ BoxChild defaultBoxChildProperties
            { padding = 20, expand = True } $
            widget Image [#file := (hangman !! (chances - 1))]
        , BoxChild defaultBoxChildProperties { padding = 10, expand = True } $
            container Box [#orientation := OrientationVertical]
              [ BoxChild defaultBoxChildProperties
                  { padding = 10, expand = True } $ patternLabel s
              , BoxChild defaultBoxChildProperties
                  { padding = 10, expand = True } $ wrongGuessesLabel s
              , BoxChild defaultBoxChildProperties
                  { padding = 10 } $ stateBox s
              ]
        ]     

update' :: State -> Event -> Transition State Event
update' a@(Game c w s hi ch h words) (Guess g)
  | Text.null g    = Transition a (return Nothing)
  | otherwise = Transition (checkGuess a g') (return Nothing)
  where g' = last (Text.unpack g)

update' (Game c w s hi ch h words) (GetHint hi') =
  Transition (Game c w s [hi'] (ch - 1) h words) (return Nothing)

update' _ Quit = Exit

update' (Game c w s hi ch h words) (PlayAgain s') =
  Transition (Game "" "" s' "" (length h) h words) (return Nothing)

styles :: ByteString
styles = mconcat
  [ "button {border: 2px solid gray; font-weight: 800;}"
  , ".pattern {font-size: 2em;}"
  , ".red {font-style: italic; font-size: 1.5em; color: red;}"
  , ".blue {font-style: italic; font-size: 1.5em; color: blue;}"
  ]

main :: IO ()
main = do
  void $ Gtk.init Nothing

  screen <- maybe (fail "No screen :(") return =<< Gdk.screenGetDefault
  p      <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p styles
  Gtk.styleContextAddProviderForScreen
    screen
    p
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  void . async $ do
    words  <-  (map . map) toUpper <$> lines <$> readFile "assets/dictionary"
    secret <-  getRandom words
    paths  <-  reverse . sort . filter (isPrefixOf "Hangman") <$>
                 getDirectoryContents "assets/"
    let chances = length paths

    void $ runLoop (App { view = view'
                   , update = update'
                   , inputs = []
                   , initialState = Game "" "" secret "" chances
                       (Text.pack . ((++) "assets/") <$> paths) words
                   })
    Gtk.mainQuit
  Gtk.main
