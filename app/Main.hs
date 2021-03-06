{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, readTChan)

import Data.Either (isLeft)
import Data.List (intercalate)
import Text.Printf (printf)

import System.Random

import Graphics.Vty hiding (nextEvent)

orr :: TChan a -> [TChan a -> IO b] -> IO b
orr _ [] = do
  threadDelay maxBound
  pure undefined
orr ch (io:ios) = do
  dch <- atomically $ dupTChan ch
  either id id <$> race (io dch) (orr ch ios)

eventChan :: Vty -> TChan Event
eventChan = _eventChannel . inputIface

getEvent :: TChan Event -> IO Event
getEvent ch = do
  e <- atomically $ readTChan ch

  case e of
    EvMouseDown _ _ _ _ -> do
      _ <- atomically $ readTChan ch -- mouse up
      pure e
    _ -> pure e

newtype Str = Str Int -- 0 = E string
  deriving (Eq, Ord, Num, Enum, Random)

strOffset :: Str -> Note
strOffset (Str 0) = 0
strOffset (Str 1) = 5
strOffset (Str 2) = 10
strOffset (Str 3) = 3
strOffset (Str 4) = 7
strOffset (Str 5) = 0

newtype Note = Note Int -- 0 = E on 1st string
  deriving (Eq, Ord, Num, Enum, Random)

showNote :: Note -> Either String (String, String)
showNote (Note 0)  = Left "E"
showNote (Note 1)  = Left "F"
showNote (Note 2)  = Right ("F#", "Gb")
showNote (Note 3)  = Left "G"
showNote (Note 4)  = Right ("G#", "Ab")
showNote (Note 5)  = Left "A"
showNote (Note 6)  = Right ("A#", "Bb")
showNote (Note 7)  = Left "B"
showNote (Note 8)  = Left "C"
showNote (Note 9)  = Right ("C#", "Db")
showNote (Note 10) = Left "D"
showNote (Note 11) = Right ("D#", "Eb")
showNote (Note x)  = showNote $ Note (x `mod` 12)

isWhole :: Note -> Bool
isWhole = isLeft . showNote

normalizeNote :: Note -> Note
normalizeNote (Note n) = Note (n `mod` 12)

noteToFret :: Str -> Note -> Int
noteToFret str n
  | n' == 0 = 12
  | otherwise = n'
  where
    Note n' = normalizeNote (n - strOffset str)

data StrPrefix = Colon | Arrow
data StrMode
  = Wholes StrPrefix Str
  | Sharps StrPrefix Str
  | Flats StrPrefix Str
  | All StrPrefix Str
  | Mark StrPrefix Str Note
  | Empty StrPrefix Str
  | Frets

stringImage :: Maybe Note -> StrMode -> Image
stringImage hln mode = horizCat
  [ string defAttr $ case mode of
      Wholes _ str -> either id (error "printString") $ showNote (strOffset str)
      Sharps _ str -> either id (error "printString") $ showNote (strOffset str)
      Flats _ str  -> either id (error "printString") $ showNote (strOffset str)
      All _ str    -> either id (error "printString") $ showNote (strOffset str)
      Mark _ str n -> either id (error "printString") $ showNote (strOffset str)
      Empty _ str  -> either id (error "printString") $ showNote (strOffset str)
      Frets        -> " "

  , string defAttr $ case prefix of
      Just Colon -> " :: "
      Just Arrow -> " -> "
      Nothing    -> "    "

  , horizCat $ map (`horizJoin` string defAttr " | ")
      [ case mode of
          Wholes _ str -> string (hlAttr str x) $ printf "%5s" $ either id (const "") (showNote $ strOffset str + x)
          Sharps _ str -> string (hlAttr str x) $ printf "%5s" $ either id fst (showNote $ strOffset str + x)
          Flats _ str  -> string (hlAttr str x) $ printf "%5s" $ either id snd (showNote $ strOffset str + x)
          All _ str    -> string (hlAttr str x) $ printf "%5s" $ either id (\(x, y) -> x <> "/" <> y) (showNote $ strOffset str + x)
          Mark _ str n -> string defAttr $ printf "%5s" $ if n == x then "*" else "" :: String
          Empty _ str  -> string defAttr $ printf "%5s" ("" :: String)
          Frets        -> string defAttr $ printf "%5s" $ show i
      | (i, x) <- zip [1..] $ take 12 $ [1..]
      ]
  ]
  where
    hlAttr str n
      | Just (normalizeNote $ strOffset str + n) == fmap normalizeNote hln = defAttr `withBackColor` cyan `withForeColor` black
      | otherwise = defAttr

    prefix = case mode of
      Wholes p _ -> Just p
      Sharps p _ -> Just p
      Flats p _  -> Just p
      All p _    -> Just p
      Mark p _ _ -> Just p
      Empty p _  -> Just p
      Frets      -> Nothing

fretImage :: Maybe Note -> [StrMode] -> Image
fretImage hln strs = vertCat $ mconcat
  [ [ stringImage hln str | str <- reverse strs ]
  ,
    [ string defAttr ""
    , stringImage hln Frets
    ]
  ]

fretImageForMark :: Str -> Note -> Image
fretImageForMark s n = fretImage Nothing
  [ if str == s then Mark Colon str n else Empty Colon str
  | str <- [0..5]
  ]

rndNotes :: Vty -> TChan Event -> IO ()
rndNotes vty ch = do
  str <- randomRIO (0, 5)
  n   <- randomRIO (1, 12)

  update vty $ picForImage $ fretImageForMark str n

  _ <- getEvent ch

  update vty $ picForImage $ string defAttr $ case showNote (strOffset str + n) of
    Left n -> n
    Right (x, y) -> x <> "/" <> y

  _ <- getEvent ch

  rndNotes vty ch

playNoteOnString :: Vty -> TChan Event -> IO ()
playNoteOnString vty ch = do
  rstr <- randomRIO (0, 5)
  n    <- randomRIO (1, 12)

  e <- drawFret True rstr n ch

  case e of
    EvMouseDown x y _ _ -> let fret = (x - 5) `div` 8 + 1 in do
      update vty $ picForImage $ string defAttr $ if noteToFret rstr n == fret
        then "RIGHT"
        else "WRONG: " <> show (noteToFret rstr n)

      _ <- getEvent ch

      playNoteOnString vty ch

    _ -> playNoteOnString vty ch
  where
    drawFret mode rstr n ch = do

      if mode
        then update vty $ picForImage $ vertCat
          [ fretImage Nothing
              [ if str == rstr then Empty Arrow str else Empty Colon str
              | str <- [0..5]
              ]

          , string defAttr ""
          , string defAttr $ case showNote n of
              Left n -> n
              Right (x, y) -> x <> "/" <> y
          ]
        else update vty $ picForImage $ vertCat
          [ fretImage (Just n)
              [ All Colon str
              | str <- [0..5]
              ]
          ]

      e <- getEvent ch

      case e of
        EvKey (KChar 't') [] -> drawFret (not mode) rstr n ch
        EvKey (KChar 'n') [] -> pure e
        EvMouseDown _ _ _ _ -> pure e
        _ -> drawFret mode rstr n ch

showNotesOnStrings :: Note -> Vty -> TChan Event -> IO ()
showNotesOnStrings n vty ch = do
  update vty $ picForImage $ fretImage (Just n)
    [ All Colon str
    | str <- [0..5]
    ]

  e <- getEvent ch

  case e of
    EvKey (KChar 'j') [] -> showNotesOnStrings (normalizeNote (n + 1)) vty ch
    EvKey (KChar 'k') [] -> showNotesOnStrings (normalizeNote (n - 1)) vty ch
    _ -> showNotesOnStrings n vty ch

quitOnQ :: TChan Event -> IO ()
quitOnQ ch = do
  e <- getEvent ch

  if e /= EvKey (KChar 'q') []
    then quitOnQ ch
    else pure ()

menu :: [(String, IO ())] -> Int -> Vty -> TChan Event -> IO ()
menu opts optIndex vty ch = do
  update vty $ picForImage $ vertCat
    [ string (attr i) opt
    | (i, (opt, _)) <- zip [0..] opts
    ]

  e <- getEvent ch

  case e of
    EvKey (KChar 'j') [] -> menu opts ((optIndex + 1) `mod` length opts) vty ch
    EvKey (KChar 'k') [] -> menu opts ((optIndex - 1) `mod` length opts) vty ch
    EvKey KEnter [] -> do
      snd (opts !! optIndex)
      menu opts optIndex vty ch
    _ -> menu opts optIndex vty ch

  where
    attr x
      | optIndex == x = defAttr `withBackColor` cyan `withForeColor` black
      | otherwise = defAttr

main :: IO ()
main = do
  let cfg = defaultConfig
       { mouseMode = Just True
       }
  vty <- mkVty cfg

  orr (eventChan vty)
    [ menu (opts vty) 0 vty
    , quitOnQ
    ]

  -- orr (eventChan vty)
  --  [ showNotesOnStrings 0 vty
  --  , quitOnQ
  --  ]

  -- orr (eventChan vty)
  --  [ rndNotes vty
  --  , quitOnQ
  --  ]

  -- orr (eventChan vty)
  --   [ playNoteOnString vty
  --   , quitOnQ
  --   ]

  shutdown vty
  where
    opts vty =
      [ ( "Notes on string"
        , orr (eventChan vty)
            [ showNotesOnStrings 0 vty
            , quitOnQ
            ]
        )
      , ( "Guess note"
        , orr (eventChan vty)
            [ rndNotes vty
            , quitOnQ
            ]
        )
      , ( "Guess fret"
        , orr (eventChan vty)
            [ playNoteOnString vty
            , quitOnQ
            ]
        )
      ]
