{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (isLeft)
import Data.List (intercalate)
import Text.Printf (printf)

import System.Random

import Graphics.Vty

newtype Str = Str Int -- 0 = E string
  deriving (Eq, Ord, Num, Enum, Random)

strOffset :: Str -> Note
strOffset (Str 0) = 0
strOffset (Str 1) = 7
strOffset (Str 2) = 3
strOffset (Str 3) = 10
strOffset (Str 4) = 5
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

data StrPrefix = Colon | Arrow
data StrMode
  = Wholes StrPrefix Str
  | Sharps StrPrefix Str
  | Flats StrPrefix Str
  | All StrPrefix Str
  | Mark StrPrefix Str Note
  | Empty StrPrefix Str
  | Frets

printString :: StrMode -> String
printString mode = mconcat
  [ case mode of
      Wholes _ str -> either id (error "printString") $ showNote (strOffset str)
      Sharps _ str -> either id (error "printString") $ showNote (strOffset str)
      Flats _ str  -> either id (error "printString") $ showNote (strOffset str)
      All _ str    -> either id (error "printString") $ showNote (strOffset str)
      Mark _ str n -> either id (error "printString") $ showNote (strOffset str)
      Empty _ str  -> either id (error "printString") $ showNote (strOffset str)
      Frets        -> " "

  , case prefix of
      Just Colon -> " :: "
      Just Arrow -> " -> "
      Nothing    -> "    "

  , mconcat $ map (<> " | ")
      [ case mode of
          Wholes _ str -> printf "%5s" $ either id (const "") (showNote $ strOffset str + x)
          Sharps _ str -> printf "%5s" $ either id fst (showNote $ strOffset str + x)
          Flats _ str  -> printf "%5s" $ either id snd (showNote $ strOffset str + x)
          All _ str    -> printf "%5s" $ either id (\(x, y) -> x <> "/" <> y) (showNote $ strOffset str + x)
          Mark _ str n -> printf "%5s" $ if n == x then "*" else "" :: String
          Empty _ str  -> printf "%5s" ("" :: String)
          Frets        -> printf "%5s" $ show i
      | (i, x) <- zip [1..] $ take 12 $ [1..]
      ]
  ]
  where
    prefix = case mode of
      Wholes p _ -> Just p
      Sharps p _ -> Just p
      Flats p _  -> Just p
      All p _    -> Just p
      Mark p _ _ -> Just p
      Empty p _  -> Just p
      Frets      -> Nothing

fretImage :: [StrMode] -> Image
fretImage strs = vertCat $ map (string defAttr) $ mconcat
  [ [ printString str | str <- reverse strs ]
  ,
    [ ""
    , printString Frets
    ]
  ]

fretImageForMark :: Str -> Note -> Image
fretImageForMark s n = fretImage
  [ if str == s then Mark Colon str n else Empty Colon str
  | str <- [0..5]
  ]

rndNotes :: Vty -> IO ()
rndNotes vty = do
  str <- randomRIO (0, 5)
  n   <- randomRIO (1, 12)
  update vty $ picForImage $ fretImageForMark str n
  e <- nextEvent vty
  if e == EvKey (KChar ' ') []
    then pure ()
    else do
      update vty $ picForImage $ string defAttr $ case showNote (strOffset str + n) of
        Left n -> n
        Right (x, y) -> x <> "/" <> y

      e <- nextEvent vty
      if e == EvKey (KChar ' ') []
        then pure ()
        else rndNotes vty

main :: IO ()
main = do
  let cfg = defaultConfig
       { mouseMode = Just True
       }
  vty <- mkVty cfg
  rndNotes vty
  shutdown vty
    
