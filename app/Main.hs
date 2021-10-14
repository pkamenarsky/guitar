{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (isLeft)
import Data.List (intercalate)
import Text.Printf (printf)

import Graphics.Vty

newtype Str = Str Int -- 0 = E string
  deriving (Eq, Ord, Num, Enum)

strOffset :: Str -> Note
strOffset (Str 0) = 0
strOffset (Str 1) = 7
strOffset (Str 2) = 3
strOffset (Str 3) = 10
strOffset (Str 4) = 5
strOffset (Str 5) = 0

newtype Note = Note Int -- 0 = E on 1st string
  deriving (Eq, Ord, Num, Enum)

instance Show Note where
  show (Note 0) = "E"
  show (Note 1) = "F"
  show (Note 2) = "F#/Gb"
  show (Note 3) = "G"
  show (Note 4) = "G#/Ab"
  show (Note 5) = "A"
  show (Note 6) = "A#/Bb"
  show (Note 7) = "B"
  show (Note 8) = "C"
  show (Note 9) = "C#/Db"
  show (Note 10) = "D"
  show (Note 11) = "D#/Eb"
  show (Note x) = show $ Note (x `mod` 12)

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
data StrMode = Wholes | Sharps | Flats | All | Mark Note | Frets

printString' :: StrPrefix -> StrMode -> Str -> String
printString' prefix mode str = mconcat
  [ case mode of
      Frets -> "    "
      _ -> show (strOffset str)
  , case prefix of
      Colon -> " :: "
      Arrow -> " -> "
  , mconcat $ map (<> " | ")
      [ case mode of
          Wholes -> printf "%5s" $ either id (const "") (showNote x)
          Sharps -> printf "%5s" $ either id fst (showNote x)
          Flats  -> printf "%5s" $ either id snd (showNote x)
          All    -> printf "%5s" $ either id (\(x, y) -> x <> "/" <> y) (showNote x)
          Mark n -> printf "%5s" $ if n == x then "*" else "" :: String
          Frets  -> printf "%5s" $ show i
      | (i, x) <- zip [1..] $ take 12 $ [strOffset str + 1..]
      ]
  ]

printString :: Note -> String
printString root = mconcat
  [ show root, " :: "
  , mconcat [ printf "%5s" (show x) <> " | " | x <- take 12 $ [root + 1..] ]
  ]

printEmpty :: String
printEmpty = mconcat
  [ "     "
  , mconcat [ "      | " | x <- [1..12] ]
  ]

printNumbering :: String
printNumbering = mconcat
  [ "     "
  , mconcat [ printf "%5s" (show x) <> " | " | x <- [1..12] ]
  ]

printMark :: Note -> String
printMark n = mconcat
  [ "     "
  , mconcat [ if Note x == n then "    * | " else "      | " | x <- [1..12] ]
  ]
  where
    s :: String -> String
    s = id

markFretImage :: Str -> Note -> Image
markFretImage s n = vertCat $ map (string defAttr) $ mconcat
  [ [ if s == 5 - str then printMark n else printEmpty | str <- [ 0..5 ]
    ]
  ,
    [ ""
    , printNumbering
    ]
  ]

printFret :: String
printFret = intercalate "\n" $ mconcat
  [ [ printString str | str <- [ 0..5 ]
    ]
  ,
    [ ""
    , printNumbering
    ]
  ]

fretImage :: Image
fretImage = vertCat
  [ string defAttr $ printString 0
  , string defAttr $ printString 7
  , string defAttr $ printString 3
  , string defAttr $ printString 10
  , string defAttr $ printString 5
  , string defAttr $ printString 0
  , string defAttr $ ""
  , string defAttr $ printNumbering
  ]

main :: IO ()
main = putStrLn printFret

-- testTerminal :: IO ()
-- testTerminal = do
--   putStr requestMouseEvents
--   withTerminal $ runTerminalT $ do
--     putTextLn "Hello there, please press a button!"
--     flush
--     ev <- awaitEvent
--     putStringLn $ "Event was " ++ show ev
--     flush

main2 = do
    let cfg = defaultConfig
         { mouseMode = Just True
         }
    vty <- mkVty cfg
    -- let line0 = string (defAttr ` withForeColor ` green) "first line2\nblbla"
    --     line1 = string (defAttr ` withBackColor ` blue) "second line"
    --     img = line0 <-> line1
    --     pic = picForImage img
    update vty $ picForImage $ markFretImage 0 5
    e <- nextEvent vty
    shutdown vty
    print ("Last event was: " ++ show e)
