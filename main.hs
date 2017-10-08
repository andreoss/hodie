module Main where

import System.Environment
import System.Exit

import Data.Char.Core
import Data.Char.Number.Roman
import Data.List as L
import Data.Text as T
import Data.Time
import Text.Numeral.Roman as R

romanUnicode :: Integer -> String
romanUnicode x = T.unpack s
    where (Just s) = romanNumber Subtractive Ligate x

romanAscii :: Integer -> String
romanAscii = R.toRoman

display :: Num a => (a -> String) -> LocalTime -> [Char]
display f t = L.intercalate "⋅" $ L.map f [fromIntegral d, fromIntegral m, fromIntegral y]
    where (y, m, d) = toGregorian $ localDay t


getDate :: Num a => (a -> String) -> IO String
getDate f = display f . zonedTimeToLocalTime <$> getZonedTime

usage :: IO ()
usage = putStrLn "Usage: hodie [-u]"

parse :: [String] -> IO String
parse ["-u"]     = getDate romanUnicode
parse []         = getDate romanAscii
parse  _         = usage >> exitWith (ExitFailure 2)

main :: IO ()
main = getArgs >>= parse >>= putStrLn
