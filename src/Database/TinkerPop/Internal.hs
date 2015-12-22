module Database.TinkerPop.Internal where
import Data.Char (toLower)


lowerFirst :: String -> String
lowerFirst (x:xs) = (toLower x):xs
lowerFirst [] = []
