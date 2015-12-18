module Database.TinkerPop.Types where
import Data.Char (toLower)

lowerFirst (x:xs) = (toLower x):xs

