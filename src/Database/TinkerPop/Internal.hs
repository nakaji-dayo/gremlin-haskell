module Database.TinkerPop.Internal where
import Data.Char (toLower)


lowerFirst :: String -> String
lowerFirst (x:xs) = (toLower x):xs
lowerFirst [] = []

inStatus2xx :: Int -> Bool
inStatus2xx x =  (x `quot` 100) == 2
