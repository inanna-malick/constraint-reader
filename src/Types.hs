
module Types
    ( Todo(..)
    ) where

data Todo = Todo { description :: String, body :: String } deriving (Show, Eq, Ord)
