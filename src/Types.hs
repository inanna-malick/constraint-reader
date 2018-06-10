{-# LANGUAGE DeriveGeneric #-}

module Types
    ( Todo(..)
    ) where


------------------------------------------------------------------------------
import           Data.Aeson
import           GHC.Generics
------------------------------------------------------------------------------

data Todo = Todo
  { description :: String
  , body :: String
  } deriving (Generic, Show, Eq, Ord)

instance ToJSON Todo where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Todo
    -- No need to provide a parseJSON implementation.
