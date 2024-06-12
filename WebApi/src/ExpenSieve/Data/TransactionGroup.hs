module ExpenSieve.Data.TransactionGroup where

import           Data.Aeson
import           Data.Text            (Text)
import           Data.Time.Clock
import           Deriving.Aeson.Stock


--- A @TransactionGroup@ groups transactions in a time span with a given name.
data TransactionGroup = TransactionGroup
  { transactionGroupId        :: !Int
  , transactionGroupStartDate :: !UTCTime
  , transactionGroupEndDate   :: !UTCTime
  , transactionGroupName      :: !Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "transactionGroup" TransactionGroup
