module ExpenSieve.Data.TransactionGroup where

import           Data.Aeson
import           Data.Text                  (Text)
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Deriving.Aeson.Stock


data NewTransactionGroup = NewTransactionGroup
  { newTransactionGroupStartDate :: !UTCTime
  , newTransactionGroupEndDate   :: !UTCTime
  , newTransactionGroupName      :: !Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToRow)
  deriving (FromJSON) via PrefixedSnake "newTransactionGroup" NewTransactionGroup

--- A @TransactionGroup@ groups transactions in a time span with a given name.
data TransactionGroup = TransactionGroup
  { transactionGroupId        :: !Int
  , transactionGroupStartDate :: !UTCTime
  , transactionGroupEndDate   :: !UTCTime
  , transactionGroupName      :: !Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow)
  deriving (FromJSON, ToJSON) via PrefixedSnake "transactionGroup" TransactionGroup
