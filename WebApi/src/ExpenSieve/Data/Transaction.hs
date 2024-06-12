module ExpenSieve.Data.Transaction where

import           Data.Aeson
import           Data.Char            (toLower)
import           Data.Text            (Text)
import           Deriving.Aeson       (ConstructorTagModifier,
                                       StringModifier (..))
import           Deriving.Aeson.Stock

data ToLower

instance StringModifier ToLower where
  getStringModifier ""     = ""
  getStringModifier (c:cs) = toLower c : cs

data TransactionKind = Income | Expense
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier ToLower] TransactionKind

data Transaction = Transaction
  { transactionId          :: !Int
  -- ^ The transaction id
  , transactionDescription :: !Text
  -- ^ A string describing this transaction
  , transactionAmount      :: !Double
  -- ^ Stonks involved in the transaction
  , transactionKind        :: !TransactionKind
  -- ^ Is this an income or an expense?
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "transaction" Transaction
