module ExpenSieve.Data.Transaction where

import           Data.Aeson
import           Data.Char                          (toLower)
import           Data.Text                          (Text)
import           Database.PostgreSQL.Simple         (ToRow)
import           Database.PostgreSQL.Simple.ToField (ToField (..), toJSONField)
import           Deriving.Aeson                     (ConstructorTagModifier,
                                                     StringModifier (..))
import           Deriving.Aeson.Stock

data ToLower

instance StringModifier ToLower where
  getStringModifier ""     = ""
  getStringModifier (c:cs) = toLower c : cs

data TransactionKind = Income | Expense
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier ToLower] TransactionKind

instance ToField TransactionKind where
   toField = toJSONField

data NewTransaction = NewTransaction
  { newTransactionDescription :: !Text
  , newTransactionAmount      :: !Double
  , newTransactionKind        :: !TransactionKind
  }
  deriving stock (Show, Generic)
  deriving anyclass ToRow
  deriving FromJSON via PrefixedSnake "newTransaction" NewTransaction

data Transaction = Transaction
  { transactionId          :: !Int
  -- ^ The transaction id
  , transactionGroupId     :: !Int
  -- ^ The id of the group this transaction belongs to.
  , transactionDescription :: !Text
  -- ^ A string describing this transaction
  , transactionAmount      :: !Double
  -- ^ Stonks involved in the transaction
  , transactionKind        :: !TransactionKind
  -- ^ Is this an income or an expense?
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via PrefixedSnake "transaction" Transaction
