module ExpenSieve.Symmantics.Transactions where

import           ExpenSieve.Data.TransactionGroup (NewTransactionGroup,
                                                   TransactionGroup)

import           Control.Monad.IO.Class
import           ExpenSieve.Data.Transaction      (NewTransaction, Transaction)


class Transactions a where
  -- ^ Create a new transaction group.
  createTransactionGroup :: MonadIO m => a -> NewTransactionGroup -> m TransactionGroup

  -- ^ Return a list of all transaction groups.
  findAllTransactionGroups :: MonadIO m => a -> m [TransactionGroup]

  -- ^ Create a new transaction in the specified transaction group.
  createTransaction :: MonadIO m => a -> Int -> NewTransaction -> m Transaction

  -- ^ Return a list of all transactions in the given group.
  findAllTransactionsInGroup :: MonadIO m => a -> Int -> m [Transaction]
