module ExpenSieve.Symmantics.Users where

import           ExpenSieve.Data.User
import           ExpenSieve.Error

import           Control.Monad.IO.Class
import           Data.Text              (Text)

class Users a where
  -- ^ Create a new user.
  createUser :: MonadIO m => a -> NewUser -> m (Either Error User)

  -- ^ Get a user by their id.
  getUserById :: MonadIO m => a -> Int -> m (Maybe User)

  -- ^ Get a user by their email.
  getUserByEmail :: MonadIO m => a -> Text -> m (Maybe User)
