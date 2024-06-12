module ExpenSieve.Data.User where

import           Data.Aeson
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple
import           Deriving.Aeson.Stock
import           Servant.Auth.JWT

newtype UserIdentity = UserIdentity Text
  deriving stock (Show)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass (FromJWT, ToJWT)

data UserCredentials = UserCredentials
  { userCredentialsEmail    :: !Text
  , userCredentialsPassword :: !Text
  }
  deriving stock (Show, Generic)
  deriving FromJSON via PrefixedSnake "userCredentials" UserCredentials

data NewUser = NewUser
  { newUserEmail     :: !Text
  , newUserPassword  :: !Text
  , newUserFirstName :: !Text
  , newUserLastName  :: !Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToRow)
  deriving FromJSON via PrefixedSnake "newUser" NewUser

data User = User
  { userId        :: !Int
  , userEmail     :: !Text
  , userPassword  :: !Text
  , userFirstName :: !Text
  , userLastName  :: !Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow)
  deriving FromJSON via PrefixedSnake "user" User

data UserNoPassword = UserNoPassword
  { userNoPasswordId        :: !Int
  , userNoPasswordEmail     :: !Text
  , userNoPasswordFirstName :: !Text
  , userNoPasswordLastName  :: !Text
  }
  deriving stock (Show, Generic)
  deriving ToJSON via PrefixedSnake "userNoPassword" UserNoPassword
