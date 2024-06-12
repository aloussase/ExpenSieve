module ExpenSieve.Api.Users (UsersApi, userServer) where

import           ExpenSieve.Data.User        (NewUser, User (..), UserIdentity,
                                              UserNoPassword (..))
import           ExpenSieve.Error
import           ExpenSieve.Symmantics.Users

import qualified Data.ByteString             as BS
import qualified Data.Text.Encoding          as TE
import           Servant
import           Servant.Auth
import           Servant.Auth.Server         (AuthResult (Authenticated),
                                              ThrowAll (throwAll))


type UsersApi auths = "users" :>
  ( ReqBody '[JSON] NewUser :> Post '[JSON] UserNoPassword
  :<|> Auth auths UserIdentity :> Capture "id" Int :> Get '[JSON] UserNoPassword
  )

userServer :: Users users => users -> Server (UsersApi auths)
userServer users = createUser' users :<|> protectedUsers
  where
    protectedUsers (Authenticated identity) = getUserById' users
    protectedUsers _                        = throwAll err401

userToUserNoPassword :: User -> UserNoPassword
userToUserNoPassword user = UserNoPassword
  { userNoPasswordId = userId user
  , userNoPasswordEmail = userEmail user
  , userNoPasswordFirstName = userFirstName user
  , userNoPasswordLastName = userLastName user
  }

getUserById' :: Users a => a -> Int -> Handler UserNoPassword
getUserById' users uid = do
  maybeUser <- getUserById users uid
  maybe
    (throwError err404 { errBody = "User with that id not found" })
    (pure . userToUserNoPassword)
    maybeUser

createUser' :: Users a => a -> NewUser -> Handler UserNoPassword
createUser' users newUser = do
  result <- createUser users newUser
  case result of
    Right user          -> return $ userToUserNoPassword user
    Left (Conflict msg) -> throwError err409 { errBody =  BS.fromStrict $ TE.encodeUtf8 msg }
    _                   -> throwError err500
