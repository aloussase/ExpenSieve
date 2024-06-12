module ExpenSieve.Api.Auth (AuthApi, authServer) where

import           Control.Monad.IO.Class

import           ExpenSieve.Data.User        (User (userPassword),
                                              UserCredentials (userCredentialsEmail, userCredentialsPassword),
                                              UserIdentity (UserIdentity))
import           ExpenSieve.Symmantics.Users (Users (getUserByEmail))


import           Crypto.KDF.BCrypt           (validatePassword)
import qualified Data.Text.Encoding          as TE
import           Servant
import           Servant.Auth.Server         (CookieSettings, JWTSettings,
                                              SetCookie, acceptLogin)


type CookieHeadersResponse = Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent

type AuthApi = "auth"
  :> "login"
  :> ReqBody '[JSON] UserCredentials
  :> Post '[JSON] CookieHeadersResponse

authServer :: Users users => users -> JWTSettings -> CookieSettings -> Server AuthApi
authServer = login

login :: Users a => a -> JWTSettings -> CookieSettings -> UserCredentials -> Handler CookieHeadersResponse
login users jwtCfg cookieCfg creds = do
  maybeUser <- liftIO $ getUserByEmail users (userCredentialsEmail creds)
  case maybeUser of
    Nothing -> throwError err404
    Just user -> do
      if validatePassword (TE.encodeUtf8 $ userCredentialsPassword creds) (TE.encodeUtf8 $ userPassword user)
      then do
        mApplyCookies <- liftIO $ acceptLogin cookieCfg jwtCfg (UserIdentity $ userCredentialsEmail creds)
        maybe (throwError err401) (\apCookies -> return $ apCookies NoContent) mApplyCookies
      else throwError err401

