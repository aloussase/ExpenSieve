module ExpenSieve.Api (mkApp) where

import           ExpenSieve.Api.Users                 (UsersApi, userServer)

import           ExpenSieve.Symmantics.Users

import           ExpenSieve.Api.Auth
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Throttle
import           Servant
import           Servant.Auth.Server
import           System.Clock

type Api auths = "api" :> (UsersApi '[Cookie] :<|> AuthApi)

api :: Proxy (Api auths)
api = Proxy

server :: Users users => users -> JWTSettings -> CookieSettings -> Server (Api auths)
server users jwtCfg cookieCfg = userServer users :<|> authServer users jwtCfg cookieCfg

getThrottleSettings :: ThrottleSettings
getThrottleSettings = defaultThrottleSettings $ TimeSpec 5 0

-- TODO: Add swagger support
mkApp :: (Users users) => users -> IO Application
mkApp users = do
  th <- initThrottler getThrottleSettings
  myKey <- generateKey

  let jwtConfig = defaultJWTSettings myKey
  let cfg = defaultCookieSettings :. jwtConfig :. EmptyContext

  return
    . throttle th
    . logStdout
    . serveWithContext api cfg $ server users jwtConfig defaultCookieSettings
