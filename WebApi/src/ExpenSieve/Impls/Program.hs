module ExpenSieve.Impls.Program (Program, mkProgram) where

import           Configuration.Dotenv               (defaultConfig, loadFile,
                                                     onMissingFile)
import           Control.Monad.IO.Class
import           Crypto.KDF.BCrypt                  (hashPassword)
import           Data.Maybe                         (listToMaybe)
import           Data.Pool
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as TE
import           Database.PostgreSQL.Simple
import           System.Environment                 (getEnv)

import           ExpenSieve.Data.TransactionGroup   (NewTransactionGroup,
                                                     TransactionGroup)
import           ExpenSieve.Data.User               (NewUser (..), User (..))
import           ExpenSieve.Error
import           ExpenSieve.Symmantics.Transactions
import           ExpenSieve.Symmantics.Users


newtype Program = Program { programDb :: Pool Connection }

loadDotenv :: IO ()
loadDotenv = onMissingFile (loadFile defaultConfig) (putStrLn ".env file not loaded")

loadPgConnectInfo :: IO ConnectInfo
loadPgConnectInfo = do
  pgUser <- getEnv "POSTGRES_USER"
  pgPassword <- getEnv "POSTGRES_PASSWORD"
  pgDatabase <- getEnv "POSTGRES_DATABASE"
  return $ defaultConnectInfo
    { connectUser = pgUser
    , connectPassword = pgPassword
    , connectDatabase = pgDatabase
    }

-- ^ Create a new instance of the Program.
mkProgram :: IO Program
mkProgram = do
  loadDotenv

  connectInfo <- loadPgConnectInfo

  let poolConfig = defaultPoolConfig (connect connectInfo) close 1 5
  pool <- newPool poolConfig

  return $ Program pool


instance Users Program where
  createUser = createUser'
  getUserById = getUserById'
  getUserByEmail = getUserByEmail'

instance Transactions Program where
  createTransactionGroup = createTransactionGroup'
  findAllTransactionGroups = findAllTransactionGroups'
  createTransaction = createTransaction'
  findAllTransactionsInGroup = findAllTransactionsInGroup'


createUser' :: MonadIO m => Program -> NewUser -> m (Either Error User)
createUser' program newUser = liftIO $
  withResource (programDb program) $ \conn -> do
    es :: [Only Int] <- query conn "select 1 from users where email = ?" (Only $ newUserEmail newUser)
    case es of
      es' | not (null es') -> return $ Left $ Conflict "A user with that email already exists"
      _ -> do
        hashedPassword <- hashPassword 12 (TE.encodeUtf8 $ newUserPassword newUser)
        let newUser' = newUser { newUserPassword = TE.decodeUtf8 hashedPassword }
        let q = "insert into users (email, password, first_name, last_name) values (?, ?, ?, ?) returning *"
        rs <- query conn q newUser'
        return $ maybe (Left $ Unexpected "Failed to create the user") Right (listToMaybe rs)

getUserById' :: MonadIO m => Program -> Int -> m (Maybe User)
getUserById' program uid = liftIO $
  withResource (programDb program) $ \conn -> do
    rs <- query conn "select * from users where id = ?" (Only uid)
    return $ listToMaybe rs

getUserByEmail' :: MonadIO m => Program -> Text -> m (Maybe User)
getUserByEmail' program email = liftIO $
  withResource (programDb program) $ \conn -> do
    rs <- query conn "select * from users where email = ?" (Only email)
    return $ listToMaybe rs

createTransactionGroup' :: MonadIO m => Program -> NewTransactionGroup -> m TransactionGroup
createTransactionGroup' = undefined

findAllTransactionGroups' = undefined

createTransaction' = undefined

findAllTransactionsInGroup' = undefined

