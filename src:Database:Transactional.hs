module Database.Transactional where

import Database.Types
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

-- Transactional database state
data TDatabase = TDatabase
    { tRecords :: Map Key Value
    , tNextKey :: Key
    } deriving (Show)

-- Transaction monad with error handling
newtype Transaction a = Transaction 
    { runTransaction :: ExceptT DBError (State TDatabase) a }
    deriving (Functor, Applicative, Monad, MonadState TDatabase, MonadError DBError)

emptyTDatabase :: TDatabase
emptyTDatabase = TDatabase Map.empty 1

-- Transactional operations
insertT :: Key -> Value -> Transaction ()
insertT key value = do
    db <- get
    case Map.lookup key (tRecords db) of
        Just _  -> throwError (KeyExists key)
        Nothing -> put $ db { tRecords = Map.insert key value (tRecords db) }

insertAutoT :: Value -> Transaction Key
insertAutoT value = do
    db <- get
    let key = tNextKey db
    put $ db 
        { tRecords = Map.insert key value (tRecords db)
        , tNextKey = key + 1
        }
    return key

updateT :: Key -> Value -> Transaction ()
updateT key value = do
    db <- get
    case Map.lookup key (tRecords db) of
        Just _  -> put $ db { tRecords = Map.insert key value (tRecords db) }
        Nothing -> throwError (KeyNotFound key)

deleteT :: Key -> Transaction ()
deleteT key = do
    db <- get
    case Map.lookup key (tRecords db) of
        Just _  -> put $ db { tRecords = Map.delete key (tRecords db) }
        Nothing -> throwError (KeyNotFound key)

getT :: Key -> Transaction Value
getT key = do
    db <- get
    case Map.lookup key (tRecords db) of
        Just value -> return value
        Nothing -> throwError (KeyNotFound key)

getAllT :: Transaction [(Key, Value)]
getAllT = do
    db <- get
    return $ Map.toList (tRecords db)

-- Execute transaction
executeTransaction :: Transaction a -> Either DBError (a, TDatabase)
executeTransaction transaction = 
    runState (runExceptT (runTransaction transaction)) emptyTDatabase