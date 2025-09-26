{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Core where

import Database.Types
import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe (isJust)

-- Basic database state
data Database = Database
    { records :: Map Key Value
    , nextKey :: Key
    } deriving (Show)

-- Database monad
newtype DBM a = DBM { runDBM :: State Database a }
    deriving (Functor, Applicative, Monad, MonadState Database)

-- Initial database
emptyDB :: Database
emptyDB = Database Map.empty 1

-- Core operations
insert :: Key -> Value -> DBM Bool
insert key value = do
    db <- get
    let existing = Map.lookup key (records db)
    case existing of
        Just _  -> return False
        Nothing -> do
            put $ db { records = Map.insert key value (records db) }
            return True

insertAuto :: Value -> DBM Key
insertAuto value = do
    db <- get
    let key = nextKey db
    put $ db 
        { records = Map.insert key value (records db)
        , nextKey = key + 1
        }
    return key

update :: Key -> Value -> DBM Bool
update key value = do
    db <- get
    let existing = Map.lookup key (records db)
    case existing of
        Just _  -> do
            put $ db { records = Map.insert key value (records db) }
            return True
        Nothing -> return False

delete :: Key -> DBM Bool
delete key = do
    db <- get
    let existing = Map.lookup key (records db)
    case existing of
        Just _  -> do
            put $ db { records = Map.delete key (records db) }
            return True
        Nothing -> return False

get :: Key -> DBM (Maybe Value)
get key = do
    db <- get
    return $ Map.lookup key (records db)

getAll :: DBM [(Key, Value)]
getAll = do
    db <- get
    return $ Map.toList (records db)

-- Run database operations
runDatabase :: DBM a -> (a, Database)
runDatabase = flip runState emptyDB . runDBM