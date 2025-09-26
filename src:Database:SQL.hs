{-# LANGUAGE OverloadedStrings #-}

module Database.SQL where

import Database.Types
import qualified Data.Map as Map
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T

-- SQL database state
data SQLDatabase = SQLDatabase
    { sqlTables :: Map TableName Table
    } deriving (Show)

emptySQLDatabase :: SQLDatabase
emptySQLDatabase = SQLDatabase Map.empty

-- SQL monad
newtype SQL a = SQL { runSQL :: State SQLDatabase a }
    deriving (Functor, Applicative, Monad, MonadState SQLDatabase)

-- SQL operations
createTable :: TableName -> Map ColumnName ColumnType -> SQL ()
createTable name schema = do
    db <- get
    when (Map.member name (sqlTables db)) $
        error $ "Table " ++ T.unpack name ++ " already exists"
    let newTable = Table name schema Map.empty 1
    put $ db { sqlTables = Map.insert name newTable (sqlTables db) }

insertInto :: TableName -> Row -> SQL Key
insertInto tableName row = do
    db <- get
    case Map.lookup tableName (sqlTables db) of
        Nothing -> error $ "Table " ++ T.unpack tableName ++ " not found"
        Just table -> do
            let key = tableNextKey table
            let newTable = table 
                    { tableData = Map.insert key row (tableData table)
                    , tableNextKey = key + 1
                    }
            put $ db { sqlTables = Map.insert tableName newTable (sqlTables db) }
            return key

selectFrom :: TableName -> SQL [Row]
selectFrom tableName = do
    db <- get
    case Map.lookup tableName (sqlTables db) of
        Nothing -> error $ "Table " ++ T.unpack tableName ++ " not found"
        Just table -> return $ Map.elems (tableData table)

selectWhere :: TableName -> (Row -> Bool) -> SQL [Row]
selectWhere tableName predicate = do
    db <- get
    case Map.lookup tableName (sqlTables db) of
        Nothing -> error $ "Table " ++ T.unpack tableName ++ " not found"
        Just table -> return $ filter predicate (Map.elems (tableData table))

-- Run SQL operations
runSQLDatabase :: SQL a -> (a, SQLDatabase)
runSQLDatabase = flip runState emptySQLDatabase . runSQL

when :: Bool -> IO () -> IO ()
when condition action = if condition then action else return ()