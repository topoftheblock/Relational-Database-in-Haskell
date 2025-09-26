{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Types where

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T

-- Common type definitions
type Key = Int
type Value = Text
type TableName = Text
type ColumnName = Text

-- Basic record
data Record = Record
    { recordKey :: Key
    , recordValue :: Value
    } deriving (Show, Eq, Ord)

-- Row representation for SQL-like interface
type Row = Map ColumnName Value

-- Table definition
data Table = Table
    { tableName :: TableName
    , tableSchema :: Map ColumnName ColumnType
    , tableData :: Map Key Row
    , tableNextKey :: Key
    } deriving (Show)

data ColumnType
    = TextType
    | IntType
    | BoolType
    deriving (Show, Eq, Enum)

-- Database errors
data DBError
    = KeyNotFound Key
    | KeyExists Key
    | TableNotFound TableName
    | ColumnNotFound ColumnName
    | InvalidType ColumnName ColumnType Value
    | InvalidOperation Text
    deriving (Show, Eq)