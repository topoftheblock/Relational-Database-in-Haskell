# Haskell Database

A functional, type-safe in-memory database implemented in Haskell featuring:

- **Basic CRUD operations**
- **Transactional support** with rollback capabilities
- **SQL-like interface** for complex queries
- **Type-safe schema definitions**

## Features

- 🏗️ Pure functional implementation
- 🔄 ACID-like transactions
- 📋 SQL-inspired query interface
- 🛡️ Type-safe operations
- ⚡ In-memory performance

## Installation

### Using Stack

```bash
git clone https://github.com/your-username/haskell-database
cd haskell-database
stack build
Using Cabal

bash
cabal update
cabal build
Usage

Basic Operations

haskell
import Database.Core

main :: IO ()
main = do
  let (result, finalDB) = runDatabase $ do
        key <- insertAuto "Hello, World!"
        value <- get key
        return value
  
  print result  -- Just "Hello, World!"
Transactions

haskell
import Database.Transactional

main :: IO ()
main = do
  case executeTransaction $ do
          key <- insertAutoT "Transaction safe"
          value <- getT key
          return value of
    Left err -> print err
    Right (result, _) -> print result
SQL-like Interface

haskell
import Database.SQL
import qualified Data.Map as Map

main :: IO ()
main = do
  let (result, _) = runSQLDatabase $ do
        createTable "users" $ Map.fromList 
            [("name", TextType), ("age", IntType)]
        
        insertInto "users" $ Map.fromList
            [("name", "Alice"), ("age", "30")]
        
        selectFrom "users"
  
  print result

