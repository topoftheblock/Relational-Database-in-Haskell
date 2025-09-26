module Database.CoreSpec (spec) where

import Test.Hspec
import Database.Core

spec :: Spec
spec = describe "Database.Core" $ do
  describe "insert" $ do
    it "inserts a new record" $ do
      let (result, db) = runDatabase $ do
            insert 1 "test"
            get 1
      result `shouldBe` Just "test"
  
  describe "update" $ do
    it "updates an existing record" $ do
      let (result, _) = runDatabase $ do
            insert 1 "old"
            update 1 "new"
            get 1
      result `shouldBe` Just "new"