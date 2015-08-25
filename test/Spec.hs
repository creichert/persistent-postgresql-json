
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import qualified Data.Aeson                       as A
import           Data.Text                        (Text)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           System.Log.FastLogger

import           Test.Hspec
import           Test.HUnit                       (Assertion, assertBool,
                                                   assertFailure, (@=?), (@?=))

import           Database.Persist.Postgresql.Json


share [ mkPersist sqlSettings
      , mkMigrate "migration"
      ] [persistLowerCase|
Foo
  json  Jsonb
  deriving Show Eq

Bar
  json  Json
  deriving Show Eq
|]


db :: SqlPersistT (LoggingT (ResourceT IO)) () -> Assertion
db actions = runResourceT $ runConn (actions >> cleanDB)
  where
    cleanDB = deleteWhere ([] :: [Filter Foo])
              >> deleteWhere ([] :: [Filter Bar])


assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual preface expected actual =
    unless (actual /= expected) (assertFailure msg)
  where
    msg = (if null preface then "" else preface ++ "\n") ++
          "expected: " ++ show expected ++ "\n to not equal: " ++ show actual


runConn f = flip runLoggingT logit $
    withPostgresqlPool connstr 1 $ runSqlPool f
  where
    connstr = "host=localhost port=5432 password=test user=test dbname=test"
    logit _ _ _ s = print $ fromLogStr s

io = liftIO

infix 1 @/= --, /=@
infix 1 @==, ==@

(@/=), (@==), (==@) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
actual   @/= expected = io $ assertNotEqual "" expected actual
expected @== actual   = io $ expected @?= actual
expected ==@ actual   = io $ expected @=? actual


main :: IO ()
main = do
  runResourceT $ runConn $ runMigration migration
  hspec specs

pjsonb = Jsonb $ A.object [ "key" A..= ("value"::Text) ]
pjson  = Json  $ A.object [ "key" A..= ("value"::Text)
                          , "key2" A..= A.object [ "nested" A..= (""::Text) ]
                          ]


specs :: Spec
specs = do
  describe "persistent-postgresql json" $ do
    it "inserts json" $ db $ do
      let bar = Bar pjson
      k <- insert bar
      bars <- selectList ([] :: [Filter Bar]) []
      bars @== [Entity k bar]

  describe "persistent-postgresql jsonb" $ do
    it "inserts jsonb" $ db $ do
      let foo = Foo pjsonb
      k <- insert foo
      foos <- selectList ([] :: [Filter Foo]) []
      foos @== [Entity k foo]

    it "postgresql jsonb operator @>" $ db $ do
      let foo = Foo pjsonb
      k <- insert foo
      foos <- selectList [FooJson @>. A.object [ "key" A..= ("value"::Text)] ] []
      foos @== [Entity k foo]
      foos' <- selectList [FooJson @>. A.object [ "ky" A..= ("alue"::Text)] ] []
      foos' @== []

    it "postgresql jsonb operator <@" $ db $ do
      let foo = Foo pjsonb
      k <- insert foo
      foos <- selectList [FooJson <@. A.object [ "key" A..= ("value"::Text)] ] []
      foos @== [Entity k foo]

    -- not working to do query placeholder syntax

    -- it "?" $ db $ do
    --   let foo = Foo pjsonb
    --   k <- insert foo
    --   foos <- selectList [FooJson ?. "key"] []

    -- it "?|" $ db $ do
    --   let foo = Foo pjsonb
    --   k <- insert foo
    --   foos <- selectList [FooJson ?|. [ "key" ]] []
    --   foos @== [Entity k foo]

    -- it "?&" $ db $ do
    --   let foo = Foo pjsonb
    --   k <- insert foo
    --   foos <- selectList [FooJson ?&. [ "key", "key2" ]] []
    --   foos @== [Entity k foo]
