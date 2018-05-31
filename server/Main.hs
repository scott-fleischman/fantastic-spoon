{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wno-unused-top-binds #-}

import           Control.Monad.IO.Class as Monad.IO.Class
import qualified Control.Monad.Logger as Monad.Logger
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Database.Persist ((==.))
import qualified Database.Persist as Persist
import qualified Database.Persist.Postgresql as Persist.Postgresql
import qualified Database.Persist.TH as Persist.TH

Persist.TH.share
  [Persist.TH.mkPersist Persist.TH.sqlSettings, Persist.TH.mkMigrate "migrateAll"]
  [Persist.TH.persistLowerCase|
User
  name Text
  password ByteString
  age Int
  UniqueName name
  deriving Show
|]

connStr :: Persist.Postgresql.ConnectionString
connStr = "host=localhost dbname=test user=test password=test port=5432"

main :: IO ()
main = Monad.Logger.runStderrLoggingT $ Persist.Postgresql.withPostgresqlPool connStr 10 $ \pool -> Monad.IO.Class.liftIO $ do
  flip Persist.Postgresql.runSqlPersistMPool pool $ do
    Persist.Postgresql.runMigration migrateAll

    userId <- Persist.insert $ User "scott" "pwd" 26
    scott <- Persist.selectList [UserId ==. userId] []
    Monad.IO.Class.liftIO $ print scott
    Persist.delete userId
