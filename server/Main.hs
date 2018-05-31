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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import           Database.Persist ((==.))
import qualified Database.Persist as Persist
import qualified Database.Persist.Postgresql as Persist.Postgresql
import qualified Database.Persist.TH as Persist.TH
import qualified System.Environment as Environment

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

main :: IO ()
main = do
  connectionString <- Environment.getEnv "DATABASE_URL"
  let connectionStringBytes = (Text.Encoding.encodeUtf8 . Text.pack) connectionString
  Monad.Logger.runStderrLoggingT $ Persist.Postgresql.withPostgresqlPool connectionStringBytes 10 $ \pool -> Monad.IO.Class.liftIO $ do
    flip Persist.Postgresql.runSqlPersistMPool pool $ do
      Persist.Postgresql.runMigration migrateAll

      userId <- Persist.insert $ User "scott" "pwd" 26
      scott <- Persist.selectList [UserId ==. userId] []
      Monad.IO.Class.liftIO $ print scott
      Persist.delete userId
