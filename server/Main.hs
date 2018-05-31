{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wno-unused-top-binds #-}

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Database.Persist ((==.))
import qualified Database.Persist as Persist
import qualified Database.Persist.Sqlite as Persist.Sqlite
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

main :: IO ()
main = Persist.Sqlite.runSqlite ":memory:" $ do
  Persist.Sqlite.runMigration migrateAll
  _ <- Persist.insert $ User "scott" "pwd" 26
  scott <- Persist.selectList [UserName ==. "scott"] []
  liftIO $ print scott
