{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wno-unused-top-binds #-}

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Control.Monad.IO.Class as Monad.IO.Class
import qualified Control.Monad.Logger as Monad.Logger
import qualified Control.Monad.Trans.Resource as Monad.Trans.Resource
import           Data.ByteString (ByteString)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Database.Persist as Persist
import qualified Database.Persist.Postgresql as Persist.Postgresql
import qualified Database.Persist.TH as Persist.TH
import qualified System.Environment as Environment
import qualified Text.Read
import qualified Yesod

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

data FantasticSpoon = FantasticSpoon Persist.Postgresql.ConnectionPool

Yesod.mkYesod "FantasticSpoon" [Yesod.parseRoutes|
/ HomeR GET
/user/#UserId UserR GET
/user CreateUserR POST
|]
-- /static StaticR Static getStatic

instance Yesod.Yesod FantasticSpoon

instance Yesod.YesodPersist FantasticSpoon where
  type YesodPersistBackend FantasticSpoon = Persist.Postgresql.SqlBackend

  runDB action = do
    FantasticSpoon pool <- Yesod.getYesod
    Persist.Postgresql.runSqlPool action pool

-- List all people in the database
getHomeR :: Yesod.HandlerFor FantasticSpoon Yesod.Html
getHomeR = do
  users <- Yesod.runDB $ Persist.selectList [] [Persist.Asc UserAge]
  Yesod.defaultLayout
    [Yesod.whamlet|
      <ul>
        $forall Persist.Entity userId user <- users
          <li>
            <a href=@{UserR userId}>#{userName user}
    |]

getUserR :: UserId -> Yesod.HandlerFor FantasticSpoon String
getUserR userId = do
  user <- Yesod.runDB $ Yesod.get404 userId
  return $ show user

postCreateUserR :: Yesod.HandlerFor FantasticSpoon Aeson.Value
postCreateUserR = do
  return $ Aeson.object ["message" .= Aeson.String "Post successful!"]

main :: IO ()
main = do
  connectionString    <- fmap (Text.Encoding.encodeUtf8 . Text.pack)  $ Environment.getEnv "DATABASE_URL"
  openConnectionCount <- fmap (Text.Read.read @Int)                   $ Environment.getEnv "DATABASE_CONNECTION_COUNT"
  port                <- fmap (Text.Read.read @Int)                   $ Environment.getEnv "PORT"
  Monad.Logger.runStderrLoggingT $ do
    $(Monad.Logger.logInfo) $ "Open connection count: " <> (Text.pack . show) openConnectionCount
    $(Monad.Logger.logInfo) $ "Port: " <> (Text.pack . show) port

    Persist.Postgresql.withPostgresqlPool connectionString openConnectionCount $ \pool -> Monad.IO.Class.liftIO $ do
      let
        runMigrationAction = Persist.Postgresql.runMigration migrateAll
        runMigrationWithPool = Persist.Postgresql.runSqlPool runMigrationAction pool
      Monad.Trans.Resource.runResourceT runMigrationWithPool

      let application = FantasticSpoon pool
      Yesod.warp port application
