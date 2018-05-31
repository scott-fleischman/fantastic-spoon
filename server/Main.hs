{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Database.Persist as Persist
import qualified Database.Persist.Postgresql as Persist.Postgresql
import qualified Database.Persist.TH as Persist.TH
import           GHC.Generics (Generic)
import qualified Network.HTTP.Types as HTTP.Types
import qualified System.Environment as Environment
import qualified Text.Read
import qualified Yesod
import           Yesod.Static (Static)
import qualified Yesod.Static

Persist.TH.share
  [Persist.TH.mkPersist Persist.TH.sqlSettings, Persist.TH.mkMigrate "migrateAll"]
  [Persist.TH.persistLowerCase|
User
  name Text
  password Text
  age Int
  UniqueName name
  deriving Show
  deriving Generic
|]

instance Aeson.ToJSON User
instance Aeson.FromJSON User

data FantasticSpoon = FantasticSpoon
  { appStatic :: Static
  , appConnectionPool :: Persist.Postgresql.ConnectionPool
  }

Yesod.mkYesod "FantasticSpoon" [Yesod.parseRoutes|
/ HomeR GET
/user CreateUserR POST
/static StaticR Static appStatic
|]

instance Yesod.Yesod FantasticSpoon

instance Yesod.YesodPersist FantasticSpoon where
  type YesodPersistBackend FantasticSpoon = Persist.Postgresql.SqlBackend

  runDB action = do
    app <- Yesod.getYesod
    let pool = appConnectionPool app
    Persist.Postgresql.runSqlPool action pool

-- List all people in the database
getHomeR :: Yesod.HandlerFor FantasticSpoon Yesod.Html
getHomeR = do
  users <- Yesod.runDB $ Persist.selectList [] [Persist.Asc UserAge]
  let userAges = fmap (userAge . Persist.entityVal) users
  Yesod.defaultLayout
    [Yesod.whamlet|
      <ul>
        $forall age <- userAges
          <li>#{age}
    |]

postCreateUserR :: Yesod.HandlerFor FantasticSpoon Aeson.Value
postCreateUserR = do
  userResult <- Yesod.parseCheckJsonBody
  user :: User <-
    case userResult of
      Aeson.Error err -> do
        $(Monad.Logger.logInfo) $ "postCreateUserR: invalid request " <> Text.pack err
        Yesod.sendResponseStatus HTTP.Types.status400 $ Aeson.object ["error" .= Aeson.String "Invalid request"]
      Aeson.Success x -> return x
  userId <- Yesod.runDB $ Persist.insert user
  return $ Aeson.object ["userId" .= Aeson.toJSON userId]

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

      let embeddedStatic = $(Yesod.Static.embed "../static")
      let
        application =
          FantasticSpoon
          { appStatic = embeddedStatic
          , appConnectionPool = pool
          }
      Yesod.warp port application
