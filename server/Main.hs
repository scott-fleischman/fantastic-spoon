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
import qualified Control.Monad.IO.Class as Monad.IO
import qualified Control.Monad.Logger as Monad.Logger
import qualified Control.Monad.Trans.Resource as Monad.Trans.Resource
import qualified Data.String.Interpolate as String.Interpolate
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
import qualified System.Random as Random
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
/populate PopulateUsersR POST
/static StaticR Static appStatic
|]

instance Yesod.Yesod FantasticSpoon

instance Yesod.YesodPersist FantasticSpoon where
  type YesodPersistBackend FantasticSpoon = Persist.Postgresql.SqlBackend

  runDB action = do
    app <- Yesod.getYesod
    let pool = appConnectionPool app
    Persist.Postgresql.runSqlPool action pool

newtype RawHtml = RawHtml String
instance Yesod.ToContent RawHtml where
  toContent (RawHtml htmlText) = Yesod.toContent htmlText
instance Yesod.ToTypedContent RawHtml where
  toTypedContent rawHtml = Yesod.TypedContent Yesod.typeHtml (Yesod.toContent rawHtml)
instance Yesod.HasContentType RawHtml where
  getContentType _ = Yesod.typeHtml

getHomeR :: Yesod.HandlerFor FantasticSpoon Yesod.Html
getHomeR = do
  users <- Yesod.runDB $ Persist.selectList [] [Persist.Asc UserAge]
  let
    userAges = fmap (userAge . Persist.entityVal) users
    userAgesJS =
      Text.concat
        [ "["
        , Text.intercalate "," $ fmap (Text.pack . show) userAges
        , "]"
        ]
    _minAgeJS = Text.pack . show $ minimum userAges
    _maxAgeJS = Text.pack . show $ maximum userAges
  Yesod.sendResponseStatus HTTP.Types.status200 $ RawHtml [String.Interpolate.i|<!DOCTYPE html>
<meta charset="utf-8">
<head>
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
</head>
<body>
<div id="myDiv"></div>
<script>
var userAges = #{userAgesJS};
var trace = {
  x: userAges,
  type: 'histogram',
  marker: {
     color: "rgba(220, 226, 189, 1)",
  },
  name: "All Users",
  xbins: {
    end: 100,
    size: 5, 
    start: 10
  },
  autobinx: false,
  autobiny: true,
};
var layout = {
  bargap: 0.02,
  title: "Ages of Users", 
  xaxis: {title: "Age"},
  yaxis: {title: "Count"}
};
var data = [trace];
Plotly.newPlot('myDiv', data, layout);
</script>
</body>
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

nameList :: [Text]
nameList = ["olivia","ruby","emily","grace","jessica","chloe","sophie","lily","amelia","evie","mia","ella","charlotte","lucy","megan","ellie","isabelle","isabella","hannah","katie","ava","holly","summer","millie","daisy","phoebe","freya","abigail","poppy","erin","emma","molly","imogen","amy","jasmine","isla","scarlett","leah","sophia","elizabeth","eva","brooke","matilda","caitlin","keira","alice","lola","lilly","amber","isabel","lauren","georgia","gracie","eleanor","bethany","madison","amelie","isobel","paige","lacey","sienna","libby","maisie","anna","rebecca","rosie","tia","layla","maya","niamh","zara","sarah","lexi","maddison","alisha","sofia","skye","nicole","lexie","faith","martha","harriet","zoe","eve","julia","aimee","hollie","lydia","evelyn","alexandra","maria","francesca","tilly","florence","alicia","abbie","emilia","courtney","maryam","esme","jack","oliver","thomas","harry","joshua","alfie","charlie","daniel","james","william","samuel","george","joseph","lewis","ethan","mohammed","dylan","benjamin","alexander","jacob","ryan","liam","jake","max","luke","tyler","callum","matthew","jayden","oscar","archie","adam","riley","harvey","harrison","lucas","muhammad","henry","isaac","leo","connor","edward","finley","logan","noah","cameron","alex","owen","rhys","nathan","jamie","michael","mason","toby","aaron","charles","ben","theo","louis","freddie","finlay","leon","harley","david","mohammad","reece","kian","kai","kyle","brandon","hayden","zachary","kieran","luca","ashton","bailey","sebastian","gabriel","sam","evan","bradley","elliot","john","taylor","joe","corey","reuben","joel","robert","ellis","blake","aidan","louie","christopher","ewan","jay","morgan","billy","sean","zak"]

postPopulateUsersR :: Yesod.HandlerFor FantasticSpoon Aeson.Value
postPopulateUsersR = do
  stdGen <- Monad.IO.liftIO Random.getStdGen
  let
    ageRange = (10, 100)
    fakePassword = "pwd"
    ages = take (length nameList) (Random.randomRs ageRange stdGen)
    nameAgePairs = zip nameList ages
    users = fmap (\(name, age) -> User name fakePassword age) nameAgePairs
  Yesod.runDB $ do
    _ <- mapM Persist.insert users
    return ()
  return $ Aeson.object ["usersAdded" .= Aeson.toJSON (length users)]

main :: IO ()
main = do
  connectionString    <- fmap (Text.Encoding.encodeUtf8 . Text.pack)  $ Environment.getEnv "DATABASE_URL"
  openConnectionCount <- fmap (Text.Read.read @Int)                   $ Environment.getEnv "DATABASE_CONNECTION_COUNT"
  port                <- fmap (Text.Read.read @Int)                   $ Environment.getEnv "PORT"
  Monad.Logger.runStderrLoggingT $ do
    $(Monad.Logger.logInfo) $ "Open connection count: " <> (Text.pack . show) openConnectionCount
    $(Monad.Logger.logInfo) $ "Port: " <> (Text.pack . show) port

    Persist.Postgresql.withPostgresqlPool connectionString openConnectionCount $ \pool -> Monad.IO.liftIO $ do
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
