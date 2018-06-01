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

import qualified Control.Monad.IO.Class as Monad.IO
import qualified Control.Monad.Logger as Monad.Logger
import qualified Control.Monad.Trans.Reader as Monad.Trans.Reader
import qualified Control.Monad.Trans.Resource as Monad.Trans.Resource
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.String.Interpolate as String.Interpolate
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import           Database.Persist ((==.))
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
  password Text Maybe
  age Int
  UniqueName name
  deriving Show
  deriving Generic
|]

instance Aeson.ToJSON User
instance Aeson.FromJSON User

data App = App
  { appStatic :: Static
  , appConnectionPool :: Persist.Postgresql.ConnectionPool
  }

Yesod.mkYesod "App" [Yesod.parseRoutes|
/ HomeR GET POST
/ages AgesR GET
/register RegisterR GET POST
/static StaticR Static appStatic
|]

instance Yesod.Yesod App

instance Yesod.YesodPersist App where
  type YesodPersistBackend App = Persist.Postgresql.SqlBackend

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

getHomeR :: Yesod.HandlerFor App Yesod.Html
getHomeR = do
  maybeUserId <- tryGetUserId
  case maybeUserId of
    Just _ -> Yesod.redirect AgesR
    Nothing -> return ()
  Yesod.sendResponseStatus HTTP.Types.status200 $ RawHtml [String.Interpolate.i|<!DOCTYPE html>
<meta charset="utf-8">
<head>
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
</head>
<body>
<div style="max-width: 600px; margin: 0 auto;">
<img src="static/images/wood-spoon-sm.jpg" style="float: left; margin: 0 0 1em 1em;">
<div style="font-family: sans serif; font-size: 32px">Fantastic Spoon 🥄</div>
<form method="post">
  <div><input type="text" name="name"><div>
  <div><input type="password" name="password"><div>
  <div><input type="submit" value="Sign in"><div>
</form>
</div>
</body>
|]

noUserRedirect :: Yesod.HandlerFor App a
noUserRedirect = Yesod.redirect HomeR

postHomeR :: Yesod.HandlerFor App ()
postHomeR = do
  maybeName <- Yesod.lookupPostParam "name"
  maybePassword <- Yesod.lookupPostParam "password"
  (name, password) <-
    case (maybeName, maybePassword) of
      (Just name, Just password) -> return (name, password)
      _ -> noUserRedirect

  users <- Yesod.runDB $ Persist.selectList [UserName ==. name] []
  user <-
    case users of
      [user] -> return user
      _ -> noUserRedirect

  if (userPassword . Persist.entityVal) user == Just password
    then return ()
    else noUserRedirect

  setSessionUserId $ Persist.entityKey user
  Yesod.redirect AgesR

setSessionUserId :: UserId -> Yesod.HandlerFor App ()
setSessionUserId = Yesod.setSessionBS sessionUserIdKey . ByteString.Lazy.toStrict . Aeson.encode

sessionUserIdKey :: Text
sessionUserIdKey = "userId"

tryGetUserId :: Yesod.HandlerFor App (Maybe UserId)
tryGetUserId = do
  maybeUserIdBytes <- Yesod.lookupSessionBS sessionUserIdKey
  let maybeUserId = maybeUserIdBytes >>= Aeson.decodeStrict @UserId
  return maybeUserId

requireUserId :: Yesod.HandlerFor App UserId
requireUserId = do
  maybeUserId <- tryGetUserId
  userId <-
    case maybeUserId of
      Nothing -> noUserRedirect
      Just x -> return x
  return userId

getAgesR :: Yesod.HandlerFor App Yesod.Html
getAgesR = do
  userId <- requireUserId
  maybeCurrentUser <- Yesod.runDB $ Persist.get userId
  currentUser <-
    case maybeCurrentUser of
      Nothing -> noUserRedirect
      Just x -> return x

  users <- Yesod.runDB $ Persist.selectList [] [Persist.Asc UserAge]
  let
    currentAge = userAge currentUser
    userAges = fmap (userAge . Persist.entityVal) users
    makeJSArray items = Text.concat ["[", Text.intercalate "," items, "]"]
    userAgesJS = makeJSArray $ fmap (Text.pack . show) userAges
    baseColor = "\"rgba(220, 226, 189, 1)\"" :: Text
    selectedColor = "\"rgba(147, 192, 164, 1)\"" :: Text
    start = 10 :: Int
    end = 100 :: Int
    step = 5 :: Int
    bins = [start, start + step .. end]
    colorsJS = makeJSArray $ fmap (\x -> if currentAge >= x && currentAge < x + step then selectedColor else baseColor) bins
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
    color: #{colorsJS},
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

getRegisterR :: Yesod.HandlerFor App Yesod.Html
getRegisterR = do
  Yesod.sendResponseStatus HTTP.Types.status200 $ RawHtml [String.Interpolate.i|<!DOCTYPE html>
<meta charset="utf-8">
<head>
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
</head>
<body>
<div style="max-width: 600px; margin: 0 auto;">
<img src="static/images/wood-spoon-sm.jpg" style="float: left; margin: 0 0 1em 1em;">
<div style="font-family: sans serif; font-size: 32px">Fantastic Spoon 🥄</div>
<div style="font-family: sans serif; font-size: 24px">Register</div>
<form method="post">
  <div><input type="text" name="name"><div>
  <div><input type="password" name="password"><div>
  <div><input type="text" name="age"><div>
  <div><input type="submit" value="Register"><div>
</form>
</div>
</body>
|]

postRegisterR :: Yesod.HandlerFor App Aeson.Value
postRegisterR = do
  maybeName <- Yesod.lookupPostParam "name"
  maybePassword <- Yesod.lookupPostParam "password"
  maybeAge <- Yesod.lookupPostParam "age"
  (name, password, ageText) <-
    case pure (,,) <*> maybeName <*> maybePassword <*> maybeAge of
      Just (name, password, ageText) -> return (name, password, ageText)
      _ -> Yesod.redirect RegisterR
  age <-
    case Text.Read.readMaybe @Int (Text.unpack ageText) of
      Just age -> return age
      _ -> Yesod.redirect RegisterR
  let user = User name (Just password) age
  userId <- Yesod.runDB $ Persist.insert user
  setSessionUserId userId
  Yesod.redirect AgesR

nameList :: [Text]
nameList = ["olivia","ruby","emily","grace","jessica","chloe","sophie","lily","amelia","evie","mia","ella","charlotte","lucy","megan","ellie","isabelle","isabella","hannah","katie","ava","holly","summer","millie","daisy","phoebe","freya","abigail","poppy","erin","emma","molly","imogen","amy","jasmine","isla","scarlett","leah","sophia","elizabeth","eva","brooke","matilda","caitlin","keira","alice","lola","lilly","amber","isabel","lauren","georgia","gracie","eleanor","bethany","madison","amelie","isobel","paige","lacey","sienna","libby","maisie","anna","rebecca","rosie","tia","layla","maya","niamh","zara","sarah","lexi","maddison","alisha","sofia","skye","nicole","lexie","faith","martha","harriet","zoe","eve","julia","aimee","hollie","lydia","evelyn","alexandra","maria","francesca","tilly","florence","alicia","abbie","emilia","courtney","maryam","esme","jack","oliver","thomas","harry","joshua","alfie","charlie","daniel","james","william","samuel","george","joseph","lewis","ethan","mohammed","dylan","benjamin","alexander","jacob","ryan","liam","jake","max","luke","tyler","callum","matthew","jayden","oscar","archie","adam","riley","harvey","harrison","lucas","muhammad","henry","isaac","leo","connor","edward","finley","logan","noah","cameron","alex","owen","rhys","nathan","jamie","michael","mason","toby","aaron","charles","ben","theo","louis","freddie","finlay","leon","harley","david","mohammad","reece","kian","kai","kyle","brandon","hayden","zachary","kieran","luca","ashton","bailey","sebastian","gabriel","sam","evan","bradley","elliot","john","taylor","joe","corey","reuben","joel","robert","ellis","blake","aidan","louie","christopher","ewan","jay","morgan","billy","sean","zak"]

populateUsersIfNeeded :: Monad.Trans.Reader.ReaderT Persist.Postgresql.SqlBackend (Monad.Trans.Resource.ResourceT IO) ()
populateUsersIfNeeded = do
  stdGen <- Monad.IO.liftIO Random.getStdGen
  let
    ageRange = (10, 100)
    fakePassword = Just "pwd"
    ages = take (length nameList) (Random.randomRs ageRange stdGen)
    nameAgePairs = zip nameList ages
    users = fmap (\(name, age) -> User name fakePassword age) nameAgePairs
  existingUser <- Persist.selectList [UserName ==. head nameList] []
  case existingUser of
    [] -> mapM Persist.insert users >> return ()
    (_ : _) -> return ()

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
        runMigrationAction = do
          Persist.Postgresql.runMigration migrateAll
          populateUsersIfNeeded
        runMigrationWithPool = Persist.Postgresql.runSqlPool runMigrationAction pool
      Monad.Trans.Resource.runResourceT runMigrationWithPool

      let embeddedStatic = $(Yesod.Static.embed "../static")
      let
        application =
          App
          { appStatic = embeddedStatic
          , appConnectionPool = pool
          }
      Yesod.warp port application
