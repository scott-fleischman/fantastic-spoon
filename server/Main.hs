{-# LANGUAGE DeriveGeneric #-} -- Allow code to process Haskell data structures in this file
{-# LANGUAGE GADTs #-} -- Required by Yesod code generation
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Required by Yesod code generation
{-# LANGUAGE MultiParamTypeClasses #-} -- Required by Yesod code generation
{-# LANGUAGE OverloadedStrings #-} -- interpret string literals with different types
{-# LANGUAGE QuasiQuotes #-} -- allow multiline strings and parsing of them
{-# LANGUAGE TemplateHaskell #-} -- allow code generation within this file
{-# LANGUAGE TypeApplications #-} -- allow explicit naming of types
{-# LANGUAGE TypeFamilies #-} -- Required by Yesod code generation
{-# OPTIONS -Wno-unused-top-binds #-} -- Ignore unused generated code

import qualified Control.Monad.IO.Class as Monad.IO -- label use of logging and database code
import qualified Control.Monad.Logger as Monad.Logger -- logging utility
import qualified Control.Monad.Trans.Reader as Monad.Trans.Reader -- used by database library
import qualified Control.Monad.Trans.Resource as Monad.Trans.Resource -- close resources at end of code block
import qualified Data.Aeson as Aeson -- JSON processing
import qualified Data.ByteString.Lazy as ByteString.Lazy -- byte processing
import qualified Data.String.Interpolate as String.Interpolate -- multiline strings with variable interpolation
import           Data.Semigroup ((<>)) -- abstract string concatenation
import           Data.Text (Text) -- string type
import qualified Data.Text as Text -- string library
import qualified Data.Text.Encoding as Text.Encoding -- string encoding as bytes
import           Database.Persist ((==.)) -- SQL equality check
import qualified Database.Persist as Persist -- Database library
import qualified Database.Persist.Postgresql as Persist.Postgresql -- PostgreSQL-specific database library
import qualified Database.Persist.TH as Persist.TH -- SQL schema generation from Haskell code
import           GHC.Generics (Generic) -- Allow code to process types defined in this file
import qualified Network.HTTP.Types as HTTP.Types -- HTTP data structures
import qualified System.Environment as Environment -- Environment variable loading
import qualified System.Random as Random -- Random number generation
import qualified Text.Read -- Parse strings as numbers
import qualified Yesod -- Web site library
import           Yesod.Auth.HashDB (HashDBUser) -- user with hashed password
import qualified Yesod.Auth.HashDB -- library for hashing passwords

-- Define our Database schema in Haskell
-- We have one table "User", with name, password and age
-- Create a unique index on "name".
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

instance Aeson.ToJSON User -- code gen to serialize User in JSON
instance Aeson.FromJSON User -- code gen to parse User from JSON

-- Code for hashing a User's password before storing in the database
instance HashDBUser User where
  userPasswordHash = userPassword
  setPasswordHash h u = u { userPassword = Just h }

-- Application settings. We need only a connection pool for PostgreSQL.
data App = App
  { appConnectionPool :: Persist.Postgresql.ConnectionPool
  }

-- | Routing for our web application.
-- "/" supports GET and POST, implemented by the 'getHomeR' and 'postHomeR' functions.
-- "/ages" supports GET, implemented by 'getAgeR' function.
-- "/register" supports GET and POST, implemented by 'getRegisterR' and 'postRegisterR' functions.
-- "/sign-out" supports GET implemented by 'getSignOutR' function.
Yesod.mkYesod "App" [Yesod.parseRoutes|
/ HomeR GET POST
/ages AgesR GET
/register RegisterR GET POST
/sign-out SignOutR GET
|]

-- Hookup our application state to the web server
instance Yesod.Yesod App

-- Implement the database backend on our web server
instance Yesod.YesodPersist App where
  type YesodPersistBackend App = Persist.Postgresql.SqlBackend

  -- How to run a database action:
  -- Get a connection from the connection pool
  -- and send the query/update to PostgreSQL.
  runDB action = do
    app <- Yesod.getYesod
    let pool = appConnectionPool app
    Persist.Postgresql.runSqlPool action pool

-- Boilerplate to allow us to use interpolated strings as HTML responses for a web page.
-- Sets the content type to HTML in the HTTP response.
newtype RawHtml = RawHtml String
instance Yesod.ToContent RawHtml where
  toContent (RawHtml htmlText) = Yesod.toContent htmlText
instance Yesod.ToTypedContent RawHtml where
  toTypedContent rawHtml = Yesod.TypedContent Yesod.typeHtml (Yesod.toContent rawHtml)
instance Yesod.HasContentType RawHtml where
  getContentType _ = Yesod.typeHtml

-- Common HTML for including Bootstrap CSS and JS at the top of the web page.
bootstrapStyles :: String
bootstrapStyles = [String.Interpolate.i|
<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css" integrity="sha384-WskhaSGFgHYWDcbwN70/dfYBj47jz9qbsMId/iRN3ewGhXQFZCSftd1LZCfmhktB" crossorigin="anonymous">
<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" integrity="sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" crossorigin="anonymous"></script>
<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/js/bootstrap.min.js" integrity="sha384-smHYKdLADwkXOn1EmN1qk/HfnUcbVRZyYmZ4qpPea6sjB/pTJ0euyQp0Mk8ck+5T" crossorigin="anonymous"></script>
|]

-- Common navbar HTML using Bootstrap.
makeNavBar :: Yesod.HandlerFor App String
makeNavBar = do
  render <- Yesod.getUrlRender -- we can create links to our other pages using this function
  maybeUserId <- tryGetUserId -- check whether we have an authenticated user
  let
    navLinkDisabled =
      case maybeUserId of
        Just _ -> "" -- 
        Nothing -> "disabled" :: String
    agesLink = render AgesR -- create link to "ages" page
    registerLink = render RegisterR -- create link to "register" page
    signOutLink = render SignOutR -- create link to "sign out" page

  -- HTML for navbar
  -- embed data from above using this syntax:  #{variable}`
  return [String.Interpolate.i|
<nav class="navbar navbar-expand-lg navbar-light bg-light">
  <a class="navbar-brand" href="#">Fantastic Spoon 🥄</a>
  <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarNav" aria-controls="navbarNav" aria-expanded="false" aria-label="Toggle navigation">
    <span class="navbar-toggler-icon"></span>
  </button>
  <div class="collapse navbar-collapse" id="navbarNav">
    <ul class="navbar-nav">
      <li class="nav-item">
        <a class="nav-link #{navLinkDisabled}" href="#{agesLink}">Ages</a>
      </li>
      <li class="nav-item">
        <a class="nav-link" href="#{registerLink}">Register</a>
      </li>
      <li class="nav-item">
        <a class="nav-link #{navLinkDisabled}" href="#{signOutLink}">Sign Out</a>
      </li>
    </ul>
  </div>
</nav>
|] -- this ends the interpolated string

-- handle signout
getSignOutR :: Yesod.HandlerFor App Yesod.Html
getSignOutR = do
  Yesod.clearSession -- clear all data from the session
  noUserRedirect -- redirect to default page

-- set an alert message in the session data
setAlert :: Text -> Yesod.HandlerFor App ()
setAlert alert = Yesod.setSession "alert" alert

-- Get HTML for the alert in the session data
-- If no such data is present, then return empty HTML.
getAlertHtml :: Yesod.HandlerFor App String
getAlertHtml = do
  maybeAlert <- Yesod.lookupSession "alert" -- lookup the alert mesage
  case maybeAlert of
    Nothing -> return "" -- none, so return empty

    Just alert -> do -- alert message is present!
      -- delete the message from the session
      Yesod.deleteSession "alert" 

      -- return a <div> with the message
      return [String.Interpolate.i|
<div class="alert alert-warning" role="alert">#{alert}</div>
|]

-- Home page.
-- This redirects to the "ages" page when logged in.
-- Else shows the log in form.
getHomeR :: Yesod.HandlerFor App Yesod.Html
getHomeR = do
  -- check for logged-in user
  maybeUserId <- tryGetUserId

  case maybeUserId of
    -- user present, so redirect to ages
    Just _ -> Yesod.redirect AgesR

    -- no user present so continue processing
    Nothing -> return ()

  navBar <- makeNavBar -- make the nav bar HTML
  alertHtml <- getAlertHtml -- make any alert message HTML
  render <- Yesod.getUrlRender -- get the link function
  let registerLink = render RegisterR -- make a link to the register page

  -- return a 200 status
  Yesod.sendResponseStatus HTTP.Types.status200 $

    -- HTML for the page
    -- data from above is interpolated using this syntax: #{variable}
    RawHtml [String.Interpolate.i|<!DOCTYPE html>
<meta charset="utf-8">
<head>
#{bootstrapStyles}
</head>
<body>
#{navBar}
<div class="container" style="width: 800px">
<h1>Fantastic Spoon 🥄</h1>
#{alertHtml}
<p>Sign in with your name and password below or <a href="#{registerLink}">Register</a> a new account.</p>
<form method="post">
  <div class="form-group">
    <label for="userName1">Name</label>
    <input type="text" class="form-control" id="userName1" placeholder="Enter name" name="name">
  </div>
  <div class="form-group">
    <label for="inputPassword1">Password</label>
    <input type="password" class="form-control" id="inputPassword1" placeholder="Password" name="password">
  </div>
  <button type="submit" class="btn btn-primary">Sign In</button>
</form>
</div>
</body>
|]

-- The default redirect when not logged in
-- Redirects to the log in page.
noUserRedirect :: Yesod.HandlerFor App a
noUserRedirect = Yesod.redirect HomeR

-- POST to the home page. This happens when clicking on the "Sign in" button.
postHomeR :: Yesod.HandlerFor App ()
postHomeR = do
  -- get the name and password from the form
  maybeName <- Yesod.lookupPostParam "name"
  maybePassword <- Yesod.lookupPostParam "password"

  -- do simple validation on the name and password:
  -- both must be non-empty
  (name, password) <-
    case (maybeName, maybePassword) of
      (Just name, Just password)
        | (not . Text.null) name
        , (not . Text.null) password
        -> return (name, password)
      _ -> do
        setAlert "Please enter both name and password." -- message when name or password is empty
        noUserRedirect

  -- lookup the user in the database matching the name
  users <- Yesod.runDB $ Persist.selectList [UserName ==. name] []

  -- validate that the user exists
  user <-
    case users of
      [user] -> return user
      _ -> do
        setAlert "User not found." -- message if the user does not exist in the database
        noUserRedirect

  -- validate the hashed password
  if Yesod.Auth.HashDB.validatePass (Persist.entityVal user) password == Just True
    then return ()
    else do
      setAlert "Invalid password." -- message if the hashed passwords do not match
      noUserRedirect

  -- successful login!
  -- set the userId on the session to indicate they are logged in
  setSessionUserId $ Persist.entityKey user

  -- redirect to the ages page
  Yesod.redirect AgesR

-- set the UserId in the session encoded as bytes
setSessionUserId :: UserId -> Yesod.HandlerFor App ()
setSessionUserId = Yesod.setSessionBS sessionUserIdKey . ByteString.Lazy.toStrict . Aeson.encode

-- the name of the session key for the UserId
sessionUserIdKey :: Text
sessionUserIdKey = "userId"

-- check the session for the userId
tryGetUserId :: Yesod.HandlerFor App (Maybe UserId)
tryGetUserId = do
  maybeUserIdBytes <- Yesod.lookupSessionBS sessionUserIdKey
  let maybeUserId = maybeUserIdBytes >>= Aeson.decodeStrict @UserId
  return maybeUserId

-- Check the session for the UserId.
-- If not present then redirect to the log in page
requireUserId :: Yesod.HandlerFor App UserId
requireUserId = do
  maybeUserId <- tryGetUserId
  userId <-
    case maybeUserId of
      Nothing -> noUserRedirect -- redirect when user not logged in
      Just x -> return x
  return userId -- return the UserId when successful

-- Render the ages page (requires logged in user)
getAgesR :: Yesod.HandlerFor App Yesod.Html
getAgesR = do
  -- get user or redirect
  userId <- requireUserId

  -- get full User data by looking up based on userId in the database
  maybeCurrentUser <- Yesod.runDB $ Persist.get userId
  currentUser <-
    case maybeCurrentUser of
      Nothing -> noUserRedirect -- redirect to log in if the userId does not exist in the database
      Just x -> return x

  -- get all users from the database, ordered by ascending age
  users <- Yesod.runDB $ Persist.selectList [] [Persist.Asc UserAge]

  navBar <- makeNavBar -- create nav bar HTML
  let
    -- the age of the current user
    currentAge = userAge currentUser

    -- list of all users ages
    userAges = fmap (userAge . Persist.entityVal) users

    -- helper function to make a string for a JavaScript array
    makeJSArray items = Text.concat ["[", Text.intercalate "," items, "]"]

    -- the user ages as a JS array (rendered as a string)
    userAgesJS = makeJSArray $ fmap (Text.pack . show) userAges

    -- color of bar for other ages
    baseColor = "\"rgba(220, 226, 189, 1)\"" :: Text

    -- color of bar for your age
    selectedColor = "\"rgba(147, 192, 164, 1)\"" :: Text

    -- define the histogram bins, ages 10-100 with a bar covering 5 years
    start = 10 :: Int
    end = 100 :: Int
    step = 5 :: Int
    bins = [start, start + step .. end]

    -- define the colors of the bins, using the base color except when the user age matches the age range, then use selectedColor
    colorsJS = makeJSArray $
      fmap
      (\x ->
        if currentAge >= x
          && currentAge < x + step
        then selectedColor
        else baseColor
      )
      bins

  -- send 200 response
  Yesod.sendResponseStatus HTTP.Types.status200

    -- define HTML inline with interpolation: #{variable}
    $ RawHtml [String.Interpolate.i|<!DOCTYPE html>
<meta charset="utf-8">
<head>
#{bootstrapStyles}
<script src="https://cdn.plot.ly/plotly-1.38.1.min.js"></script>
</head>
<body>
#{navBar}
<h1 style="text-align: center">Ages of Users 🥄</h1>
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
  navBar <- makeNavBar
  alertHtml <- getAlertHtml
  Yesod.sendResponseStatus HTTP.Types.status200 $ RawHtml [String.Interpolate.i|<!DOCTYPE html>
<meta charset="utf-8">
<head>
#{bootstrapStyles}
</head>
<body>
#{navBar}
<div class="container" style="width: 800px">
<h1>Register 🥄</h1>
#{alertHtml}
<form method="post">
  <div class="form-group">
    <label for="userName1">Name</label>
    <input type="text" class="form-control" id="userName1" aria-describedby="nameHelp" placeholder="Enter name" name="name">
    <small id="nameHelp" class="form-text text-muted">We'll never share your name with anyone else.</small>
  </div>
  <div class="form-group">
    <label for="inputPassword1">Password</label>
    <input type="password" class="form-control" id="inputPassword1" placeholder="Password" name="password">
  </div>
  <div class="form-group">
    <label for="userAge1">Name</label>
    <input type="text" class="form-control" id="userAge1" aria-describedby="ageHelp" placeholder="Enter age" name="age">
    <small id="ageHelp" class="form-text text-muted">We show a histogram of ages of all users in aggregate.</small>
  </div>
  <button type="submit" class="btn btn-primary">Register</button>
</form>
</div>
</body>
|]

-- POST on register page, when registration form is submitted
postRegisterR :: Yesod.HandlerFor App Aeson.Value
postRegisterR = do
  -- get form data for name, password and age
  maybeName <- Yesod.lookupPostParam "name"
  maybePassword <- Yesod.lookupPostParam "password"
  maybeAge <- Yesod.lookupPostParam "age"

  -- ensure all three fields are present
  (name, password, ageText) <-
    case pure (,,) <*> maybeName <*> maybePassword <*> maybeAge of
      Just (name, password, ageText)
        | (not . Text.null) name
        , (not . Text.null) password
        , (not . Text.null) ageText
        -> return (name, password, ageText)
      _ -> do
        setAlert "Please fill out all fields." -- message when one field isn't filled out
        Yesod.redirect RegisterR -- redirect to register page again

  -- parse/validate age as a number
  age <-
    case Text.Read.readMaybe @Int (Text.unpack ageText) of
      Just age -> return age
      _ -> do
        setAlert "Invalid age." -- message when age cannot be parsed as a number
        Yesod.redirect RegisterR -- redirect to register page to show message

  -- validate there is no existing user with that name
  -- query database for a user with that name
  existingUsers <- Yesod.runDB $ Persist.selectList [UserName ==. name] []
  case existingUsers of
    [] -> return ()
    (_ : _) -> do
      setAlert "Name already exists." -- message when name already exists
      Yesod.redirect RegisterR -- redirect to show message

  -- first create a user data structure with no password
  let userNoPassword = User name Nothing age

  -- hash the password given to store in the database
  user <- Yesod.Auth.HashDB.setPassword password userNoPassword

  -- insert the User record in the database
  -- receive the userId in response
  userId <- Yesod.runDB $ Persist.insert user

  -- set the userId in the session so they are logged in
  setSessionUserId userId

  -- go to ages page
  Yesod.redirect AgesR

-- hard-coded list of names to create fake data
nameList :: [Text]
nameList = ["olivia","ruby","emily","grace","jessica","chloe","sophie","lily","amelia","evie","mia","ella","charlotte","lucy","megan","ellie","isabelle","isabella","hannah","katie","ava","holly","summer","millie","daisy","phoebe","freya","abigail","poppy","erin","emma","molly","imogen","amy","jasmine","isla","scarlett","leah","sophia","elizabeth","eva","brooke","matilda","caitlin","keira","alice","lola","lilly","amber","isabel","lauren","georgia","gracie","eleanor","bethany","madison","amelie","isobel","paige","lacey","sienna","libby","maisie","anna","rebecca","rosie","tia","layla","maya","niamh","zara","sarah","lexi","maddison","alisha","sofia","skye","nicole","lexie","faith","martha","harriet","zoe","eve","julia","aimee","hollie","lydia","evelyn","alexandra","maria","francesca","tilly","florence","alicia","abbie","emilia","courtney","maryam","esme","jack","oliver","thomas","harry","joshua","alfie","charlie","daniel","james","william","samuel","george","joseph","lewis","ethan","mohammed","dylan","benjamin","alexander","jacob","ryan","liam","jake","max","luke","tyler","callum","matthew","jayden","oscar","archie","adam","riley","harvey","harrison","lucas","muhammad","henry","isaac","leo","connor","edward","finley","logan","noah","cameron","alex","owen","rhys","nathan","jamie","michael","mason","toby","aaron","charles","ben","theo","louis","freddie","finlay","leon","harley","david","mohammad","reece","kian","kai","kyle","brandon","hayden","zachary","kieran","luca","ashton","bailey","sebastian","gabriel","sam","evan","bradley","elliot","john","taylor","joe","corey","reuben","joel","robert","ellis","blake","aidan","louie","christopher","ewan","jay","morgan","billy","sean","zak"]

-- Add random ages with the nameList to populate the database with some data
-- If the first name already exists in the database, though, this function does nothing.
-- That way this can be called every time on startup, and if the data is there it does nothing.
-- But if the database is empty then it populates it with some data.
populateUsersIfNeeded :: Monad.Trans.Reader.ReaderT Persist.Postgresql.SqlBackend (Monad.Trans.Resource.ResourceT IO) ()
populateUsersIfNeeded = do
  -- get random number generator
  stdGen <- Monad.IO.liftIO Random.getStdGen

  let
    -- use ages from 10-100
    ageRange = (10, 100)

    -- use the same fake password for everyone
    fakePassword = "pwd"

    -- generate enough random ages for our names
    ages = take (length nameList) (Random.randomRs ageRange stdGen)

    -- make pairs of (name, age)
    nameAgePairs = zip nameList ages

    -- create user objects with no password
    usersNoPassword = fmap (\(name, age) -> User name Nothing age) nameAgePairs

  -- create hashed passwords for each user
  -- note: this is only done if we end up inserting the users into the database
  users <- mapM (Yesod.Auth.HashDB.setPassword fakePassword) usersNoPassword

  -- check if the first user exists in the database
  existingUser <- Persist.selectList [UserName ==. head nameList] []

  case existingUser of
    -- if not, then add all users
    [] -> mapM Persist.insert users >> return ()

    -- if it is, then do nothing
    (_ : _) -> return ()

-- Main entry point of application:
-- * create database schema
-- * populate with random data
-- * start up web server
main :: IO ()
main = do
  -- get required environment variables with sensitive connection information
  connectionString    <- fmap (Text.Encoding.encodeUtf8 . Text.pack)  $ Environment.getEnv "DATABASE_URL"
  openConnectionCount <- fmap (Text.Read.read @Int)                   $ Environment.getEnv "DATABASE_CONNECTION_COUNT"
  port                <- fmap (Text.Read.read @Int)                   $ Environment.getEnv "PORT"

  -- start logging service
  Monad.Logger.runStderrLoggingT $ do

    -- log connection count and port info for debugging
    $(Monad.Logger.logInfo) $ "Open connection count: " <> (Text.pack . show) openConnectionCount
    $(Monad.Logger.logInfo) $ "Port: " <> (Text.pack . show) port

    -- create connection pool to PostgreSQL
    Persist.Postgresql.withPostgresqlPool connectionString openConnectionCount $ \pool -> Monad.IO.liftIO $ do

      -- define database actions to do on startup
      let
        runMigrationAction = do
          -- create database schema
          Persist.Postgresql.runMigration migrateAll

          -- populate database with random user age data
          populateUsersIfNeeded

        -- use a connection from the connection pool to run the database actions
        runMigrationWithPool = Persist.Postgresql.runSqlPool runMigrationAction pool

      -- actually run the database actions
      -- return the connection back to the pool once done
      Monad.Trans.Resource.runResourceT runMigrationWithPool

      -- define our application state, which only needs the connection pool information
      let application = App { appConnectionPool = pool }

      -- start up the web server
      Yesod.warp port application
