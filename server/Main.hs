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
<style>

.bars rect {
  fill: steelblue;
}

.axis text {
  font: 10px sans-serif;
}

.axis path, .axis line {
  fill: none;
  stroke: #000;
  shape-rendering: crispEdges;
}

</style>
<body>
<script src="https://d3js.org/d3.v3.min.js"></script>
<script>

var userAges = #{userAgesJS};
d3.select("body")
  .datum(userAges)
  .call(histogramChart());

function histogramChart() {
  var margin = {top: 0, right: 0, bottom: 20, left: 0},
      width = 960,
      height = 500;

  var histogram = d3.layout.histogram(),
      x = d3.scale.ordinal(),
      y = d3.scale.linear(),
      xAxis = d3.svg.axis().scale(x).orient("bottom").tickSize(6, 0);

  function chart(selection) {
    selection.each(function(data) {

      // Compute the histogram.
      data = histogram(data);

      // Update the x-scale.
      x   .domain(data.map(function(d) { return d.x; }))
          .rangeRoundBands([0, width - margin.left - margin.right], .1);

      // Update the y-scale.
      y   .domain([0, d3.max(data, function(d) { return d.y; })])
          .range([height - margin.top - margin.bottom, 0]);

      // Select the svg element, if it exists.
      var svg = d3.select(this).selectAll("svg").data([data]);

      // Otherwise, create the skeletal chart.
      var gEnter = svg.enter().append("svg").append("g");
      gEnter.append("g").attr("class", "bars");
      gEnter.append("g").attr("class", "x axis");

      // Update the outer dimensions.
      svg .attr("width", width)
          .attr("height", height);

      // Update the inner dimensions.
      var g = svg.select("g")
          .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

      // Update the bars.
      var bar = svg.select(".bars").selectAll(".bar").data(data);
      bar.enter().append("rect");
      bar.exit().remove();
      bar .attr("width", x.rangeBand())
          .attr("x", function(d) { return x(d.x); })
          .attr("y", function(d) { return y(d.y); })
          .attr("height", function(d) { return y.range()[0] - y(d.y); })
          .order();

      // Update the x-axis.
      g.select(".x.axis")
          .attr("transform", "translate(0," + y.range()[0] + ")")
          .call(xAxis);
    });
  }

  chart.margin = function(_) {
    if (!arguments.length) return margin;
    margin = _;
    return chart;
  };

  chart.width = function(_) {
    if (!arguments.length) return width;
    width = _;
    return chart;
  };

  chart.height = function(_) {
    if (!arguments.length) return height;
    height = _;
    return chart;
  };

  // Expose the histogram's value, range and bins method.
  d3.rebind(chart, histogram, "value", "range", "bins");

  // Expose the x-axis' tickFormat method.
  d3.rebind(chart, xAxis, "tickFormat");

  return chart;
}

</script>
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
