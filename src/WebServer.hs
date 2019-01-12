{-# LANGUAGE OverloadedStrings #-}

module WebServer where

import Web.Spock
import Web.Spock.Config
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Network.Wai.Middleware.Static

import Command
import World

newtype ServerState = ServerState { world :: IORef World }

type Server a = SpockM () () ServerState a

webServer :: World -> Int -> IO ()
webServer world port = do
    st <- ServerState <$> newIORef world
    cfg <- defaultSpockCfg () PCNoDatabase st
    runSpock port (spock cfg app)

app :: Server ()
app = do
    get root $ redirect "/index.html"
    middleware (staticPolicy (addBase "static"))
    post "/action" $ do
        userId <- param' "uid"
        command <- param' "cmd"
        worldRef <- world <$> getState
        output <- liftIO $ atomicModifyIORef' worldRef $ \world ->
            let
                operationResult = do
                    world' <- addPlayerIfAbsent userId world
                    applyCommand userId (parseCommand command) world'
            in case operationResult of
                Left err ->
                    (world, [ err ])
                Right (nextWorld) ->
                    extractOutput userId nextWorld
        html $ pack $ foldl (\acc s -> acc ++ "\n" ++ s) "" output