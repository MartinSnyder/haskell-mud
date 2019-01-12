{-# LANGUAGE OverloadedStrings #-}

module WebServer where

import Web.Spock
import Web.Spock.Config
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)

import Command
import World

newtype ServerState = ServerState { world :: IORef World }

type Server a = SpockM () () ServerState a

webServer :: World -> IO ()
webServer world = do
    st <- ServerState <$> newIORef world
    cfg <- defaultSpockCfg () PCNoDatabase st
    runSpock 8080 (spock cfg app)

app :: Server ()
app = do
    -- get root $ do
    --     world' <- getState >>= (liftIO . readIORef . world)
    --     html $ pack $ show world'

    get root $ do
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
        html $ pack $ foldl (\acc s -> acc ++ "<br>" ++ s) "" output