module Link where

import Data.Map as Map
import Data.Maybe (fromMaybe)

import LinkDef
import PassageDef
import GameDef
import GameObj

data Link = Link { def :: LinkDef
                 , passageDef :: Maybe PassageDef
                 }

instance GameObj Link where
    sDesc link = case passageDef link of
        Just passageDef -> GameDef.sDesc passageDef
        Nothing         -> GameDef.sDesc $ Link.def link
    lDesc link = case passageDef link of
        Just passageDef -> GameDef.lDesc passageDef
        Nothing         -> GameDef.lDesc $ Link.def link
    matches link keyword =
        let
            maybeMatchPassageDef = fmap (\pd -> GameDef.matches pd keyword) $ passageDef link
            matchesLink = GameDef.matches (Link.def link) keyword
        in  matchesLink || (fromMaybe False maybeMatchPassageDef)

buildLink :: Map DefId PassageDef -> LinkDef ->Link
buildLink passageDefs linkDef =
    Link linkDef ((passageId linkDef) >>= (\id -> Map.lookup id passageDefs))

exitString :: Link -> String
exitString link = case direction $ def link of
    Enter -> case passageDef link of
        Just passageDef -> GameDef.sDesc passageDef
        Nothing -> "Broken Exit"
    dir -> (getLabel dir) ++ " - " ++ (GameObj.sDesc link)

canClose :: Link -> Bool
canClose link = case passageDef link of
    Just pDef -> PassageDef.canClose $ passageType pDef
    Nothing -> False

canLock :: Link -> Bool
canLock link = case passageDef link of
    Just pDef -> PassageDef.canLock $ passageType pDef
    Nothing -> False