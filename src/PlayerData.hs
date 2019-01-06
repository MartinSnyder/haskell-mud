module PlayerData where

type UserId = String

data PlayerData = PlayerData { userId :: UserId
                             , name :: String
                             } deriving (Show, Eq)
