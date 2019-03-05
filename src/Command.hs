module Command where

data Command = Noop
             | Invalid String
             | Help
             | Broadcast String
             | Yell String
             | Say String
             | Go String
             | Look
             | Inventory
             | Get String
             | Drop String
             deriving (Eq, Show)