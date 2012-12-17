{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Server.Location.Chat

where

import Data.Data            ( Data, Typeable )
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
import Data.Lens.Template   ( makeLens )
import Data.Lens
import Data.Text            ( Text )

import Server.Profile.Acid    ( UserName )
import Server.Auth.Acid       ( UserId )

newtype Chat = Chat (UserName, Text) deriving (Eq, Ord, Data, Typeable, SafeCopy, Read, Show) 

newtype ChatList = ChatList { _chats :: [Chat] } deriving (Eq, Ord, Data, Typeable, SafeCopy, Read, Show)

class ChatRoom room where
    addChat     :: Chat -> room -> room
    getChats    :: room -> [Chat]

instance ChatRoom ChatList where
    addChat chat (ChatList chats) = ChatList $ chat : chats
    getChats (ChatList chats) = chats

emptyChatList :: ChatList
emptyChatList = ChatList []
