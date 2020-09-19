module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

filterEntry :: (Entry -> Boolean) -> AddressBook -> Maybe Entry
filterEntry f = head <<< filter f

-- Exercise 1.
-- head :: AddressBook -> Maybe Entry
-- filter :: (Entry -> Boolean) -> AddressBook -> AddressBook
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = filterEntry byName
  where
    byName :: Entry -> Boolean
    byName e = e.firstName == firstName && e.lastName == lastName

-- Exercise 2.
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = filterEntry byStreet
  where
    byStreet :: Entry -> Boolean
    byStreet { address } = address.street == street


-- Exercise 3.
entryExists :: String -> String -> AddressBook -> Boolean
entryExists firstName lastName = not null <<< filter byName
  where
    byName :: Entry -> Boolean
    byName e = e.firstName == firstName && e.lastName == lastName

-- Exercise 4.
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy compareEntryByName
  where
    compareEntryByName :: Entry -> Entry -> Boolean
    compareEntryByName a b = a.firstName == b.firstName && a.lastName == b.lastName

