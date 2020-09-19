module Main where

import Data.AddressBook
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, ($))

rawData :: AddressBook
rawData = (
  { firstName: "Giorno", lastName: "Giovana", address: { street: "No. 123 Fake Road", city: "Fake City", state: "XX" } } :
  { firstName: "Pablo", lastName: "Asdf", address: { street: "No. 789 Fake Road", city: "Fake City", state: "XX" } } :
  { firstName: "Pablo", lastName: "Asdf", address: { street: "No. 789 Fake Road", city: "Fake City", state: "XX" } } :
  { firstName: "Fabio", lastName: "Lee", address: { street: "No. 456 Fake Road", city: "Fake City", state: "XX" } } :
  { firstName: "Giorno", lastName: "Giovana", address: { street: "No. 123 Fake Road", city: "Fake City", state: "XX" } } :
  { firstName: "Fabio", lastName: "Lee", address: { street: "No. 456 Fake Road", city: "Fake City", state: "XX" } } :
  { firstName: "Pablo", lastName: "Asdf", address: { street: "No. 789 Fake Road", city: "Fake City", state: "XX" } } :
  { firstName: "Fabio", lastName: "Lee", address: { street: "No. 456 Fake Road", city: "Fake City", state: "XX" } } :
  Nil
)

main :: Effect Unit
main = do
  case result of
    Just entry -> log $ showEntry entry
    Nothing -> log "🍝"
  where
    book = removeDuplicates rawData
    result = findEntryByStreet "No. 123 Fake Road" book
