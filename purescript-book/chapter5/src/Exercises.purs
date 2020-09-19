module Exercises where

import Prelude

-- Exercise 5.5.1
factorial ∷ Int → Int
factorial n = loop n 1
  where
    loop 0 acc = acc
    loop m acc = loop (m - 1) (m * acc)

-- Exercise 5.5.2
choose ∷ Int → Int → Int
choose n k
  | 0 <= k && k < n =
    factorial n / (factorial k * factorial (n - k))
  | otherwise = 0

--

type Address = {
  street ∷ String,
  city ∷ String
}

type Person = {
  name ∷ String,
  address ∷ Address
}

-- Exercise 5.9.1
sameCity ∷ Person → Person → Boolean
sameCity
  { address: { city: x } }
  { address: { city: y } }
  = x == y

-- Exercise 5.9.3
fromSingleton ∷ ∀ a. a → Array a → a
fromSingleton d [x] = x
fromSingleton d _ = d
