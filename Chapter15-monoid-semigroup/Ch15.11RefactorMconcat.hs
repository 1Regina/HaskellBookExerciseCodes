module Madness where

import Data.Monoid ()

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String

-- Replace with mconcat
madlibbinBetter' e adv noun adj = mconcat
  [ e
  , "! he said "
  , adv
  , " as he jumped into his car "
  , noun
  , " and drove off with his "
  , adj
  , " wife."
  ]

-- >>> madlibbinBetter' "wow" "excitedly" "seat" "pretty"
-- "wow! he said excitedly as he jumped into his car seat and drove off with his pretty wife."
--
