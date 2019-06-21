module Ritoppu.Model.Tile
  ( Tile(..)
  , passibleThrough
  , seeableThrough
  ) where

import Prelude

data Tile
  = Floor
  | Wall

passibleThrough :: Tile -> Boolean
passibleThrough = case _ of
  Floor -> true
  Wall -> false

seeableThrough :: Tile -> Boolean
seeableThrough = case _ of
  Floor -> true
  Wall -> false

instance showTile :: Show Tile where
  show = case _ of
    Floor -> "F"
    Wall -> "W"
