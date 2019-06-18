module Ritoppu.Model.Tile
  ( Tile(..)
  , passibleThrough
  , seeableThrough
  ) where

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
