{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Turtle
import qualified Data.Maybe as Maybe
import qualified Search as Search

main :: IO ()
main = sh $ do
  boss <- options "Day 21" parser
  let minimal = Loadout 0 Nothing Nothing Nothing
  let won_fight load = battle (makePlayer load) boss == "Player"
  cheapest_steps <-
    case Search.djikstra upgrade won_fight [] minimal of
      Just victory -> return (snd victory)
      Nothing -> die "Victory is impossible!"
  let cheapest = snd $ last cheapest_steps
  let cheapest_cost = loadoutCost cheapest
  printf "Cheapest winning loadout (part 1):\n"
  printf ("    "%s%"\n") . itemName $ weapons !! weaponIdx cheapest
  printf ("    "%s%"\n") $
    maybe "No armor" (itemName . (armors !!)) (armorIdx cheapest)
  printf ("    "%s%"\n") $
    maybe "No left ring" (itemName . (rings !!)) (leftRingIdx cheapest)
  printf ("    "%s%"\n") $
    maybe "No right ring" (itemName . (rings !!)) (rightRingIdx cheapest)
  printf ("Total cost (part 1): "%d%"\n") cheapest_cost
  let maximal = Loadout 4 (Just 4) (Just 4) (Just 5)
  most_expensive_steps <-
    case Search.djikstra downgrade (not . won_fight) [] maximal of
      Just victory -> return (snd victory)
      Nothing -> die "Victory is certain!"
  let most_expensive = snd $ last most_expensive_steps
  let most_expensive_cost = loadoutCost most_expensive
  printf "Most expensive losing loadout (part 2):\n"
  printf ("    "%s%"\n") . itemName $ weapons !! weaponIdx most_expensive
  printf ("    "%s%"\n") $
    maybe "No armor" (itemName . (armors !!)) (armorIdx most_expensive)
  printf ("    "%s%"\n") $
    maybe "No left ring" (itemName . (rings !!)) (leftRingIdx most_expensive)
  printf ("    "%s%"\n") $
    maybe "No right ring" (itemName . (rings !!)) (rightRingIdx most_expensive)
  printf ("Total cost (part 2): "%d%"\n") most_expensive_cost


battle :: Character -> Character -> PlayerName
battle p1 p2
  | hp p1 <= 0 = name p2
  | otherwise = battle damaged_p2 p1
  where
    damaged_p2 =
      let raw_damage = damage p1 - armor p2
      in p2 {hp = hp p2 - if raw_damage < 1 then 1 else raw_damage}

upgrade ::
  Loadout -- current loadout
  -> [(Int, Loadout)] -- [(incremental cost, new loadout)]
upgrade loadout =
  Maybe.catMaybes [
  (\idx -> loadout { weaponIdx = idx })
    <$> try_upgrade (Just $ weaponIdx loadout) weapons,
  (\idx -> loadout { armorIdx = Just idx })
    <$> try_upgrade (armorIdx loadout) armors,
  do
    ridx <- rightRingIdx loadout
    lidx <- try_upgrade (leftRingIdx loadout) rings
    if lidx < ridx
      then Just $ loadout {leftRingIdx = Just lidx}
      else Nothing
  ,
  (\idx -> loadout { rightRingIdx = Just idx })
    <$> try_upgrade (rightRingIdx loadout) rings
  ]
  & map (\new -> (loadoutCost new - loadoutCost loadout, new))
  where
    try_upgrade Nothing table = Just 0
    try_upgrade (Just idx) table =
      if idx + 1 >= length table
      then Nothing
      else Just (idx + 1)

downgrade ::
  Loadout -- current loadout
  -> [(Int, Loadout)] -- [(incremental savings, new loadout)]
downgrade loadout =
  Maybe.catMaybes [
  do
    wmidx <- try_downgrade (Just $ weaponIdx loadout) weapons
    (\idx -> loadout { weaponIdx = idx }) <$> wmidx,
  (\midx -> loadout { armorIdx = midx })
    <$> try_downgrade (armorIdx loadout) armors,
  (\midx -> loadout { leftRingIdx = midx })
    <$> try_downgrade (leftRingIdx loadout) rings,
  do
    rmidx <- try_downgrade (rightRingIdx loadout) rings
    let lmidx = leftRingIdx loadout
    if Maybe.isNothing lmidx && Maybe.isNothing rmidx || lmidx < rmidx
      then Just $ loadout {rightRingIdx = rmidx}
      else Nothing
  ]
  & map (\new -> (loadoutCost loadout - loadoutCost new, new))
  where
    try_downgrade Nothing table = Nothing
    try_downgrade (Just idx) table =
      if idx == 0
      then Just Nothing
      else Just . Just $ idx - 1

loadoutCost :: Loadout -> Int
loadoutCost Loadout{..} =
  cost (weapons !! weaponIdx)
  + maybe 0 (cost . (armors !!)) armorIdx
  + maybe 0 (cost . (rings !!)) leftRingIdx
  + maybe 0 (cost . (rings !!)) rightRingIdx

makePlayer :: Loadout -> Character
makePlayer Loadout{..} =
  Character {
    name = "Player",
    hp = 100,
    damage =
      damageBonus (weapons !! weaponIdx)
      + maybe 0 (damageBonus . (rings !!)) leftRingIdx
      + maybe 0 (damageBonus . (rings !!)) rightRingIdx,
    armor =
      maybe 0 (armorBonus . (armors !!)) armorIdx
      + maybe 0 (armorBonus . (rings !!)) leftRingIdx
      + maybe 0 (armorBonus . (rings !!)) rightRingIdx
  }

type PlayerName = Text

data Character = Character {
  name :: Text,
  hp :: Int,
  damage :: Int,
  armor :: Int
  } deriving (Show)

data Item = Item {
  itemName :: Text,
  cost :: Int,
  damageBonus :: Int,
  armorBonus :: Int
  } deriving (Eq, Ord, Show)

data Loadout = Loadout {
  weaponIdx :: Int,
  armorIdx :: Maybe Int,
  leftRingIdx :: Maybe Int,
  rightRingIdx :: Maybe Int
  } deriving (Eq, Ord, Show)

weapons = [
  Item "Dagger" 8 4 0,
  Item "Shortsword" 10 5 0,
  Item "Warhammer" 25 6 0,
  Item "Longsword" 40 7 0,
  Item "Greataxe" 74 8 0
  ]

armors = [
  Item "Leather" 13 0 1,
  Item "Chainmail" 31 0 2,
  Item "Splintmail" 53 0 3,
  Item "Bandedmail" 75 0 4,
  Item "Platemail" 102 0 5
  ]

rings = [
  Item "Defense +1" 20 0 1,
  Item "Damage +1" 25 1 0,
  Item "Defense +2" 40 0 2,
  Item "Damage +2" 50 2 0,
  Item "Defense +3" 80 0 3,
  Item "Damage +3" 100 3 0
  ]

parser =
  Character "Boss"
  <$> (fromIntegral <$> optInteger "health" 'p' "Boss health")
  <*> (fromIntegral <$> optInteger "damage" 'd' "Boss damage")
  <*> (fromIntegral <$> optInteger "armor" 'a' "Boss armor")
