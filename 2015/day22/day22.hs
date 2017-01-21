{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Text as Text
import Turtle
import Control.Lens
import qualified Data.Ord as Ord
import qualified Search

main :: IO ()
main = sh $ do
  initial_boss <- options "Day 21" parser
  let initial_player = Character 50 500 0 0
  let initial_state = BattleState initial_player initial_boss [] Nothing
  (total_mana, steps) <-
    case Search.djikstra
         (\st -> map (\sp -> (cost sp, bossTurn $ cast sp st)) spells)
         (\st -> st ^. (boss . hp) <= 0)
         [\st -> st ^. (player . hp) <= 0]
         initial_state
    of
      Just (mana, steps) -> return (mana, steps)
      Nothing -> die "Victory is impossible!"
  printf "Winning strategy:\n"
  mapM_ (\(cst, st) ->
           case justCast st of
             Just sp -> printf ("    "%s%" ("%d%" mana)\n") (name sp) (cost sp)
             Nothing -> return ()
        )
    steps
  printf ("Total cost: "%d%"\n") total_mana

fullTurn :: Spell -> BattleState -> BattleState
fullTurn sp = bossTurn . applyEffects . cast sp . applyEffects

cast :: Spell -> BattleState -> BattleState
cast sp st = st & over effects (sp :)

bossTurn :: BattleState -> BattleState
bossTurn st = st & player . hp -~ (if raw_damage > 1 then raw_damage else 1)
  where
    raw_damage = (st ^. boss . damage) - (st ^. player . armor)

applyEffects :: BattleState -> BattleState
applyEffects st =
  let (final_st, final_effs) =
        foldl
        (\(st, efs') sp -> (act sp st, update sp efs')) (st, [])
        (st ^. effects)
      act sp =
        case effect sp of
          Immediate f -> f
          Ongoing _ f -> f
      update sp rest =
        case effect sp of
          Immediate _ -> rest
          Ongoing 0 _ -> rest
          Ongoing t f -> sp {effect = Ongoing (t - 1) f} : rest
  in final_st & effects .~ final_effs

magicMissile :: Spell
magicMissile = Spell {
  name = "Magic Missile",
  cost = 53,
  effect = Immediate $ \st -> st & boss . hp -~ 4
  }

drain :: Spell
drain = Spell {
  name = "Drain",
  cost = 73,
  effect = Immediate $ \st -> st & (boss . hp -~ 2) . (player . hp +~ 2)
  }

shield :: Spell
shield = Spell {
  name = "Shield",
  cost = 113,
  effect = Ongoing 6 $ \st -> st & player . armor +~ 7
  }

poison :: Spell
poison = Spell {
  name = "Poison",
  cost = 73,
  effect = Ongoing 6 $ \st -> st & boss . hp -~ 3
  }

recharge :: Spell
recharge = Spell {
  name = "Recharge",
  cost = 229,
  effect = Ongoing 5 $ \st -> st & player . mana +~ 101
  }

spells :: [Spell]
spells = [
  magicMissile,
  drain,
  shield,
  poison,
  recharge
  ]

data Result = Win | Lose

data BattleState = BattleState {
  _player :: Character,
  _boss :: Character,
  _effects :: [Spell],
  justCast :: Maybe Spell
  } deriving (Eq, Ord, Show)

player :: Lens' BattleState Character
player = lens _player (\bs x -> bs {_player = x })
boss :: Lens' BattleState Character
boss = lens _boss (\bs x -> bs {_boss = x })
effects :: Lens' BattleState [Spell]
effects = lens _effects (\bs x -> bs {_effects = x })

data Character = Character {
  _hp :: Int,
  _mana :: Int,
  _armor :: Int,
  _damage :: Int
  } deriving (Eq, Ord, Show)

hp :: Lens' Character Int
hp = lens _hp (\ch x -> ch { _hp = x })
mana :: Lens' Character Int
mana = lens _mana (\ch x -> ch { _mana = x })
armor :: Lens' Character Int
armor = lens _armor (\ch x -> ch { _armor = x })
damage :: Lens' Character Int
damage = lens _damage (\ch x -> ch { _damage = x })

data Spell = Spell {
  name :: Text,
  cost :: Int,
  effect :: Effect
  }

instance Show Spell where
  show = Text.unpack . name

instance Eq Spell where
  a == b = name a == name b

instance Ord Spell where
  compare = Ord.comparing name

data Effect =
  Immediate (BattleState -> BattleState)
  | Ongoing Int (BattleState -> BattleState)

parser =
  Character
  <$> (fromIntegral <$> optInteger "health" 'p' "Boss health")
  <*> pure 0
  <*> (fromIntegral <$> optInteger "damage" 'd' "Boss damage")
  <*> pure 0
