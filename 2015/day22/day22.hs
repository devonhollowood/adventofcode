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
    case runSearch initial_state of
      Just (mana, steps) -> return (mana, steps)
      Nothing -> die "Victory is impossible!"
  printf "Winning strategy:\n"
  mapM_ (\(cst, st) ->
           case st ^. justCast of
             Just sp -> printf ("    "%s%" ("%d%" mana)\n") (name sp) (cost sp)
             Nothing -> return ()
        )
    steps
  printf ("Total cost: "%d%" mana\n") total_mana

runSearch initial_state = 
  Search.djikstra
  (\st -> map (\sp -> (cost sp, fullTurn sp st)) (possibleSpells st))
  (\st -> st ^. (boss . hp) <= 0) -- win
  [\st -> st ^. (player . hp) <= 0] -- lose
  initial_state

fullTurn :: Spell -> BattleState -> BattleState
fullTurn sp st =
  let halfway = applyEffects . cast sp $ st
  in
    if halfway ^. boss . hp <= 0
    then halfway
    else applyEffects . bossTurn $ halfway

cast :: Spell -> BattleState -> BattleState
cast sp st = st
  & over effects (sp :)
  & player . mana -~ cost sp
  & justCast .~ Just sp

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
          Ongoing _ f _ -> f
          EffectEnd f -> f
      update sp rest =
        case effect sp of
          EffectEnd _ -> rest
          Ongoing 0 _ ef -> sp {effect = ef} : rest
          Ongoing t f ef -> sp {effect = Ongoing (t - 1) f ef} : rest
  in final_st & effects .~ final_effs

magicMissile :: Spell
magicMissile = Spell {
  name = "Magic Missile",
  cost = 53,
  effect = immediateEffect $ \st -> st & boss . hp -~ 4
  }

drain :: Spell
drain = Spell {
  name = "Drain",
  cost = 73,
  effect = immediateEffect $ \st -> st & (boss . hp -~ 2) . (player . hp +~ 2)
  }

shield :: Spell
shield = Spell {
  name = "Shield",
  cost = 113,
  effect = bracketedEffect 6
    (\st -> st & player . armor +~ 7)
    (\st -> st & player . armor -~ 7)
  }

poison :: Spell
poison = Spell {
  name = "Poison",
  cost = 173,
  effect = ongoingEffect 6 $ \st -> st & boss . hp -~ 3
  }

recharge :: Spell
recharge = Spell {
  name = "Recharge",
  cost = 229,
  effect = ongoingEffect 5 $ \st -> st & player . mana +~ 101
  }

possibleSpells :: BattleState -> [Spell]
possibleSpells st =
  let current_mana = st ^. player . mana
      current_spells = st ^. effects
  in filter (\sp -> cost sp <= current_mana && not (sp `elem` current_spells)) [
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
  _justCast :: Maybe Spell
  } deriving (Eq, Ord, Show)

player :: Lens' BattleState Character
player = lens _player (\bs x -> bs {_player = x })
boss :: Lens' BattleState Character
boss = lens _boss (\bs x -> bs {_boss = x })
effects :: Lens' BattleState [Spell]
effects = lens _effects (\bs x -> bs {_effects = x })
justCast :: Lens' BattleState (Maybe Spell)
justCast = lens _justCast (\bs x -> bs {_justCast = x})

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
  show sp = Text.unpack $
    format (s%" "%d) (name sp) (remainingTurns $ effect sp)

instance Eq Spell where
  a == b = name a == name b

instance Ord Spell where
  compare = Ord.comparing name

data Effect =
  Ongoing Int (BattleState -> BattleState) Effect
  | EffectEnd (BattleState -> BattleState)

bracketedEffect ::
  Int -- Turns
  -> (BattleState -> BattleState) -- Start
  -> (BattleState -> BattleState) -- End
  -> Effect
bracketedEffect turns f inverse =
  Ongoing 0 f $ Ongoing (turns - 1) id $ EffectEnd inverse

ongoingEffect ::
  Int -- Turns
  -> (BattleState -> BattleState) -- status change
  -> Effect
ongoingEffect turns f = Ongoing (turns - 2) f $ EffectEnd f

immediateEffect :: (BattleState -> BattleState) -> Effect
immediateEffect f = EffectEnd f

remainingTurns :: Effect -> Int
remainingTurns (Ongoing t _ ef) = (t + 1) + remainingTurns ef
remainingTurns (EffectEnd f) = 0

parser =
  Character
  <$> (fromIntegral <$> optInteger "health" 'p' "Boss health")
  <*> pure 0
  <*> pure 0
  <*> (fromIntegral <$> optInteger "damage" 'd' "Boss damage")
