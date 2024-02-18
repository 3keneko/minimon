module MinimonTypes (
  MiniType(..),
  Effectivness,
  effectivness,
  effCoeff,
  types
) where

data MiniType = Fire | Ice | Steel | Plant | Normal deriving (Eq, Show, Ord, Enum)

data Effectivness = Effective | NoEffect | Ineffective | Immune
  deriving Eq

instance Show Effectivness where
  show Effective = "C'est super efficace!"
  show NoEffect  = ""
  show Ineffective = "Ce n'est pas super efficace"
  show Immune = "Cela n'a aucun effet..."

types :: [MiniType]
types = [(Fire)..(Plant)]

effectivness :: MiniType -> MiniType -> Effectivness

effectivness Fire Plant = Effective
effectivness Fire Steel = Effective
effectivness Fire Ice = Effective

effectivness Plant Fire = Ineffective
effectivness Plant Steel = Ineffective

effectivness Ice Fire = Ineffective
effectivness Ice Plant = Effective
effectivness Ice Steel = Ineffective

effectivness Steel Fire = Ineffective
effectivness Steel Ice = Effective

effectivness x y = if x == y then Ineffective else NoEffect

effCoeff :: MiniType -> MiniType -> Int
effCoeff x y = case effectivness x y of
  Effective -> 4
  NoEffect -> 2
  Ineffective -> 1
  Immune -> 0
