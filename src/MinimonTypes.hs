module MinimonTypes where

data MiniTypes = Fire | Ice | Steel | Plant deriving (Show, Eq)

data Effectivness = Effective | Normal | Ineffective | Noeffect
  deriving (Show, Eq)


effectivness :: MiniTypes -> MiniTypes -> Effectivness

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

effectivness x y = if x == y then Ineffective else Normal
