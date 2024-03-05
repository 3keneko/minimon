{-# LANGUAGE DerivingStrategies, DeriveAnyClass, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module MinimonTypes (
  MiniType(..),
  Effectivness(..),
  effectiveness,
  effCoeff,
  types
) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data MiniType = Normal
              | Fire
              | Water
              | Electric
              | Grass
              | Ice
              | Fighting
              | Poison
              | Ground
              | Flying
              | Psychic
              | Bug
              | Rock
              | Ghost
              | Dragon
              | Dark
              | Steel
              | Fairy
              deriving (Eq, Show, Ord, Enum, Read)
              deriving stock (Generic)
              deriving anyclass (Hashable)

data Effectivness = Effective | NoEffect | Ineffective | Immune
  deriving Eq

instance Show Effectivness where
  show Effective = "C'est super efficace!"
  show NoEffect  = ""
  show Ineffective = "Ce n'est pas super efficace"
  show Immune = "Cela n'a aucun effet..."

types :: [MiniType]
types = [Normal ..(Fairy)]

effectiveness :: MiniType -> MiniType -> Effectivness
effectiveness Fire Grass = Effective
effectiveness Fire Water = Ineffective
effectiveness Fire Rock = Ineffective
effectiveness Fire Ice = Effective
effectiveness Fire Bug = Effective
effectiveness Fire Steel = Ineffective
effectiveness Fire Dragon = Ineffective

effectiveness Water Fire = Effective
effectiveness Water Ground = Effective
effectiveness Water Rock = Effective
-- effectiveness Water Electric = Ineffective
effectiveness Water Grass = Ineffective
effectiveness Water Dragon = Ineffective

effectiveness Electric Water = Effective
effectiveness Electric Flying = Effective
effectiveness Electric Steel = Effective
effectiveness Electric Grass = Ineffective
effectiveness Electric Electric = Ineffective
effectiveness Electric Dragon = Ineffective
effectiveness Electric Ground = Immune

effectiveness Grass Water = Ineffective
effectiveness Grass Ground = Effective
effectiveness Grass Flying = Ineffective
effectiveness Grass Bug = Ineffective
effectiveness Grass Fire = Ineffective
effectiveness Grass Grass = Ineffective
effectiveness Grass Poison = Effective
effectiveness Grass Rock = Effective
-- effectiveness Grass Electric = Effective
effectiveness Grass Steel = Ineffective
effectiveness Grass Dragon = Ineffective

effectiveness Ice Grass = Effective
effectiveness Ice Fire = Ineffective
effectiveness Ice Water = Ineffective
effectiveness Ice Ice = Ineffective
effectiveness Ice Ground = Effective
effectiveness Ice Flying = Effective
effectiveness Ice Dragon = Effective
effectiveness Ice Steel = Ineffective

effectiveness Fighting Normal = Effective
effectiveness Fighting Ice = Effective
effectiveness Fighting Rock = Effective
effectiveness Fighting Dark = Effective
effectiveness Fighting Steel = Effective
effectiveness Fighting Psychic = Ineffective
effectiveness Fighting Fairy = Ineffective
effectiveness Fighting Flying = Ineffective
effectiveness Fighting Poison = Ineffective
effectiveness Fighting Bug = Ineffective
effectiveness Fighting Ghost = Ineffective

effectiveness Poison Grass = Effective
effectiveness Poison Fairy = Effective
effectiveness Poison Poison = Ineffective
effectiveness Poison Ground = Ineffective
effectiveness Poison Rock = Ineffective
effectiveness Poison Ghost = Ineffective
effectiveness Poison Steel = Immune

effectiveness Ground Fire = Effective
effectiveness Ground Electric = Effective
effectiveness Ground Poison = Effective
effectiveness Ground Rock = Ineffective
effectiveness Ground Steel = Ineffective
effectiveness Ground Flying = Immune

effectiveness Flying Grass = Effective
effectiveness Flying Fighting = Effective
effectiveness Flying Bug = Effective
effectiveness Flying Ground = Ineffective
effectiveness Flying Electric = Ineffective
effectiveness Flying Rock = Ineffective
effectiveness Flying Steel = Ineffective

effectiveness Psychic Fighting = Effective
effectiveness Psychic Poison = Effective
effectiveness Psychic Psychic = Ineffective
effectiveness Psychic Dark = Ineffective
effectiveness Psychic Steel = Ineffective

effectiveness Bug Grass = Effective
effectiveness Bug Psychic = Effective
effectiveness Bug Dark = Effective
effectiveness Bug Fighting = Ineffective
effectiveness Bug Flying = Ineffective
effectiveness Bug Poison = Ineffective
effectiveness Bug Ghost = Ineffective
effectiveness Bug Steel = Effective
effectiveness Bug Fairy = Ineffective

effectiveness Rock Fire = Effective
effectiveness Rock Ice = Effective
effectiveness Rock Flying = Effective
effectiveness Rock Bug = Effective
effectiveness Rock Ground = Ineffective
effectiveness Rock Steel = Ineffective

effectiveness Ghost Psychic = Effective
effectiveness Ghost Ghost = Effective
effectiveness Ghost Dark = Ineffective
effectiveness Ghost Normal = Ineffective

effectiveness Dragon Dragon = Effective
effectiveness Dragon Steel = Ineffective
effectiveness Dragon Fairy = Immune

effectiveness Dark Psychic = Effective
effectiveness Dark Ghost = Effective
effectiveness Dark Dark = Ineffective
effectiveness Dark Fighting = Ineffective
effectiveness Dark Fairy = Ineffective

effectiveness Steel Ice = Effective
effectiveness Steel Rock = Effective
effectiveness Steel Fairy = Effective
effectiveness Steel Steel = Ineffective

effectiveness Fairy Fighting = Effective
effectiveness Fairy Bug = Effective
effectiveness Fairy Dark = Effective
effectiveness Fairy Dragon = Effective
effectiveness Fairy Poison = Ineffective
effectiveness Fairy Steel = Ineffective
effectiveness x y = if x == y then Ineffective else NoEffect

effCoeff :: MiniType -> MiniType -> Int
effCoeff x y = case effectiveness x y of
  Effective -> 4
  NoEffect -> 2
  Ineffective -> 1
  Immune -> 0
