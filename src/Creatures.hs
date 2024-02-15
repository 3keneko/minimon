{-# LANGUAGE OverloadedLists #-}
module Creatures (createGenericCreature) where
import MinimonTypes (MiniType(..))
import Minimon (Minimon(..), Attack(..))


frenchDico :: MiniType -> String
frenchDico Fire = "feu"
frenchDico Plant = "plante"
frenchDico Ice = "glace"
frenchDico Steel = "acier"
frenchDico Normal = "normal"

createGenericCreature :: MiniType -> String -> Minimon
createGenericCreature typ n = Minimon { hp=400, minitype=typ,
                                      attacks=[
                                        Attack { damage=15, number=10, the_type=Normal, attackName="Coup de poing"},
                                        Attack { damage=15, number=15, the_type=typ, attackName="Pot de " ++ frenchDico typ },
                                        Attack { damage=30, number=5, the_type=typ, attackName="Tornade de " ++ frenchDico typ }
                                        ],
                                        name=n
                                    }
