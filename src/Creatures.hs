{-# LANGUAGE OverloadedLists #-}
module Creatures (createGenericCreature, nameIt) where
import MinimonTypes (MiniType(..))
import Minimon (Minimon(..), Attack(..))


-- frenchDico :: MiniType -> String
-- frenchDico Fire = "feu"
-- frenchDico Grass = "plante"
-- frenchDico Ice = "glace"
-- frenchDico Steel = "acier"
-- frenchDico Normal = "normal"

nameIt :: MiniType -> String
nameIt Fire = "Drakof"
nameIt Grass = "Bulbiz"
nameIt Steel = "Steelix"
nameIt Ice = "Icecone"
nameIt Normal = "Josh"
nameIt _ = undefined

createGenericHelp :: MiniType -> String -> String -> String -> String -> Minimon
createGenericHelp typ att1 att2 att3 theName = Minimon { hp=400, minitype=typ,
                                attacks=[
                                    Attack { damage=15, number=10, the_type=Normal, attackName=att1 },
                                    Attack { damage=15, number=15, the_type=typ, attackName=att2 },
                                    Attack { damage=30, number=5, the_type=Fire, attackName=att3 }
                                ],
                                name=theName
                        }

createGenericCreature :: MiniType -> Minimon
createGenericCreature Fire = createGenericHelp Fire "Coup de poing" "Tornade de feu" "Coup de Soleil" "Pyrex"
createGenericCreature Water = createGenericHelp Water "Coup de poing" "Hydrocanon" "Cascade" "AquaMon"
createGenericCreature Electric = createGenericHelp Electric "Coup de poing" "Éclair" "Tonnerre" "ElectroMon"
createGenericCreature Grass = createGenericHelp Grass "Coup de poing" "Lame-feuille" "Fouet lianes" "HerbMon"
createGenericCreature Ice = createGenericHelp Ice "Coup de poing" "Verglas" "Avalanche" "GlaciusMon"
createGenericCreature Fighting = createGenericHelp Fighting "Coup de poing" "Poing Karaté" "Coup Croisé" "CombatMon"
createGenericCreature Poison = createGenericHelp Poison "Coup de poing" "Gaz Toxik" "Vapeur Toxique" "ToxikMon"
createGenericCreature Ground = createGenericHelp Ground "Coup de poing" "Séisme" "Tombe de Pierre" "TerreMon"
createGenericCreature Flying = createGenericHelp Flying "Coup de poing" "Vent Violent" "Ouragan" "AiluMon"
createGenericCreature Psychic = createGenericHelp Psychic "Coup de poing" "Psyko" "Choc Mental" "PsyMon"
createGenericCreature Bug = createGenericHelp Bug "Coup de poing" "Piqûre" "Toile Gluante" "InsectMon"
createGenericCreature Rock = createGenericHelp Rock "Coup de poing" "Éboulement" "Rocher" "RocheMon"
createGenericCreature Ghost = createGenericHelp Ghost "Coup de poing" "Ombre" "Spectre" "FantômeMon"
createGenericCreature Dragon = createGenericHelp Dragon "Coup de poing" "Dracosouffle" "Furie Draconique" "DracoMon"
createGenericCreature Dark = createGenericHelp Dark "Coup de poing" "Morsure" "Croc Fatal" "TénèbreMon"
createGenericCreature Steel = createGenericHelp Steel "Coup de poing" "Tranch'Acier" "Métalaser" "AcierMon"
createGenericCreature Fairy = createGenericHelp Fairy "Coup de poing" "Éclat Magique" "Baiser Démoniaque" "FéeMon"
createGenericCreature Normal = createGenericHelp Normal "Coup de poing" "Charge" "Écrasement" "NormalMon"
