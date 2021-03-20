port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Markdown
import NarrativeEngine.Core.Rules as Rules
import NarrativeEngine.Core.WorldModel as WorldModel exposing (addTag, applyChanges, emptyLinks, emptyStats, emptyTags, getStat, setStat)
import NarrativeEngine.Debug
import NarrativeEngine.Syntax.EntityParser as EntityParser
import NarrativeEngine.Syntax.Helpers as SyntaxHelpers
import NarrativeEngine.Syntax.NarrativeParser as NarrativeParser
import NarrativeEngine.Syntax.RuleParser as RuleParser
import Process
import Random
import Random.Extra as Random
import Random.List as Random
import Set exposing (Set)
import Task


type alias EntityFields =
    NamedComponent {}


type alias NamedComponent a =
    { a
        | name : String
        , description : String
    }


type alias MyEntity =
    WorldModel.NarrativeComponent EntityFields


type alias MyWorldModel =
    Dict WorldModel.ID MyEntity


type alias RuleFields =
    NarrativeComponent {}


type alias NarrativeComponent a =
    { a | narrative : String }


type alias MyRule =
    Rules.Rule RuleFields


type alias MyRules =
    Dict String MyRule


type alias RulesSpec =
    Dict Rules.RuleID ( String, {} )


type alias Model =
    { parseErrors : Maybe SyntaxHelpers.ParseErrors
    , worldModel : MyWorldModel
    , rules : MyRules
    , ruleCounts : Dict String Int
    , debug : NarrativeEngine.Debug.State
    , generatedHeroes : List Character
    , generatedMonsters : List Character
    , levelsForRound : Levels
    , chooseHero : Bool
    , chooseBonus : Bool
    , lineUpCount : Int
    , story : String
    , continueButton : Maybe ( String, Msg )
    , score : Int
    , battleHistory : Dict WorldModel.ID { beat : Set WorldModel.ID, lost : Set WorldModel.ID }
    , spells : List String
    , bonuses : Set String
    , bonusOptions : List String
    , round : Int
    }


maxCharacters : Int
maxCharacters =
    -- enough images/names to go 4 rounds
    -- 18
    3 + 4 + 5 + 6


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { parseErrors = Nothing
      , worldModel = Dict.empty
      , rules = Dict.empty
      , ruleCounts = Dict.empty
      , debug = NarrativeEngine.Debug.init
      , generatedHeroes = []
      , generatedMonsters = []
      , levelsForRound = { heroes = [], monsters = [] }
      , chooseHero = False
      , chooseBonus = False
      , lineUpCount = 3
      , story = ""
      , continueButton = Nothing
      , score = 10
      , battleHistory = Dict.empty
      , spells = []
      , bonuses = Set.empty
      , bonusOptions = []
      , round = 1
      }
    , Random.generate LineupGenerated (makeLineUp maxCharacters)
    )


bonuses =
    Dict.empty
        |> bonus "Lucky day" "+10 HP" (\k m -> { m | score = m.score + 10 })
        |> bonus "Charm of persistent insight" "Reveal a monster's level when it is defeated" addBonus
        |> bonus "Scroll of self awareness" "Always reveal hero levels" addBonus
        |> bonus "Relic of photographic memory" "See outcomes of previous battles" addBonus
        |> spell "Appeal for bravery"
            "Automatically select a hero who can beat the current monster, and reveal that hero's level (one-time spell)"
            addSpell
            (\m ->
                let
                    monsterId =
                        query "*.monster.fighting" m.worldModel
                            |> List.head
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault "?"
                in
                case query ("*.hero.!defeated.!victorious.!fighting.level>(stat " ++ monsterId ++ ".level)") m.worldModel |> List.head |> Maybe.map Tuple.first of
                    Nothing ->
                        { m | story = "You shout for a brave hero to step forward... but none do.  The heroes that can beat this monster must have already perished.  What a shame." }

                    Just id ->
                        { m
                            | story = "You shout for a brave hero to step forward.  " ++ getName id m.worldModel ++ " answers the call."
                            , continueButton = Just ( "Continue", AppealForBraveryContinue )
                            , chooseHero = False
                            , worldModel = updateWorldModel [ id ++ ".revealed.fighting" ] m.worldModel
                        }
            )
        |> spell "Summoning of identification"
            "Reveal level of current monster (one-time spell)"
            addSpell
            (\m ->
                case query "*.monster.fighting.revealed" m.worldModel of
                    [] ->
                        { m
                            | worldModel = updateWorldModel [ "(*.monster.fighting).revealed" ] m.worldModel
                            , story = "You mutter the forbidden words, the monster fidgets, then bashfully admits its level."
                        }

                    _ ->
                        { m | story = "You mutter the forbidden words, the monster fidgets, then laughs at you, as it reveals the information you already knew.  Rookie mistake." }
            )
        |> spell "Incantation of fortification"
            "Increase level of all currently defeated heroes (one-time spell)"
            addSpell
            (\m ->
                case query "*.hero.defeated" m.worldModel of
                    [] ->
                        { m | story = "You cast this potent spell to strengthen the fallen... but no one is able to benefit.  That was foolish." }

                    _ ->
                        { m
                            | worldModel = updateWorldModel [ "(*.hero.defeated).level+10" ] m.worldModel
                            , story = "You look around and see your fallen heroes.  This day things look bleak.  But tomorrow already feels more promising."
                        }
            )
        |> spell "Show of fierceness"
            "All defeated monsters will run away (one-time spell)"
            addSpell
            (\m ->
                case query "*.monster.defeated" m.worldModel of
                    [] ->
                        { m | story = "You muster your strength and give a mighty howl.  But none of the monsters have taken any blows yet, so they just look at you with a mix of bewilderment and disapproval." }

                    _ ->
                        { m
                            | worldModel = updateWorldModel [ "(*.monster.defeated).-monster.-defeated" ] m.worldModel
                            , story = "You muster your strength and give a mighty howl.  The defeated monsters eye you cautiously.  You take the opportunity and jab at them.  They turn their tails and run off.  Good show."
                        }
            )


bonus name desc selectFn =
    Dict.insert name { name = name, description = desc, selectFn = selectFn name, useFn = identity }


spell name desc selectFn useFn =
    Dict.insert name { name = name, description = desc, selectFn = selectFn name, useFn = useFn }


addBonus k m =
    { m | bonuses = Set.insert k m.bonuses }


addSpell k m =
    { m | spells = k :: m.spells }


hasBonus k m =
    Set.member k m.bonuses


type alias NameOptions =
    { prefixs : List String
    , mains : List String
    , suffix1s : List String
    , suffix2s : List String
    }


monsterNameOptions : NameOptions
monsterNameOptions =
    { prefixs =
        [ "Disproportionate "
        , "Resurrected "
        , "Deranged "
        , "Flying "
        , "Scaly "
        , "Sleepy "
        , "Poisonous "
        , "Cursed "
        , "Partly invisible "
        , "Giant "
        , "Uncommon "
        , "Misunderstood "
        , "Dyslexic "
        , "Seemingly innocent "
        , "Mutant "
        , "Undead "
        , "Mildly annoying "
        , "Screaming "
        , "Unstable "
        , "100 year-old "
        , "Supernatural "
        , "Evil "
        ]
    , mains =
        [ "Imp"
        , "Snail"
        , "Spider"
        , "Rodent"
        , "Sock puppet"
        , "Bob the Blob"
        , "Clown"
        , "Newt"
        , "Politician"
        , "Teddy-bear"
        , "Witch"
        , "Zombie"
        , "Hooligan"
        , "Telemarketer"
        , "Presence"
        , "Wraith"
        , "Ghoul"
        , "Undertaker"
        , "Poltergeist"
        , "Halfling"
        ]
    , suffix1s =
        [ " of doom"
        , " of disreputable company"
        , " from Hell"
        , " infected with scurvy"
        , " of limited intellect"
        , " from the house next door"
        , " from your nightmares"
        ]
    , suffix2s =
        [ " with 1000 eyes"
        , " with no face"
        , " with secret powers"
        , " with nefarious intent"
        , ", armed to the teeth"
        , ", without grace"
        , ", blinded with rage"
        , ", with glowing tentacles"
        , ", with a personal vendetta"
        , ", who hasn't eaten for over a week"
        , ", who just wants attention"
        , ", a wanted criminal"
        ]
    }


heroNameOptions : NameOptions
heroNameOptions =
    { prefixs =
        [ "Ill-equipped "
        , "Constable "
        , "Young "
        , "Above-average "
        , "Untrained "
        , "Angry "
        , "Crazy "
        , "Unlucky "
        , "Undercover "
        , "Super "
        , "Trigger-happy "
        , "Fearless "
        ]
    , mains =
        [ "Average Joe"
        , "Sir Wellington"
        , "Sam Harberspoon"
        , "Jimbo"
        , "Ivan"
        , "Sallyworth"
        , "Kennith"
        , "Malinda"
        , "Anastasia"
        , "Billingford"
        , "Daniel"
        , "Lady Ferrington"
        , "\"Max the Axe\""
        , "Eliza"
        , "Boddington"
        , "Cudworth"
        , "Manfred McMurphy"
        , "Hector"
        , "\"Alice the Ace\""
        , "Jessica"
        ]
    , suffix1s =
        [ " the Disgraced"
        , " the Undisputed"
        , " the Bashful"
        , " the Terrible"
        , " the Forgetful"
        , " the Unknown"
        , " the Dim-witted"
        , " the Third"
        , " the Unknown"
        , " the Outcast"
        , " the Wonderful"
        , " Senior"
        , " Junior"
        ]
    , suffix2s =
        [ ", from far, far away"
        , "  and a team of trained ferrets"
        , ", of royal descent"
        , ", of recent notoriety"
        , ", of unparalleled beauty"
        , ", of questionable antics"
        , ", trained by monks"
        , ", who took one semester of \"Monster Hunting 101\""
        , ", wielding \"Fighting Monsters for Dummies\""
        , ", who just wants everyone to get along"
        ]
    }


makeLineUp : Int -> Random.Generator LineUp
makeLineUp n =
    let
        heroImageCount =
            22

        monsterImageCount =
            22

        heroImages =
            List.range 1 heroImageCount |> Random.shuffle

        monsteroImages =
            List.range 1 monsterImageCount |> Random.shuffle

        randomNames nameOptions =
            let
                sometimesTakeName p l =
                    Random.weighted
                        ( 1 - p, ( "", l ) )
                        [ ( p, ( List.head l |> Maybe.withDefault "", List.drop 1 l ) ) ]

                recur n_ g =
                    if n_ == 0 then
                        g

                    else
                        Random.andThen
                            (\( results, { prefixs, mains, suffix1s, suffix2s } ) ->
                                recur (n_ - 1) <|
                                    Random.map4
                                        (\( p, ps ) ( m, ms ) ( s1, s1s ) ( s2, s2s ) ->
                                            ( String.join "" [ p, m, s1, s2 ] :: results
                                            , NameOptions ps ms s1s s2s
                                            )
                                        )
                                        (sometimesTakeName 0.4 prefixs)
                                        (sometimesTakeName 1 mains)
                                        (sometimesTakeName 0.2 suffix1s)
                                        (sometimesTakeName 0.2 suffix2s)
                            )
                            g
            in
            Random.pair
                (Random.constant [])
                (Random.map4 NameOptions
                    (Random.shuffle nameOptions.prefixs)
                    (Random.shuffle nameOptions.mains)
                    (Random.shuffle nameOptions.suffix1s)
                    (Random.shuffle nameOptions.suffix2s)
                )
                |> recur n
                |> Random.map Tuple.first

        monsters =
            Random.map2
                (List.map2 Character)
                (randomNames monsterNameOptions)
                monsteroImages

        heroes =
            Random.map2
                (List.map2 Character)
                (randomNames heroNameOptions)
                heroImages
    in
    Random.map2 LineUp heroes monsters


getDescription : NarrativeParser.Config MyEntity -> WorldModel.ID -> MyWorldModel -> String
getDescription config entityID worldModel_ =
    Dict.get entityID worldModel_
        |> Maybe.map .description
        |> Maybe.withDefault ("ERROR can't find entity " ++ entityID)
        |> NarrativeParser.parse config
        |> List.head
        |> Maybe.withDefault ("ERROR parsing narrative content for " ++ entityID)


getName : WorldModel.ID -> MyWorldModel -> String
getName entityID worldModel_ =
    Dict.get entityID worldModel_
        |> Maybe.map .name
        |> Maybe.withDefault ("ERROR can't find entity " ++ entityID)


makeConfig : WorldModel.ID -> Rules.RuleID -> Dict String Int -> MyWorldModel -> NarrativeParser.Config MyEntity
makeConfig trigger matchedRule ruleCounts worldModel =
    { cycleIndex = Dict.get matchedRule ruleCounts |> Maybe.withDefault 0
    , propKeywords = Dict.singleton "name" (\id -> Ok <| getName id worldModel)
    , worldModel = worldModel
    , trigger = trigger
    }


type alias ParsedEntity =
    Result SyntaxHelpers.ParseErrors ( WorldModel.ID, MyEntity )


type alias Character =
    { name : String, image : Int }


type alias LineUp =
    { heroes : List Character
    , monsters : List Character
    }


type alias Levels =
    { heroes : List Int
    , monsters : List Int
    }


type Msg
    = InteractWith WorldModel.ID
    | UpdateDebugSearchText String
    | AddEntities (EntityParser.ParsedWorldModel EntityFields)
    | AddRules (RuleParser.ParsedRules RuleFields)
    | LineupGenerated LineUp
    | GenerateLevelsForRound
    | BonusesGenerated (List String)
    | BonusSelected (Model -> Model)
    | UseSpell String (Model -> Model)
    | StartRound Levels
    | Tick
    | HeroSelected WorldModel.ID
    | AppealForBraveryContinue
    | HeroPreview (Maybe WorldModel.ID)
    | ResetRound
    | ResetGame
    | ClearBattlefield
    | PlaySound String


type alias EntitySpec =
    { description : String, entity : String, name : String }


type alias RuleSpec =
    { rule : String, rule_id : String, narrative : String }


port playSound : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []


parseEntities : List EntitySpec -> EntityParser.ParsedWorldModel EntityFields
parseEntities entities =
    let
        addExtraEntityFields { description, name } { tags, stats, links } =
            { tags = tags
            , stats = stats
            , links = links
            , name = name
            , description = description
            }

        parsedEntities =
            entities
                |> List.map (\{ entity, description, name } -> ( entity, { description = description, name = name } ))
                |> EntityParser.parseMany addExtraEntityFields

        parsedDescriptions =
            entities
                |> List.map (\{ entity, description } -> ( entity, description ))
                |> Dict.fromList
                |> NarrativeParser.parseMany
    in
    parsedDescriptions |> Result.andThen (always parsedEntities)


parseRules : List RuleSpec -> RuleParser.ParsedRules RuleFields
parseRules rules =
    let
        addExtraEntityFields { narrative } { changes, conditions, trigger } =
            { trigger = trigger
            , conditions = conditions
            , changes = changes
            , narrative = narrative
            }

        parsedRules =
            rules
                |> List.map (\{ rule_id, rule, narrative } -> ( rule_id, ( rule, { narrative = narrative } ) ))
                |> Dict.fromList
                |> RuleParser.parseRules addExtraEntityFields

        parsedNarratives =
            rules
                |> List.map (\{ rule_id, narrative } -> ( rule_id, narrative ))
                |> Dict.fromList
                |> NarrativeParser.parseMany
    in
    parsedNarratives |> Result.andThen (always parsedRules)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InteractWith trigger ->
            -- we need to check if any rule matched
            case Rules.findMatchingRule trigger model.rules model.worldModel of
                Just ( matchedRuleID, { changes, narrative } ) ->
                    ( { model
                        | worldModel = WorldModel.applyChanges changes trigger model.worldModel
                        , story =
                            narrative
                                |> NarrativeParser.parse (makeConfig trigger matchedRuleID model.ruleCounts model.worldModel)
                                |> String.join "\n\n"
                        , ruleCounts = Dict.update matchedRuleID (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just) model.ruleCounts
                        , debug =
                            model.debug
                                |> NarrativeEngine.Debug.setLastMatchedRuleId matchedRuleID
                                |> NarrativeEngine.Debug.setLastInteractionId trigger
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | story = getDescription (makeConfig trigger trigger model.ruleCounts model.worldModel) trigger model.worldModel
                        , ruleCounts = Dict.update trigger (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just) model.ruleCounts
                        , debug =
                            model.debug
                                |> NarrativeEngine.Debug.setLastMatchedRuleId trigger
                                |> NarrativeEngine.Debug.setLastInteractionId trigger
                      }
                    , Cmd.none
                    )

        UpdateDebugSearchText searchText ->
            ( { model | debug = NarrativeEngine.Debug.updateSearch searchText model.debug }, Cmd.none )

        AddEntities parsedEntities ->
            case parsedEntities of
                Err errors ->
                    ( { model | parseErrors = Just errors }, Cmd.none )

                Ok newEntities ->
                    ( { model | parseErrors = Nothing, worldModel = Dict.union newEntities model.worldModel }, Cmd.none )

        AddRules parsedRules ->
            case parsedRules of
                Err errors ->
                    ( { model | parseErrors = Just errors }, Cmd.none )

                Ok newRules ->
                    ( { model | parseErrors = Nothing, rules = Dict.union newRules model.rules }, Cmd.none )

        LineupGenerated lineUp ->
            ( { model | generatedHeroes = lineUp.heroes, generatedMonsters = lineUp.monsters }
            , Random.generate BonusesGenerated (Dict.keys bonuses |> Random.shuffle)
            )

        BonusesGenerated bonusOptions ->
            update GenerateLevelsForRound { model | bonusOptions = bonusOptions }

        BonusSelected selectFn ->
            update GenerateLevelsForRound (selectFn { model | bonusOptions = List.drop 2 model.bonusOptions })

        UseSpell usedName useFn ->
            ( useFn { model | spells = List.filter (\name -> name /= usedName) model.spells }, Cmd.none )

        GenerateLevelsForRound ->
            let
                levelRange =
                    List.range 1 model.lineUpCount

                monsterLevels =
                    levelRange |> List.map ((*) 10) |> Random.shuffle

                heroLevels =
                    levelRange |> List.map ((*) 10 >> (+) 5) |> Random.shuffle

                levelsForRound h m =
                    { heroes = h, monsters = m }
            in
            ( { model | chooseBonus = False }, Random.generate StartRound (Random.map2 levelsForRound heroLevels monsterLevels) )

        StartRound levels ->
            let
                toId tag name =
                    tag
                        ++ "-"
                        ++ name
                        |> String.replace " " "_"
                        |> String.replace "," ""
                        |> String.replace "'" "_"
                        |> String.replace "-" "_"
                        |> String.replace "\"" ""

                makeCharacter tag =
                    List.map2
                        (\{ name, image } level ->
                            ( toId tag name
                            , { tags = emptyTags
                              , stats = emptyStats
                              , links = emptyLinks
                              , name = name
                              , description = ""
                              }
                                |> addTag tag
                                |> setStat "level" level
                                |> setStat "image" image
                            )
                        )

                entities =
                    []
                        ++ (makeCharacter "hero" (List.take model.lineUpCount model.generatedHeroes) levels.heroes
                                |> List.map
                                    (if hasBonus "Scroll of self awareness" model then
                                        Tuple.mapSecond (addTag "revealed")

                                     else
                                        identity
                                    )
                           )
                        ++ makeCharacter "monster" (List.take model.lineUpCount model.generatedMonsters) levels.monsters
                        |> Dict.fromList
            in
            ( { model
                | worldModel = entities
                , generatedHeroes = List.drop model.lineUpCount model.generatedHeroes
                , generatedMonsters = List.drop model.lineUpCount model.generatedMonsters
                , story = "Your village is under attack from invading monsters!  You must send out your heroes to fight them off.\n\n Just one problem - you don't know anyone's strengths, so you'll have to figure it out as you go."
                , continueButton = Just ( "Ready!", Tick )
              }
            , Cmd.none
            )

        ResetRound ->
            let
                worldModel =
                    updateWorldModel
                        [ "(*.hero).-fighting.-defeated.-victorious"
                        , "(*.monster).-fighting.-defeated"
                        ]
                        model.worldModel
            in
            ( { model
                | worldModel = worldModel
                , story = "The clock rolls back..."
                , score = model.score - model.round
                , round = model.round + 1
                , continueButton = Nothing
              }
            , Cmd.batch [ after 1000 Tick ]
            )

        ResetGame ->
            initialModel

        Tick ->
            case
                [ query "*.monster.fighting.!defeated" model.worldModel
                , query "*.hero.fighting.!defeated.!victorious" model.worldModel
                , query "*.monster.!fighting.!defeated" model.worldModel
                , query "*.hero.!fighting.!defeated.!victorious" model.worldModel
                ]
            of
                -- no monster fighting, no monsters left, round won, level up, queue start round
                -- Important - this must come before the round lost check!
                [ [], _, [], _ ] ->
                    let
                        gameOver =
                            model.lineUpCount == 6

                        story =
                            if gameOver then
                                "You fought off all the monsters and they've given up!  You did it, your village is safe, you win."

                            else
                                "You fought off all the monsters! +" ++ String.fromInt model.lineUpCount ++ " HP, and you get to choose a bonus above.\n\nBut don't celebrate just yet... looks like trouble brewing on the horizon."

                        continue =
                            if gameOver then
                                Just ( "Play again", ResetGame )

                            else
                                Nothing
                    in
                    ( { model
                        | lineUpCount = model.lineUpCount + 1
                        , story = story
                        , chooseBonus = not gameOver
                        , score = model.score + model.lineUpCount
                        , worldModel = updateWorldModel [ "(*).-defeated.-victorious.revealed" ] model.worldModel
                        , round = 1
                        , continueButton = continue
                      }
                    , playSound "sfx/win"
                    )

                -- no hero fighting, no heroes left, round lost, restart lineup
                [ _, [], _, [] ] ->
                    if model.score >= model.lineUpCount then
                        ( { model
                            | story = "You are out of heroes, but monsters still remain.  You have lost the battle.\n\nHowever, you have more insight now.  For " ++ String.fromInt model.round ++ " HP you can try again."
                            , continueButton = Just ( "Try again", ResetRound )
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | story = "You are out of heroes, but monsters still remain.  You have lost the battle.  You also don't have enough HP left to try again.  You have failed, the monsters win."
                            , worldModel = updateWorldModel [ "(*).-defeated.-victorious.revealed" ] model.worldModel
                            , continueButton = Just ( "Restart", ResetGame )
                          }
                        , Cmd.none
                        )

                -- no monster fighting, monsters available, heros available monster attacks
                [ [], _, ( monster_up_next, _ ) :: _, _ :: _ ] ->
                    let
                        worldModel =
                            updateWorldModel [ monster_up_next ++ ".fighting" ] model.worldModel
                    in
                    ( { model
                        | worldModel = worldModel
                        , story = getName monster_up_next model.worldModel ++ " attacks!  Choose a hero to join the fight."
                        , continueButton = Nothing
                        , chooseHero = True
                      }
                    , playSound "sfx/draw"
                    )

                -- monster still fighting, no (undefeated) hero fighting, heroes left, choose hero
                [ ( monster_fighting, _ ) :: _, [], _, _ :: _ ] ->
                    ( { model
                        | story = getName monster_fighting model.worldModel ++ " is still standing.  Try another hero."
                        , continueButton = Nothing
                        , chooseHero = True
                      }
                    , Cmd.none
                    )

                -- monster & hero fighting, determine outcome
                [ ( monster_fighting, _ ) :: _, ( hero_fighting, _ ) :: _, _, _ ] ->
                    let
                        recordVictory =
                            Dict.update hero_fighting
                                (Maybe.map (\h -> Just { h | beat = Set.insert monster_fighting h.beat })
                                    >> Maybe.withDefault (Just { beat = Set.singleton monster_fighting, lost = Set.empty })
                                )

                        recordLoss =
                            Dict.update hero_fighting
                                (Maybe.map (\h -> Just { h | lost = Set.insert monster_fighting h.lost })
                                    >> Maybe.withDefault (Just { lost = Set.singleton monster_fighting, beat = Set.empty })
                                )
                    in
                    Maybe.map2
                        (\m_level h_level ->
                            if m_level > h_level then
                                ( { model
                                    | story = getName hero_fighting model.worldModel ++ " is defeated! -1 HP"
                                    , worldModel =
                                        updateWorldModel
                                            [ hero_fighting ++ ".defeated" ]
                                            model.worldModel
                                    , score = model.score - 1
                                    , battleHistory = recordLoss model.battleHistory
                                    , continueButton = Nothing
                                  }
                                , Cmd.batch [ after 1000 ClearBattlefield ]
                                )

                            else
                                ( { model
                                    | story = getName hero_fighting model.worldModel ++ " is victorious! +1 HP"
                                    , worldModel =
                                        updateWorldModel
                                            [ hero_fighting ++ ".victorious"
                                            , monster_fighting
                                                ++ (if hasBonus "Charm of persistent insight" model then
                                                        ".defeated.revealed"

                                                    else
                                                        ".defeated"
                                                   )
                                            ]
                                            model.worldModel
                                    , score = model.score + 1
                                    , battleHistory = recordVictory model.battleHistory
                                    , continueButton = Nothing
                                  }
                                , Cmd.batch [ after 1000 ClearBattlefield ]
                                )
                        )
                        (getStat monster_fighting "level" model.worldModel)
                        (getStat hero_fighting "level" model.worldModel)
                        |> Maybe.withDefault ( { model | story = "error determining winner" }, Cmd.none )

                -- shouldn't ever trigger
                other ->
                    -- let
                    --     x =
                    --         Debug.log "unexpected match" other
                    -- in
                    ( { model | story = "Error in game loop!" }, Cmd.none )

        ClearBattlefield ->
            ( { model
                | worldModel =
                    model.worldModel
                        |> updateWorldModel [ "(*.fighting.victorious).-fighting" ]
                        |> updateWorldModel [ "(*.fighting.defeated).-fighting" ]
              }
            , after 1500 Tick
            )

        AppealForBraveryContinue ->
            ( { model | continueButton = Nothing }
            , Cmd.batch [ after 1500 Tick, after 800 <| PlaySound "sfx/fight" ]
            )

        HeroSelected heroId ->
            let
                worldModel =
                    updateWorldModel [ heroId ++ ".fighting.-preview" ] model.worldModel
            in
            ( { model
                | worldModel = worldModel
                , chooseHero = False
                , story = getName heroId model.worldModel ++ " joins the fight."
                , continueButton = Nothing
              }
            , Cmd.batch [ after 1500 Tick, after 800 <| PlaySound "sfx/fight", playSound "sfx/select" ]
            )

        HeroPreview Nothing ->
            let
                worldModel =
                    updateWorldModel [ "(*.preview).-preview" ] model.worldModel
            in
            ( { model | worldModel = worldModel }, Cmd.none )

        HeroPreview (Just id) ->
            let
                worldModel =
                    updateWorldModel [ id ++ ".preview" ] model.worldModel
            in
            ( { model | worldModel = worldModel }, Cmd.none )

        PlaySound key ->
            ( model, playSound key )


after : Float -> Msg -> Cmd Msg
after ms msg =
    Task.perform (always msg) <| Process.sleep ms


updateWorldModel : List String -> MyWorldModel -> MyWorldModel
updateWorldModel queries worldModel =
    let
        parsedChanges =
            List.foldr
                (\q acc ->
                    RuleParser.parseChanges q
                        |> Result.map (\c -> c :: acc)
                        |> Result.withDefault acc
                )
                []
                queries
    in
    applyChanges parsedChanges "no trigger" worldModel


query : String -> MyWorldModel -> List ( WorldModel.ID, MyEntity )
query q worldModel =
    RuleParser.parseMatcher q
        |> Result.map (\parsedMatcher -> WorldModel.query parsedMatcher worldModel)
        |> Result.withDefault []


assert : String -> MyWorldModel -> Bool
assert q worldModel =
    not <| List.isEmpty <| query q worldModel


view : Model -> Html Msg
view model =
    let
        heroes =
            query "*.hero.!fighting.!defeated.!victorious" model.worldModel

        monsters =
            query "*.monster.!fighting.!defeated" model.worldModel

        fightingHero =
            query "*.hero.fighting" model.worldModel

        previewHero =
            if model.chooseHero then
                query "*.hero.preview" model.worldModel

            else
                []

        fightingMonster =
            query "*.monster.fighting" model.worldModel

        defeated =
            query "*.!fighting.defeated" model.worldModel

        victorious =
            query "*.!fighting.victorious" model.worldModel

        kindPlural id =
            if assert (id ++ ".hero") model.worldModel then
                "heroes"

            else
                "monsters"

        kind id =
            if assert (id ++ ".hero") model.worldModel then
                "hero"

            else
                "monster"

        level id =
            if assert (id ++ ".revealed") model.worldModel then
                getStat id "level" model.worldModel |> Maybe.map String.fromInt |> Maybe.withDefault "error"

            else
                "?"

        imageNum id =
            getStat id "image" model.worldModel |> Maybe.withDefault 1 |> String.fromInt

        characterList attrs =
            List.map (characterCard attrs)

        characterCard attrs ( id, { name } ) =
            div
                (attrs id
                    ++ [ classList
                            [ ( "character", True )
                            , ( "character-" ++ kind id, True )
                            , ( "character-preview", kind id == "hero" && not (List.isEmpty previewHero) )
                            , ( "defeated", assert (id ++ ".defeated") model.worldModel )
                            , ( "victorious", assert (id ++ ".victorious") model.worldModel )
                            ]
                       ]
                )
                [ div
                    [ class "img"
                    , style "background-image" ("url(images/" ++ kindPlural id ++ "/" ++ imageNum id ++ ".png)")
                    ]
                    []
                , div [ class "title" ] [ text name ]
                , div [ class "level" ] [ text <| level id ]
                ]

        chooseBonus =
            model.bonusOptions
                |> List.take 2
                |> List.map
                    (\n ->
                        Dict.get n bonuses
                            |> Maybe.withDefault { name = "error", description = "finding bonus", selectFn = identity, useFn = identity }
                    )
                |> List.map
                    (\{ name, description, selectFn } ->
                        button [ class "pure-button button-primary", onClick (BonusSelected selectFn) ]
                            [ h3 [] [ text name ]
                            , p [] [ text description ]
                            ]
                    )
                |> div [ class "pure-u-1 select-bonus" ]

        spells =
            model.spells
                |> List.map
                    (\n ->
                        Dict.get n bonuses
                            |> Maybe.withDefault { name = "error", description = "finding bonus", selectFn = identity, useFn = identity }
                    )
                |> List.map
                    (\{ name, useFn } ->
                        button [ class "pure-button button-primary", onClick (UseSpell name useFn) ] [ text name ]
                    )
                |> div [ class "spells" ]

        firstOf =
            List.head
                >> Maybe.map (characterCard noHandlers)
                >> Maybe.withDefault (div [ class "hero-placeholder" ] [])

        isStartFight =
            -- works for both HeroSelected and AppealForBraveryContinue
            assert "*.hero.fighting" model.worldModel && model.continueButton == Nothing

        getHistory fn =
            previewHero
                |> List.head
                |> Maybe.andThen (\h -> Dict.get (Tuple.first h) model.battleHistory)
                |> Maybe.map (fn >> Set.toList >> List.map (\id -> query id model.worldModel) >> List.concat)
                |> Maybe.withDefault []

        noHandlers =
            always []

        heroHandlers id =
            [ onMouseEnter <| HeroPreview <| Just id
            , onMouseLeave <| HeroPreview Nothing
            ]
                ++ (if model.chooseHero then
                        [ onClick <| HeroSelected id ]

                    else
                        []
                   )

        conditionalView conds v =
            if List.all identity conds then
                v

            else
                text ""

        showEmpty l =
            if List.isEmpty l then
                [ text "--" ]

            else
                l
    in
    div [ class "game" ]
        -- [ NarrativeEngine.Debug.debugBar UpdateDebugSearchText model.worldModel model.debug
        [ div [ class "title-main-wrapper" ] [ h3 [ class "title-main" ] [ text "Deduction Quest" ] ]
        , div [ class "pure-g top" ]
            [ div
                [ class "pure-u-1-4 characters characters-heroes"
                , classList [ ( "select", model.chooseHero ) ]
                ]
                (characterList heroHandlers heroes)
            , div [ class "pure-u-1-2 " ]
                [ div [ class "pure-g battlefield", classList [ ( "start-fight", isStartFight ) ] ]
                    [ conditionalView [ model.chooseBonus ] chooseBonus
                    , conditionalView [ not model.chooseBonus ] <| div [ class "pure-u-1-2 fighting-zone" ] [ firstOf (previewHero ++ fightingHero) ]
                    , conditionalView [ not model.chooseBonus ] <| div [ class "pure-u-1-2 fighting-zone" ] [ firstOf fightingMonster ]
                    , conditionalView
                        [ model.chooseHero
                        , hasBonus "Relic of photographic memory" model
                        , not <| List.isEmpty previewHero
                        , not <| List.isEmpty <| getHistory .beat ++ getHistory .lost
                        ]
                      <|
                        div [ class "pure-u battle-history" ]
                            [ h3 [] [ text "Defeated:" ]
                            , div [ class "history-characters" ] <| showEmpty <| characterList noHandlers (getHistory .beat)
                            , h3 [] [ text "Lost to:" ]
                            , div [ class "history-characters" ] <| showEmpty <| characterList noHandlers (getHistory .lost)
                            ]
                    ]
                , div [ class "story", classList [ ( "hide", String.isEmpty model.story ) ] ]
                    [ Markdown.toHtml [] model.story
                    , Maybe.map (\( prompt, msg ) -> button [ class "pure-button button-primary", onClick msg ] [ text prompt ]) model.continueButton |> Maybe.withDefault (text "")
                    ]
                ]
            , div [ class "pure-u-1-4 characters characters-monsters" ] (characterList noHandlers monsters)
            ]
        , div [ class "bottom" ]
            [ div [ class "previous-battles" ] (characterList noHandlers (defeated ++ victorious))
            , conditionalView [ model.chooseHero ] spells
            , div [ class "hp" ] [ text <| "HP " ++ String.fromInt model.score ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = \f -> initialModel
        , view =
            \model ->
                case model.parseErrors of
                    Just errors ->
                        { title = "Parse Errors"
                        , body = [ SyntaxHelpers.parseErrorsView errors ]
                        }

                    Nothing ->
                        { title = "Deduction Quest"
                        , body = [ view model ]
                        }
        , update = update
        , subscriptions = subscriptions
        }
