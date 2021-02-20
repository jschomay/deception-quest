port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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
    , story : String
    , ruleCounts : Dict String Int
    , debug : NarrativeEngine.Debug.State
    , chooseHero : Bool
    , lineUpCount : Int
    }


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { parseErrors = Nothing
      , worldModel = Dict.empty
      , rules = Dict.empty
      , story = ""
      , ruleCounts = Dict.empty
      , debug = NarrativeEngine.Debug.init
      , chooseHero = False
      , lineUpCount = 3
      }
    , Random.generate StartRound (makeLineUp 3)
    )


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
        ]
    , suffix1s =
        [ " of doom"
        , " of disreputable company"
        , " from Hell"
        , " infected with scurvy"
        , " of limited intellect"
        , " from the house next door"
        , " from your nightmare"
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
        range =
            List.range 1 n

        monsterLevels =
            range |> List.map ((*) 10) |> Random.shuffle

        heroLevels =
            range |> List.map ((*) 10 >> (+) 5) |> Random.shuffle

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
                                        (sometimesTakeName 0.5 prefixs)
                                        (sometimesTakeName 1 mains)
                                        (sometimesTakeName 0.3 suffix1s)
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
                monsterLevels

        heros =
            Random.map2
                (List.map2 Character)
                (randomNames heroNameOptions)
                heroLevels
    in
    Random.map2 LineUp heros monsters


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
    { name : String, level : Int }


type alias LineUp =
    { heros : List Character
    , monsters : List Character
    }


type Msg
    = InteractWith WorldModel.ID
    | UpdateDebugSearchText String
    | AddEntities (EntityParser.ParsedWorldModel EntityFields)
    | AddRules (RuleParser.ParsedRules RuleFields)
    | StartRound LineUp
    | Tick
    | HeroSelected WorldModel.ID


type alias EntitySpec =
    { description : String, entity : String, name : String }


type alias RuleSpec =
    { rule : String, rule_id : String, narrative : String }


port addEntities : (List EntitySpec -> msg) -> Sub msg


port addRules : (List RuleSpec -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ addEntities <| AddEntities << parseEntities
        , addRules <| AddRules << parseRules
        ]


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

        StartRound lineUp ->
            let
                toId name =
                    name
                        |> String.replace " " "_"
                        |> String.replace "," "_"
                        |> String.replace "'" "_"
                        |> String.replace "-" "_"
                        |> String.replace "\"" "_"

                makeCharacter tag =
                    List.map
                        (\{ name, level } ->
                            ( toId name
                            , { tags = emptyTags
                              , stats = emptyStats
                              , links = emptyLinks
                              , name = name
                              , description = ""
                              }
                                |> addTag tag
                                |> setStat "level" level
                            )
                        )

                entities =
                    []
                        ++ makeCharacter "hero" lineUp.heros
                        ++ makeCharacter "monster" lineUp.monsters
                        |> Dict.fromList
            in
            ( { model | worldModel = entities }, after 1000 Tick )

        Tick ->
            case
                [ query "*.monster.fighting" model.worldModel
                , query "*.hero.fighting" model.worldModel
                , query "*.monster.!fighting.!defeated" model.worldModel
                , query "*.hero.!fighting.!defeated.!victorious" model.worldModel
                ]
            of
                -- no hero fighting, no heros left, round lost, restart lineup
                [ _, [], _, [] ] ->
                    let
                        x =
                            Debug.log "" "round lost, try again"

                        worldModel =
                            updateWorldModel
                                [ "(*.hero).-fighting.-defeated.-victorious"
                                , "(*.monster).-fighting.-defeated"
                                ]
                                model.worldModel
                    in
                    ( { model | worldModel = worldModel }, after 1000 Tick )

                -- no monster fighting, no monsters left, round won, level up, queue start round
                [ [], _, [], _ ] ->
                    let
                        x =
                            Debug.log "" "you win!  level up"
                    in
                    -- TODO build in a delay
                    ( { model | lineUpCount = model.lineUpCount + 1 }
                    , Random.generate StartRound (makeLineUp (model.lineUpCount + 1))
                    )

                -- no monster fighting, monsters available, monster attacks
                [ [], _, ( monster_up_next, _ ) :: _, _ ] ->
                    let
                        x =
                            Debug.log "" "a monster attacks"

                        worldModel =
                            updateWorldModel [ monster_up_next ++ ".fighting" ] model.worldModel
                    in
                    ( { model | worldModel = worldModel }, after 1000 Tick )

                -- monster & hero fighting, determine outcome
                [ ( monster_fighting, _ ) :: _, ( hero_fighting, _ ) :: _, _, _ ] ->
                    let
                        x =
                            Debug.log "" "ready... fight!"

                        ( outcomeMsg, worldModel ) =
                            Maybe.map2
                                (\m_level h_level ->
                                    if m_level > h_level then
                                        ( "monster wins!"
                                        , updateWorldModel
                                            [ hero_fighting ++ ".-fighting.defeated" ]
                                            model.worldModel
                                        )

                                    else
                                        ( "hero is victorious!"
                                        , updateWorldModel
                                            [ hero_fighting ++ ".-fighting.victorious"
                                            , monster_fighting ++ ".-fighting.defeated"
                                            ]
                                            model.worldModel
                                        )
                                )
                                (getStat monster_fighting "level" model.worldModel)
                                (getStat hero_fighting "level" model.worldModel)
                                |> Maybe.withDefault ( "oops", model.worldModel )

                        y =
                            Debug.log "" outcomeMsg
                    in
                    ( { model | worldModel = worldModel }, after 1000 Tick )

                -- monster fighting & no hero fighting, heros available, queue hero select
                [ monster_fighting :: _, [], _, hero_up_next :: _ ] ->
                    let
                        x =
                            Debug.log "" "choose a hero to fight"
                    in
                    ( { model | chooseHero = True }, Cmd.none )

                other ->
                    let
                        x =
                            Debug.log "unexpected match" other
                    in
                    ( model, Cmd.none )

        HeroSelected id ->
            let
                worldModel =
                    updateWorldModel [ id ++ ".fighting" ] model.worldModel
            in
            ( { model | worldModel = worldModel, chooseHero = False }, after 1000 Tick )



-- GAME LOOP
-- round start, generate heros and monsters, queue monster attacks
-- monster attacks (no monsters fighting) send monster to fight, wait for pick hero
-- pick hero (1 monster fighting, no heros) send hero to fight, querue fight
-- fight (1 monster, 1 hero) compare stats, determine outcome
-- -- hero wins (hero > monster) monster defeated, hero victorious
-- -- -- monsters left, queue monster attacks
-- -- -- no monsters left, queue level up
-- -- monster wins (monster > hero) hero defeated
-- -- -- heros left, queue pick hero
-- -- -- no heros left, queue restart round
-- restart round, lose text, reset lineup
-- level up, win text, bonuses, increment lineup count, queue round start


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
        heros =
            query "*.hero.!fighting.!defeated.!victorious" model.worldModel

        monsters =
            query "*.monster.!fighting.!defeated" model.worldModel

        fightingHero =
            query "*.hero.fighting" model.worldModel

        fightingMonster =
            query "*.monster.fighting" model.worldModel

        defeated =
            query "*.defeated" model.worldModel

        victorious =
            query "*.victorious" model.worldModel

        listOf l maybeMsg =
            let
                handler id =
                    maybeMsg |> Maybe.map (\m -> [ onClick <| m id ]) |> Maybe.withDefault []
            in
            ul [] <| List.map (\( id, { name } ) -> li ([] ++ handler id) [ text name ]) l

        firstOf =
            List.head
                >> Maybe.map (\( _, { name } ) -> div [] [ text name ])
                >> Maybe.withDefault (div [] [ text "empty" ])

        heroHandler =
            if model.chooseHero then
                Just HeroSelected

            else
                Nothing
    in
    div [ style "width" "90%", style "margin" "auto" ] <|
        -- [ NarrativeEngine.Debug.debugBar UpdateDebugSearchText model.worldModel model.debug
        [ div [ class "pure-g" ]
            [ div [ class "pure-u-1-4" ] [ h3 [] [ text "Heros" ], listOf heros heroHandler ]
            , div [ class "pure-u-1-2" ]
                [ h3 [] [ text "Current battle" ]
                , div [ class "pure-g" ]
                    [ div [ class "pure-u-1-2" ] [ firstOf fightingHero ]
                    , div [ class "pure-u-1-2" ] [ firstOf fightingMonster ]
                    ]
                ]
            , div [ class "pure-u-1-4" ] [ h3 [] [ text "Monsters" ], listOf monsters Nothing ]
            ]
        , h3 [] [ text "Previous battles" ]
        , div [ class "pure-g" ]
            [ div [ class "pure-u" ] [ listOf (defeated ++ victorious) Nothing ]
            ]
        ]


entityView : ( WorldModel.ID, { a | name : String } ) -> Html Msg
entityView ( id, { name } ) =
    li [ onClick <| InteractWith id, style "cursor" "pointer" ] [ text name ]


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
