port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import NarrativeEngine.Core.Rules as Rules
import NarrativeEngine.Core.WorldModel as WorldModel exposing (addTag, emptyLinks, emptyStats, emptyTags, setStat)
import NarrativeEngine.Debug
import NarrativeEngine.Syntax.EntityParser as EntityParser
import NarrativeEngine.Syntax.Helpers as SyntaxHelpers
import NarrativeEngine.Syntax.NarrativeParser as NarrativeParser
import NarrativeEngine.Syntax.RuleParser as RuleParser
import Random
import Random.Extra as Random
import Random.List as Random


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


toEntity : String -> String -> String -> ( String, EntityFields )
toEntity entityString name description =
    ( entityString, { name = name, description = description } )


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
    , started : Bool
    , story : String
    , ruleCounts : Dict String Int
    , debug : NarrativeEngine.Debug.State
    }


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { parseErrors = Nothing
      , worldModel = Dict.empty
      , rules = Dict.empty
      , started = False
      , story = ""
      , ruleCounts = Dict.empty
      , debug = NarrativeEngine.Debug.init
      }
    , Random.generate Randomize (makeLineUp 5)
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
    | Randomize LineUp


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
                    { model | parseErrors = Nothing, rules = Dict.union newRules model.rules }
                        |> (\m ->
                                if m.started then
                                    ( m, Cmd.none )

                                else
                                    update (InteractWith "start") { m | started = True }
                           )

        Randomize lineUp ->
            let
                makeCharacter tag =
                    List.map
                        (\{ name, level } ->
                            ( name
                            , { tags = emptyTags
                              , stats = emptyStats
                              , links = emptyLinks
                              , name = name
                              , description = ""
                              }
                                |> addTag tag
                            )
                        )

                entities =
                    []
                        ++ makeCharacter "hero" lineUp.heros
                        ++ makeCharacter "monster" lineUp.monsters
                        |> Dict.fromList
            in
            ( { model | worldModel = Dict.union entities model.worldModel }, Cmd.none )


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
            query "*.hero.!fighting.!defeated" model.worldModel

        monsters =
            query "*.monster.!fighting.!defeated" model.worldModel

        fightingHero =
            query "*.hero.fighting" model.worldModel

        fightingMonster =
            query "*.monster.fighting" model.worldModel

        defeated =
            query "*.defeated" model.worldModel

        listOf =
            ul [] << List.map (\( _, { name } ) -> li [] [ text name ])

        firstOf =
            List.head
                >> Maybe.map (\( _, { name } ) -> div [] [ text name ])
                >> Maybe.withDefault (div [] [ text "empty" ])
    in
    div [ style "width" "90%", style "margin" "auto" ] <|
        [ NarrativeEngine.Debug.debugBar UpdateDebugSearchText model.worldModel model.debug
        , div [ class "pure-g" ]
            [ div [ class "pure-u-1-4" ] [ h3 [] [ text "Heros" ], listOf heros ]
            , div [ class "pure-u-1-2" ]
                [ div [ class "pure-g" ]
                    [ div [ class "pure-u-1-2" ] [ firstOf fightingHero ]
                    , div [ class "pure-u-1-2" ] [ firstOf fightingMonster ]
                    ]
                ]
            , div [ class "pure-u-1-4" ] [ h3 [] [ text "Monsters" ], listOf monsters ]
            ]
        , div [ class "pure-g" ]
            [ div [ class "pure-u" ] [ listOf defeated ]
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
