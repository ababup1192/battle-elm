module Main exposing (..)

import Html exposing (Html, text, div, h1, img, ul, li, input, span, a, p)
import Html.Attributes exposing (src, type_, value, id, class)
import Html.Events exposing (onClick)
import Random


---- MODEL ----


type alias Str =
    Int


type alias Agi =
    Int


type alias Dice =
    Int


type alias DPoint =
    Int


type Character
    = Player
    | Enemy


type alias Damage =
    { minDamage : DPoint, maxDamage : DPoint }


type alias PlayerStatus =
    { level : Int, hp : Int, mp : Int, maxHp : Int, maxMp : Int, str : Str, agi : Agi }


type alias EnemyStatus =
    { hp : Int, maxHp : Int, str : Str, agi : Agi }


type alias Model =
    { playerStatus : PlayerStatus
    , enemyStatus : EnemyStatus
    , currentPlayerDamage : DPoint
    , currentEnemtyDamage : DPoint
    , preemptiveCharacter : Character
    }


init : ( Model, Cmd Msg )
init =
    ( { playerStatus = level1
      , enemyStatus = slime
      , currentPlayerDamage = 0
      , currentEnemtyDamage = 0
      , preemptiveCharacter = Player
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Attack
    | NewDamage Dice Dice DPoint DPoint


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ playerStatus, enemyStatus } as model) =
    case msg of
        Attack ->
            let
                pDamage =
                    calcAttackDamage playerStatus.str enemyStatus.agi

                eDamage =
                    calcAttackDamage enemyStatus.str playerStatus.agi

                randomDamages =
                    Random.map4
                        (,,,)
                        dice100
                        dice32
                        (Random.int pDamage.minDamage pDamage.maxDamage)
                        (Random.int eDamage.minDamage eDamage.maxDamage)
            in
                model ! [ Random.generate (\( d100, d32, pd, ed ) -> NewDamage d100 d32 pd ed) randomDamages ]

        NewDamage d100 d32 pDamage eDamage ->
            let
                preemptiveCharacter =
                    if d100 < floor (calcPreemptiveStrike playerStatus.agi enemyStatus.agi * 100) then
                        Player
                    else
                        Enemy

                currentPlayerDamage =
                    if d32 == 1 then
                        0
                    else
                        pDamage

                updatedPlayerStatus =
                    { playerStatus | hp = playerStatus.hp - eDamage }

                updatedEnemyStatus =
                    { enemyStatus | hp = enemyStatus.hp - currentPlayerDamage }
            in
                { model
                    | playerStatus = updatedPlayerStatus
                    , enemyStatus = updatedEnemyStatus
                    , currentPlayerDamage = currentPlayerDamage
                    , currentEnemtyDamage = eDamage
                    , preemptiveCharacter = preemptiveCharacter
                }
                    ! []


dice32 : Random.Generator Dice
dice32 =
    Random.int 1 32


dice100 : Random.Generator Dice
dice100 =
    Random.int 1 100



---- VIEW ----


view : Model -> Html Msg
view { playerStatus, enemyStatus } =
    div [ id "container" ]
        [ div [ class "header" ]
            [ div [ class "status" ]
                [ span []
                    [ text "ステータス" ]
                , div [ class "status-row" ]
                    [ span [] [ text "レベル" ]
                    , span [] [ text "30" ]
                    ]
                , div [ class "status-row" ]
                    [ span [] [ text "HP" ]
                    , span [] [ text <| toString playerStatus.hp ]
                    ]
                , div [ class "status-row" ]
                    [ span [] [ text "MP" ]
                    , span [] [ text "157" ]
                    ]
                ]
            , div [ class "command" ]
                [ span []
                    [ text "コマンド" ]
                , div []
                    [ a [ onClick Attack ] [ text "たたかう" ]
                    , a [] [ text "じゅもん" ]
                    ]
                , div []
                    [ a [] [ text "にげる" ]
                    , a [] [ text "どうぐ" ]
                    ]
                ]
            ]
        , div [ class "monster" ] []
        , div [ class "messages" ]
            [ div [ class "log" ]
                [ p [] [text "test"]
                ]
            ]
        ]



{-
   div []
       [ input [ type_ "button", value "attack", onClick Attack ] []
       , playerStatusView playerStatus
       , enemyStatusView enemyStatus
       ]
-}


playerStatusView : PlayerStatus -> Html Msg
playerStatusView { level, hp, mp } =
    ul []
        [ li [] [ text <| toString level ]
        , li [] [ text <| toString hp ]
        , li [] [ text <| toString mp ]
        ]


enemyStatusView : EnemyStatus -> Html Msg
enemyStatusView { hp } =
    ul []
        [ li [] [ text <| toString hp ]
        ]



---- UTIL ----


{-| 先制確率計算
return: 確率
-}
calcPreemptiveStrike : Agi -> Agi -> Float
calcPreemptiveStrike pAgi eAgi =
    toFloat (pAgi * 4) / (toFloat <| pAgi * 4 + eAgi)


{-| ダメージ計算(与・被)
return: (最小ダメージ, 最大ダメージ)
-}
calcAttackDamage : Str -> Agi -> Damage
calcAttackDamage str agi =
    Damage (str - (agi // 2) % 4) (str - (agi // 2) % 2)



---- DATA ----


level1 : PlayerStatus
level1 =
    PlayerStatus 1 16 0 16 0 4 4


slime : EnemyStatus
slime =
    EnemyStatus 3 3 5 6



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
