module Main exposing (..)

import Html exposing (Html, text, div, h1, img, ul, li, input, span, a, p)
import Html.Attributes exposing (src, type_, value, id, class)
import Html.Events exposing (onClick)
import Random
import Html.Keyed as Keyed


---- MODEL ----


type WindowMessage
    = PlayerMessage String String
    | EnemyMessage String String


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
    , currentWindowMessages : List WindowMessage
    , windowMessageCount : Int
    , playableCharacter : Character
    }


init : ( Model, Cmd Msg )
init =
    ( { playerStatus = level1
      , enemyStatus = slime
      , currentWindowMessages = encountMessage "おやじヤギ"
      , windowMessageCount = 0
      , playableCharacter = Player
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = PlayerDiceRole
    | EnemyDiceRole
    | PlayerAttack Dice DPoint
    | EnemyAttack Dice DPoint


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ playerStatus, enemyStatus, windowMessageCount } as model) =
    case msg of
        PlayerDiceRole ->
            let
                pDamage =
                    calcAttackDamage playerStatus.str enemyStatus.agi

                randomDamages =
                    Random.map2
                        (,)
                        dice32
                        (Random.int pDamage.minDamage pDamage.maxDamage)
            in
                model ! [ Random.generate (\( d32, pd ) -> PlayerAttack d32 pd) randomDamages ]

        EnemyDiceRole ->
            let
                eDamage =
                    calcAttackDamage enemyStatus.str playerStatus.agi

                randomDamages =
                    Random.map2
                        (,)
                        dice32
                        (Random.int eDamage.minDamage eDamage.maxDamage)
            in
                model ! [ Random.generate (\( d32, ed ) -> EnemyAttack d32 ed) randomDamages ]

        PlayerAttack d32 pDamage ->
            let
                damagePoint =
                    if d32 == 1 then
                        0
                    else
                        pDamage

                updatedEnemyStatus =
                    { enemyStatus | hp = enemyStatus.hp - damagePoint }

                nextMessage =
                    playerAttakckMessage "えるむ" "おやじヤギ" pDamage
            in
                { model
                    | enemyStatus = updatedEnemyStatus
                    , playableCharacter = Enemy
                    , currentWindowMessages = nextMessage
                    , windowMessageCount = windowMessageCount + List.length nextMessage
                }
                    ! []

        EnemyAttack d32 eDamage ->
            let
                damagePoint =
                    if d32 == 1 then
                        0
                    else
                        eDamage

                updatedPlayerStatus =
                    { playerStatus | hp = playerStatus.hp - damagePoint }

                nextMessage =
                    enemyAttakckMessage "おやじヤギ" "えるむ" eDamage
            in
                { model
                    | playerStatus = updatedPlayerStatus
                    , playableCharacter = Player
                    , currentWindowMessages = nextMessage
                    , windowMessageCount = windowMessageCount + List.length nextMessage
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
view { playerStatus, enemyStatus, currentWindowMessages, windowMessageCount, playableCharacter } =
    div
        [ id "container"
        , class <|
            if playableCharacter == Player then
                "earthquake"
            else
                ""
        ]
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
                , attackCommandView playableCharacter
                , div []
                    [ a [] [ text "にげる" ]
                    , a [] [ text "どうぐ" ]
                    ]
                ]
            ]
        , div [ class "monster" ]
            [ div
                [ class <|
                    "yagi"
                        ++ if playableCharacter == Enemy then
                            " damaged"
                           else
                            ""
                ]
                []
            ]
        , windowMessageView playableCharacter currentWindowMessages windowMessageCount
        ]


attackCommandView : Character -> Html Msg
attackCommandView playableCharacter =
    let
        attack =
            case playableCharacter of
                Player ->
                    a [ onClick PlayerDiceRole ] [ text "たたかう" ]

                Enemy ->
                    a [ onClick EnemyDiceRole ] [ text "たたかう" ]
    in
        div []
            [ attack
            , a [] [ text "じゅもん" ]
            ]


windowMessageView : Character -> List WindowMessage -> Int -> Html Msg
windowMessageView character windowMessages windowMessageCount =
    case character of
        Player ->
            Keyed.node "div" [ class "messages" ] <| messageListView windowMessages windowMessageCount

        Enemy ->
            Keyed.node "div" [ class "messages clickable", onClick EnemyDiceRole ] <| messageListView windowMessages windowMessageCount


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


messageListView : List WindowMessage -> Int -> List ( String, Html Msg )
messageListView windowMessages windowMessageCount =
    let
        pList i left right =
            [ p [ class <| "delay-" ++ toString (i + 1) ] [ text <| left ++ "  " ++ right ] ]
    in
        List.indexedMap
            (\i winMsg ->
                let
                    id =
                        toString (i + windowMessageCount)
                in
                    case winMsg of
                        PlayerMessage left right ->
                            ( id, div [ class "player-message" ] <| pList i left right )

                        EnemyMessage left right ->
                            ( id, div [ class "enemy-message" ] <| pList i left right )
            )
            windowMessages



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


commandMessage : WindowMessage
commandMessage =
    PlayerMessage "コマンド？" ""


encountMessage : String -> List WindowMessage
encountMessage monsterName =
    PlayerMessage (monsterName ++ "が") "あらわれた！"
        :: [ commandMessage ]


playerAttakckMessage : String -> String -> Int -> List WindowMessage
playerAttakckMessage playerName monsterName damage =
    [ PlayerMessage (playerName ++ "の") "こうげき！"
    , PlayerMessage (monsterName ++ "に") (toString damage ++ "ポイントの")
    , PlayerMessage "ダメージを" "あたえた！"
    ]


enemyAttakckMessage : String -> String -> Int -> List WindowMessage
enemyAttakckMessage monsterName playerName damage =
    [ EnemyMessage (monsterName ++ "の") "こうげき！"
    , EnemyMessage (playerName ++ "は") (toString damage ++ "ポイントの")
    , EnemyMessage "ダメージを" "うけた！"
    , commandMessage
    ]


defeatMessage : String -> List WindowMessage
defeatMessage monsterName =
    [ PlayerMessage (monsterName ++ "を") "たおした！" ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
