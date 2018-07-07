module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)


---- MODEL ----


type alias Str =
    Int


type alias Agi =
    Int


type alias PlayerStatus =
    { level : Int, hp : Int, mp : Int, maxHp : Int, maxMp : Int, str : Str, agi : Agi }


type alias EnemtyStatus =
    { hp : Int, maxHp : Int, str : Str, agi : Agi }


type alias Model =
    { playerStatus : PlayerStatus, enemtyStatus : EnemtyStatus }


init : ( Model, Cmd Msg )
init =
    ( { playerStatus = level1
      , enemtyStatus = slime
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "./src/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
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
calcAttackDamage : Str -> Agi -> ( Int, Int )
calcAttackDamage pStr eAgi =
    ( pStr - (eAgi // 2) % 4, pStr - (eAgi // 2) % 2 )



---- DATA ----


level1 : PlayerStatus
level1 =
    PlayerStatus 1 16 0 16 0 4 4


slime : EnemtyStatus
slime =
    EnemtyStatus 3 3 5 6



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
