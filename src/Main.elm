module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra as List
import Random exposing (Generator)
import Random.Extra as Random
import Random.List
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { showView : ShowView
    , playModel : PlayModel
    , setting : Setting
    , testString : String
    }


type alias PlayModel =
    { mines : List Int
    , opened : List Int
    , time : Int
    , playState : PlayState
    }


type PlayState
    = Init
    | Play
    | Win
    | Lose


type alias Setting =
    { width : Int
    , height : Int
    , mine : Int
    }


type ShowView
    = HowToView
    | PlayView
    | SettingView


init : () -> ( Model, Cmd Msg )
init _ =
    let
        defaultSetting =
            Setting 9 9 10
    in
    ( { playModel = initPlayModel
      , setting = defaultSetting
      , showView = PlayView
      , testString = ""
      }
    , Cmd.none
    )


initPlayModel : PlayModel
initPlayModel =
    PlayModel [] [] 0 Init



-- UPDATE


type Msg
    = ViewChange ShowView
    | GotPlayMsg PlayMsg
    | GotSettingMsg SettingMsg


type PlayMsg
    = Reset
    | ClickCell Int
    | GotMineList (List Int)
    | Tick


type SettingMsg
    = SetEasy
    | SetNormal
    | SetHard
    | IncrementWidth
    | DecrementWidth
    | IncrementHeight
    | DecrementHeight
    | IncrementMine
    | DecrementMine


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ViewChange showView ->
            ( { model | showView = showView }
            , Cmd.none
            )

        GotPlayMsg subMsg ->
            let
                ( playModel, cmd ) =
                    playModelUpdate subMsg model
            in
            ( { model | playModel = playModel }
            , Cmd.map GotPlayMsg cmd
            )

        GotSettingMsg subMsg ->
            ( { model
                | setting = settingUpdate subMsg model.setting
                , playModel = initPlayModel
              }
            , Cmd.none
            )


playModelUpdate : PlayMsg -> Model -> ( PlayModel, Cmd PlayMsg )
playModelUpdate msg model =
    let
        playModel =
            model.playModel
    in
    case msg of
        Reset ->
            ( initPlayModel
            , Cmd.none
            )

        ClickCell offset ->
            if playModel.playState == Init then
                ( { playModel | opened = offset :: playModel.opened }
                , Random.generate
                    GotMineList
                    (mineGenerator model.setting offset)
                )

            else
                ( { playModel
                    | opened = offset :: playModel.opened
                    , playState =
                        if List.member offset playModel.mines then
                            Lose

                        else
                            Play
                  }
                , Cmd.none
                )

        Tick ->
            ( { playModel | time = playModel.time + 1 }
            , Cmd.none
            )

        GotMineList list ->
            ( { playModel
                | mines = list
                , playState = Play
              }
            , Cmd.none
            )


settingUpdate : SettingMsg -> Setting -> Setting
settingUpdate msg model =
    case msg of
        SetEasy ->
            Setting 9 9 10

        SetNormal ->
            Setting 16 16 40

        SetHard ->
            Setting 30 16 99

        IncrementWidth ->
            { model | width = model.width + 1 }

        DecrementWidth ->
            { model | width = model.width - 1 }

        IncrementHeight ->
            { model | height = model.height + 1 }

        DecrementHeight ->
            { model | height = model.height - 1 }

        IncrementMine ->
            { model | mine = model.mine + 1 }

        DecrementMine ->
            { model | mine = model.mine - 1 }



-- VIEW


{-| Element Msg to Html Msg
-}
view : Model -> Html Msg
view model =
    layoutWith
        { options = [ focusStyle <| FocusStyle Nothing Nothing Nothing ] }
        []
        (viewElement model)


viewElement : Model -> Element Msg
viewElement model =
    column
        [ width fill
        , height <| shrink
        ]
        [ menuView model
        , fieldView model
        , el [ centerX ] (text model.testString)
        ]


menuView : Model -> Element Msg
menuView model =
    let
        menuButton text_ showView =
            Input.button
                [ width <| minimum 100 shrink
                , height fill
                , Font.center
                , padding 5
                , Font.size 16
                , alignRight
                , if showView == model.showView then
                    Font.color (rgb255 0 192 0)

                  else
                    Font.color (rgb255 255 255 255)
                , if showView == model.showView then
                    Background.color (rgb255 255 255 255)

                  else
                    Background.color (rgb255 0 192 0)
                ]
                { onPress = Just <| ViewChange showView
                , label = el [ centerX, centerY ] (text text_)
                }
    in
    column
        [ width fill
        , height <| px 100
        , Background.color (rgb255 0 192 0)
        ]
        [ el
            [ Font.color (rgb255 255 255 255)
            , Font.bold
            , height fill
            , width <| maximum 700 fill
            , centerX
            , centerY
            , Font.size 24
            , padding 10
            ]
            (el [ alignLeft, centerY ] <| text "Elm-minesweeper")
        , row
            [ height <| px 30
            , width <| maximum 700 fill
            , centerX
            , centerY
            ]
            [ menuButton "How to" HowToView
            , menuButton "Play" PlayView
            , menuButton "Setting" SettingView
            ]
        ]


fieldView : Model -> Element Msg
fieldView model =
    case model.showView of
        HowToView ->
            howToView

        PlayView ->
            playView model

        SettingView ->
            settingView model


howToView : Element msg
howToView =
    el [ centerX ] (text "How To View")


playView : Model -> Element Msg
playView model =
    let
        cell ri ci =
            let
                offset =
                    ri + ci
            in
            Input.button
                [ width <| px 30
                , height <| px 30
                , if List.member offset model.playModel.opened then
                    Background.color (rgb255 127 127 127)

                  else
                    fieldColor.background.base
                ]
                { onPress = Just (GotPlayMsg <| ClickCell offset)
                , label = none
                }

        cellsRow count index =
            row [ spacing 5 ] <| List.initialize count (\i -> cell i index)

        cellsGrid widthCount heightCount =
            column
                [ width shrink
                , height shrink
                , spacing 5
                , Background.color (rgb255 255 255 255)
                ]
                (List.initialize
                    heightCount
                    (\i -> cellsRow widthCount (i * heightCount))
                )
    in
    column
        [ centerX
        , padding 20
        , spacing 5
        ]
        [ row [ width fill, height <| px 10, spacing 5 ]
            [ el
                [ width <| px 100
                , height fill
                , alignLeft
                ]
                (el
                    [ centerY
                    , alignLeft
                    , Font.size 12
                    ]
                    (text "mimes")
                )
            , el
                [ width <| px 100
                , height fill
                , alignRight
                ]
                (el
                    [ centerY
                    , alignLeft
                    , Font.size 12
                    ]
                    (text "time")
                )
            ]
        , row [ width fill, spacing 5, height <| px 50 ]
            [ el
                [ width <| px 100
                , height fill
                , alignLeft
                , fieldColor.background.base
                , fieldColor.font.base
                ]
                (el
                    [ padding 10
                    , alignRight
                    , centerY
                    ]
                    (text "10")
                )
            , el
                [ width <| px 100
                , height fill
                , centerX
                , onClick (GotPlayMsg Reset)
                , fieldColor.background.base
                , fieldColor.font.base
                ]
                (el
                    [ padding 10
                    , centerX
                    , centerY
                    ]
                    (text
                        (case model.playModel.playState of
                            Init ->
                                "Reset"

                            Play ->
                                "Reset"

                            Win ->
                                "Win"

                            Lose ->
                                "Lose"
                        )
                    )
                )
            , el
                [ width <| px 100
                , height fill
                , alignRight
                , fieldColor.background.base
                , fieldColor.font.base
                ]
                (el
                    [ padding 10
                    , alignRight
                    , centerY
                    ]
                    (text <| String.fromInt model.playModel.time)
                )
            ]
        , cellsGrid model.setting.width model.setting.height
        ]


settingView : Model -> Element Msg
settingView model =
    let
        presetButton msg text_ =
            Input.button
                [ fieldColor.background.base
                , fieldColor.font.base
                , width fill
                , height <| px 50
                , Border.width 5
                , Border.color <| rgb255 63 63 127
                , Border.rounded 5
                , Element.mouseOver
                    [ fieldColor.background.focused
                    , fieldColor.font.focused
                    ]
                ]
                { onPress = Just <| GotSettingMsg msg
                , label = el [ centerX, centerY ] (text text_)
                }

        manualButton msg text_ =
            Input.button
                [ fieldColor.background.base
                , fieldColor.font.base
                , width <| px 40
                , height <| px 40
                , Border.width 5
                , Border.color <| rgb255 63 63 127
                , Element.mouseOver
                    [ fieldColor.background.focused
                    , fieldColor.font.focused
                    ]
                ]
                { onPress = Just <| GotSettingMsg msg
                , label =
                    el
                        [ Font.size 30
                        , centerX
                        , centerY
                        ]
                        (text text_)
                }

        manualRow text_ decrementMsg value incrementMsg =
            row [ alignRight, spacing 10 ]
                [ text text_
                , manualButton decrementMsg "-"
                , el
                    [ width <| px 80
                    , height fill
                    , Border.width 5
                    , Border.color <| rgb255 63 63 127
                    ]
                    (el
                        [ alignRight
                        , centerY
                        , padding 5
                        ]
                        (text <| String.fromInt value)
                    )
                , manualButton incrementMsg "+"
                ]
    in
    column
        [ width <| px 400
        , padding 30
        , spacing 30
        , centerX
        ]
        [ column
            [ width fill
            , spacing 10
            ]
            [ el [ width fill, Font.size 30 ] (text "Preset")
            , row
                [ spacing 10
                , width fill
                , centerY
                ]
                [ presetButton SetEasy "EASY"
                , presetButton SetNormal "NORMAL"
                , presetButton SetHard "HARD"
                ]
            ]
        , column
            [ width fill
            , spacing 10
            ]
            [ el [ width fill, Font.size 30 ] (text "Manual")
            , manualRow "WIDTH" DecrementWidth model.setting.width IncrementWidth
            , manualRow "HEIGHT" DecrementHeight model.setting.height IncrementHeight
            , manualRow "MINE" DecrementMine model.setting.mine IncrementMine
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playModel.playState == Play then
        Time.every 1000 (\_ -> GotPlayMsg Tick)

    else
        Sub.none



-- RANDOM


mineGenerator : Setting -> Int -> Generator (List Int)
mineGenerator setting first =
    Random.List.choices
        setting.mine
        (List.range 0 (setting.width * setting.height) |> List.remove first)
        |> Random.map Tuple.first



-- HELPER


menuColor =
    { background =
        { base = Background.color <| rgb255 0 192 0
        , selected = Background.color <| rgb255 255 255 255
        }
    , font =
        { base = Font.color <| rgb255 255 255 255
        , selected = Font.color <| rgb255 0 192 0
        }
    }


fieldColor =
    { background =
        { base = Background.color <| rgb255 63 63 127
        , focused = Background.color <| rgb255 255 255 255
        }
    , font =
        { base = Font.color <| rgb255 255 255 255
        , focused = Font.color <| rgb255 63 63 127
        }
    }
