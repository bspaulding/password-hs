port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Json.Encode as E


port sendMessage : String -> Cmd msg


port receiveMessage : (String -> msg) -> Sub msg


createRoom : Cmd msg
createRoom =
    E.object [ ( "type", E.string "create-room" ) ]
        |> E.encode 0
        |> sendMessage


joinRoom : RoomId -> Cmd msg
joinRoom roomId =
    E.object [ ( "type", E.string "join-room" ), ( "payload", E.string roomId ) ]
        |> E.encode 0
        |> sendMessage


startGame : Cmd msg
startGame =
    E.object [ ( "type", E.string "start-game" ) ]
        |> E.encode 0
        |> sendMessage


submitClue : String -> Cmd msg
submitClue clue =
    E.object [ ( "type", E.string "submit-clue" ), ( "payload", E.string clue ) ]
        |> E.encode 0
        |> sendMessage


submitGuess : String -> Cmd msg
submitGuess clue =
    E.object [ ( "type", E.string "submit-guess" ), ( "payload", E.string clue ) ]
        |> E.encode 0
        |> sendMessage


socketDecoder : D.Decoder WSMessage
socketDecoder =
    D.field "type" D.string
        |> D.andThen wsMessageDecoder


wsMessageDecoder : String -> D.Decoder WSMessage
wsMessageDecoder type_ =
    case type_ of
        "IdentifyConnection" ->
            D.map IdentifyConnection (D.field "connId" D.string)

        "ErrorResponse" ->
            D.map ErrorResponse (D.field "err" D.string)

        "CreateRoomResponse" ->
            D.map CreateRoomResponse (D.field "roomId" D.string)

        "JoinedRoom" ->
            D.map2 PlayerJoinedRoom (D.field "connId" D.string) (D.field "name" D.string)

        "JoinRoomResponse" ->
            D.map2 JoinedRoomResponse
                (D.field "roomId" D.string)
                (D.field "playerNamesById" (D.dict D.string))

        "NewWordResponse" ->
            D.map NewWordResponse (D.field "word" D.string)

        "PlayerNameChanged" ->
            D.map2 PlayerNameChanged (D.field "connId" D.string) (D.field "name" D.string)

        "GameUpdated" ->
            D.succeed GameUpdated
                |> DP.requiredAt [ "payload", "teamA" ] (D.list D.string)
                |> DP.requiredAt [ "payload", "teamB" ] (D.list D.string)
                |> DP.requiredAt [ "payload", "teamAGuesser" ] D.string
                |> DP.requiredAt [ "payload", "teamBGuesser" ] D.string
                |> DP.requiredAt [ "payload", "teamAClueGiver" ] D.string
                |> DP.requiredAt [ "payload", "teamBClueGiver" ] D.string
                |> DP.requiredAt [ "payload", "word" ] D.string
                |> DP.requiredAt [ "payload", "clues" ] (D.list D.string)
                |> DP.requiredAt [ "payload", "guesses" ] (D.list D.string)
                |> DP.requiredAt [ "payload", "teamAScore" ] D.int
                |> DP.requiredAt [ "payload", "teamBScore" ] D.int
                |> DP.requiredAt [ "payload", "possession" ] possessionDecoder

        _ ->
            D.fail <| "Unknown message type '" ++ type_ ++ "'"


type Possession
    = TeamA
    | TeamB


possessionDecoder : D.Decoder Possession
possessionDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "TeamA" ->
                        D.succeed TeamA

                    "TeamB" ->
                        D.succeed TeamB

                    _ ->
                        D.fail <| "Couldn't get a Possession from '" ++ s ++ "'"
            )


type alias RoomId =
    String


type alias ConnId =
    String


type WSMessage
    = ErrorResponse String
    | IdentifyConnection ConnId
    | CreateRoomResponse RoomId
    | PlayerJoinedRoom ConnId String
    | JoinedRoomResponse RoomId (Dict ConnId String)
    | PlayerNameChanged ConnId String
    | NewWordResponse String
    | GameUpdated (List ConnId) (List ConnId) ConnId ConnId ConnId ConnId String (List String) (List String) Int Int Possession



---- MODEL ----


type alias Player =
    { id : ConnId
    , name : String
    }


type alias PasswordGame =
    { teamA : List ConnId
    , teamB : List ConnId
    , teamAClueGiver : ConnId
    , teamBClueGiver : ConnId
    , teamAGuesser : ConnId
    , teamBGuesser : ConnId
    , word : String
    , clues : List String
    , guesses : List String
    , teamAScore : Int
    , teamBScore : Int
    , possession : Possession
    }


type alias Model =
    { connId : Maybe String
    , name : String
    , messages : List WSMessage
    , playersById : Dict ConnId Player
    , roomId : Maybe RoomId
    , word : Maybe String
    , tmpRoomId : String
    , game : Maybe PasswordGame
    , tempClue : String
    , tempGuess : String
    }


playerName : Model -> Maybe String
playerName model =
    model.connId
        |> Maybe.andThen (\connId -> Dict.get connId model.playersById)
        |> Maybe.map .name


init : ( Model, Cmd Msg )
init =
    ( { connId = Nothing
      , name = ""
      , messages = []
      , playersById = Dict.empty
      , roomId = Nothing
      , word = Nothing
      , tmpRoomId = ""
      , game = Nothing
      , tempClue = ""
      , tempGuess = ""
      }
    , Cmd.none
    )



---- UPDATE ----


playerNameMessage : String -> String
playerNameMessage name =
    E.object
        [ ( "type", E.string "player-name-updated" )
        , ( "payload", E.string name )
        ]
        |> E.encode 0


type Msg
    = PlayerNameUpdated String
    | PlayerNameSubmitted
    | UpdateRoomId String
    | CreateRoom
    | JoinRoom
    | StartGame
    | Recv String
    | SubmitClue
    | ClueUpdated String
    | SubmitGuess
    | GuessUpdated String


handleWsMessage : Model -> WSMessage -> ( Model, Cmd Msg )
handleWsMessage model1 msg =
    let
        messages =
            List.append model1.messages [ msg ]

        model =
            { model1 | messages = messages }
    in
    case msg of
        IdentifyConnection connId ->
            ( { model | connId = Just connId }, Cmd.none )

        PlayerNameChanged connId name ->
            ( { model | playersById = Dict.insert connId (Player connId name) model.playersById }, Cmd.none )

        PlayerJoinedRoom connId name ->
            ( { model | playersById = Dict.insert connId (Player connId name) model.playersById }, Cmd.none )

        JoinedRoomResponse roomId playerNamesById ->
            ( { model | roomId = Just roomId, playersById = playerNamesById |> Dict.toList |> List.map (\( connId, name ) -> ( connId, Player connId name )) |> Dict.fromList }, Cmd.none )

        CreateRoomResponse roomId ->
            ( { model | roomId = Just roomId }, Cmd.none )

        NewWordResponse word ->
            ( { model | word = Just word }, Cmd.none )

        GameUpdated teamA teamB teamAGuesser teamBGuesser teamAClueGiver teamBClueGiver word clues guesses teamAScore teamBScore possession ->
            let
                game =
                    { teamA = teamA
                    , teamB = teamB
                    , teamAGuesser = teamAGuesser
                    , teamBGuesser = teamBGuesser
                    , teamAClueGiver = teamAClueGiver
                    , teamBClueGiver = teamBClueGiver
                    , word = word
                    , clues = clues
                    , guesses = guesses
                    , teamAScore = teamAScore
                    , teamBScore = teamBScore
                    , possession = possession
                    }
            in
            ( { model | game = Just game }, Cmd.none )

        ErrorResponse _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerNameUpdated name ->
            ( { model | name = name }, Cmd.none )

        PlayerNameSubmitted ->
            ( model, sendMessage <| playerNameMessage model.name )

        UpdateRoomId roomId ->
            ( { model | tmpRoomId = roomId }, Cmd.none )

        CreateRoom ->
            ( model, createRoom )

        JoinRoom ->
            ( model, joinRoom model.tmpRoomId )

        StartGame ->
            ( model, startGame )

        ClueUpdated clue ->
            ( { model | tempClue = clue }, Cmd.none )

        SubmitClue ->
            ( { model | tempClue = "" }, submitClue model.tempClue )

        GuessUpdated clue ->
            ( { model | tempGuess = clue }, Cmd.none )

        SubmitGuess ->
            ( { model | tempGuess = "" }, submitGuess model.tempGuess )

        Recv message ->
            case D.decodeString socketDecoder message of
                Ok decodedMessage ->
                    handleWsMessage model decodedMessage

                Err _ ->
                    ( model, Cmd.none )



---- VIEW ----


areWeAGuesser : Model -> Bool
areWeAGuesser model =
    case ( model.connId, model.game ) of
        ( Just connId, Just game ) ->
            game.teamAGuesser == connId || game.teamBGuesser == connId

        _ ->
            False


isMyTeamGuessing : Model -> Bool
isMyTeamGuessing model =
    case ( model.connId, model.game ) of
        ( Just connId, Just game ) ->
            case ( myTeam connId game, currentTeamGuessing game ) of
                ( Just team, Just guessingTeam ) ->
                    team == guessingTeam

                _ ->
                    False

        _ ->
            False


currentTeamGuessing : PasswordGame -> Maybe Possession
currentTeamGuessing game =
    if List.length game.clues == 1 + List.length game.guesses then
        if modBy 2 (List.length game.guesses) == 0 then
            Just game.possession

        else
            togglePossession game.possession |> Just

    else
        Nothing


amIGuessing : Model -> Bool
amIGuessing model =
    areWeAGuesser model && isMyTeamGuessing model


togglePossession : Possession -> Possession
togglePossession team =
    case team of
        TeamA ->
            TeamB

        TeamB ->
            TeamA


currentTeamGivingClue : PasswordGame -> Maybe Possession
currentTeamGivingClue game =
    if List.length game.clues == List.length game.guesses then
        if modBy 2 (List.length game.clues) == 0 then
            Just game.possession

        else
            togglePossession game.possession |> Just

    else
        Nothing


myTeam : ConnId -> PasswordGame -> Maybe Possession
myTeam connId game =
    case ( List.member connId game.teamA, List.member connId game.teamB ) of
        ( True, _ ) ->
            Just TeamA

        ( _, True ) ->
            Just TeamB

        _ ->
            Nothing


isMyTeamGivingAClue : ConnId -> PasswordGame -> Bool
isMyTeamGivingAClue connId game =
    case ( myTeam connId game, currentTeamGivingClue game ) of
        ( Just team, Just clueGivingTeam ) ->
            team == clueGivingTeam

        _ ->
            False


amITheClueGiverForMyTeam : ConnId -> PasswordGame -> Bool
amITheClueGiverForMyTeam connId game =
    case myTeam connId game of
        Just TeamA ->
            game.teamAClueGiver == connId

        Just TeamB ->
            game.teamBClueGiver == connId

        Nothing ->
            False


amIGivingAClue : Model -> Bool
amIGivingAClue model =
    case ( model.connId, model.game ) of
        ( Just connId, Just game ) ->
            isMyTeamGivingAClue connId game && amITheClueGiverForMyTeam connId game

        _ ->
            False


wsMessageToString : WSMessage -> String
wsMessageToString msg =
    case msg of
        IdentifyConnection connId ->
            "IdentifyConnection(" ++ connId ++ ")"

        ErrorResponse err ->
            "ErrorResponse(\"" ++ err ++ "\")"

        CreateRoomResponse roomId ->
            "CreateRoomResponse(" ++ roomId ++ ")"

        PlayerJoinedRoom connId name ->
            "PlayerJoinedRoom(" ++ connId ++ ", \"" ++ name ++ "\")"

        JoinedRoomResponse roomId _ ->
            "JoinedRoomResponse(roomId = " ++ roomId ++ ")"

        PlayerNameChanged connId name ->
            "PlayerNameChanged(" ++ connId ++ ", \"" ++ name ++ "\")"

        NewWordResponse word ->
            "NewWordResponse(" ++ word ++ ")"

        GameUpdated _ _ _ _ _ _ _ _ _ _ _ _ ->
            "GameUpdated(<see debug state>)"


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]

        -- , audio [ src "/password-thinking-song.m4a", autoplay True, controls True, loop True ] []
        , case playerName model of
            Nothing ->
                Html.form [ id "player-name", onSubmit PlayerNameSubmitted ]
                    [ input [ type_ "text", value model.name, onInput PlayerNameUpdated ] []
                    , input [ type_ "submit", value "Submit" ] []
                    ]

            Just name ->
                div []
                    [ div [] [ text ("Hello, " ++ name ++ "! 👋") ]
                    , case model.roomId of
                        Nothing ->
                            div []
                                [ button [ onClick CreateRoom ] [ text "Create Room" ]
                                , Html.form [ onSubmit JoinRoom ]
                                    [ input
                                        [ type_ "text"
                                        , placeholder "room id, ie \"wxyz\""
                                        , onInput UpdateRoomId
                                        ]
                                        []
                                    ]
                                ]

                        Just roomId ->
                            div []
                                [ text ("In room " ++ roomId)
                                , case model.game of
                                    Nothing ->
                                        button [ onClick StartGame ] [ text "Start Game" ]

                                    Just game ->
                                        div []
                                            [ h2 [] [ text "Team A" ]
                                            , ul [] (List.map (playerNameLi model) game.teamA)
                                            , h2 [] [ text "Team B" ]
                                            , ul [] (List.map (playerNameLi model) game.teamB)
                                            , h2 [] [ text "Clues" ]
                                            , ul [] (List.map (\s -> li [] [ text s ]) game.clues)
                                            , h2 [] [ text "Guesses" ]
                                            , ul [] (List.map (\s -> li [] [ text s ]) game.guesses)
                                            , if amIGuessing model then
                                                div []
                                                    [ text "You are guessing!"
                                                    , Html.form [ onSubmit SubmitGuess ] [ input [ type_ "text", value model.tempGuess, onInput GuessUpdated ] [] ]
                                                    ]

                                              else
                                                div []
                                                    [ h2 [] [ text "Current Word" ]
                                                    , div [] [ text game.word ]
                                                    ]
                                            , if amIGivingAClue model then
                                                div []
                                                    [ text "Your turn to give a clue!"
                                                    , Html.form [ onSubmit SubmitClue ] [ input [ type_ "text", value model.tempClue, onInput ClueUpdated ] [] ]
                                                    ]

                                              else
                                                div [] []
                                            ]
                                ]
                    ]
        , pre [] [ code [] (List.indexedMap (\i m -> String.fromInt i ++ " " ++ m ++ "\n" |> text) (List.map wsMessageToString model.messages)) ]
        ]


playerNameLi : Model -> ConnId -> Html Msg
playerNameLi model connId =
    li [] [ text <| .name <| Maybe.withDefault (Player "" "Unknown") <| Dict.get connId model.playersById ]



---- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveMessage Recv



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
