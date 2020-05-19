port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as D
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
            D.map JoinedRoomResponse (D.field "roomId" D.string)

        "NewWordResponse" ->
            D.map NewWordResponse (D.field "word" D.string)

        "PlayerNameChanged" ->
            D.map2 PlayerNameChanged (D.field "connId" D.string) (D.field "name" D.string)

        _ ->
            D.fail <| "Unknown message type '" ++ type_ ++ "'"


type alias RoomId =
    String


type alias ConnId =
    String


type WSMessage
    = ErrorResponse String
    | IdentifyConnection ConnId
    | CreateRoomResponse RoomId
    | PlayerJoinedRoom ConnId String
    | JoinedRoomResponse RoomId
    | PlayerNameChanged ConnId String
    | NewWordResponse String



---- MODEL ----


type alias Player =
    { id : ConnId
    , name : String
    }


type alias Model =
    { connId : Maybe String
    , name : String
    , messages : List WSMessage
    , playersById : Dict ConnId Player
    , roomId : Maybe RoomId
    , word : Maybe String
    , tmpRoomId : String
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
    = NoOp
    | Send
    | PlayerNameUpdated String
    | PlayerNameSubmitted
    | UpdateRoomId String
    | CreateRoom
    | JoinRoom
    | Recv String


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

        JoinedRoomResponse roomId ->
            ( { model | roomId = Just roomId }, Cmd.none )

        CreateRoomResponse roomId ->
            ( { model | roomId = Just roomId }, Cmd.none )

        NewWordResponse word ->
            ( { model | word = Just word }, Cmd.none )

        ErrorResponse _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Send ->
            ( model, sendMessage "foo" )

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

        Recv message ->
            case D.decodeString socketDecoder message of
                Ok decodedMessage ->
                    handleWsMessage model decodedMessage

                Err _ ->
                    ( model, Cmd.none )



---- VIEW ----


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

        JoinedRoomResponse roomId ->
            "JoinedRoomResponse(" ++ roomId ++ ")"

        PlayerNameChanged connId name ->
            "PlayerNameChanged(" ++ connId ++ ", \"" ++ name ++ "\")"

        NewWordResponse word ->
            "NewWordResponse(" ++ word ++ ")"


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , case playerName model of
            Nothing ->
                Html.form [ onSubmit PlayerNameSubmitted ]
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
                            div [] [ text ("In room " ++ roomId) ]
                    ]
        , pre [] [ code [] (List.indexedMap (\i m -> String.fromInt i ++ " " ++ m ++ "\n" |> text) (List.map wsMessageToString model.messages)) ]
        ]



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
