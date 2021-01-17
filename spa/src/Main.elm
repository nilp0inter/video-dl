module Main exposing (..)

-- import Debug

import Browser
import Bulma.CDN exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Html.Events.Extra exposing (onEnter)
import Html exposing (Attribute, Html, a, code, div, i, img, p, pre, strong, text)
import Html.Attributes exposing (class, href, placeholder, rel, src, style, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave, onMouseOver)
import Http
import Json.Decode as Decode
import Maybe.Extra exposing (orElse)
import Process
import Task
import Url
import Loading exposing (LoaderType(..), defaultConfig, render)


type alias Model =
    { url : String
    , videos : List Video
    , showingModal : Bool
    , videoInModal : Maybe Video
    , textInModal : DownloadLog
    , showingVideoInfo : Maybe String
    , config : Maybe Config
    }


type alias Config =
    { title : String
    , message : String
    , actionInfo : Bool
    , actionStdout : Bool
    , actionStderr : Bool
    , actionDelete : Bool
    , bannerBackgroundColor : String
    , bannerTextColor : String
    }


type alias VideoInfo =
    { title : Maybe String
    , description : Maybe String
    , extractor_key : String
    , thumbnail : Maybe String
    }


type VideoStatus
    = Done
    | Submitted
    | Error
    | Pending


type DownloadLog
    = PendingLog
    | MissingLog
    | Log String


type alias Video =
    { id : String
    , url : String
    , title : Maybe String
    , description : Maybe String
    , extractor_key : Maybe String
    , thumbnail : Maybe String
    , status : VideoStatus
    , progress : Maybe Float
    }


defaultVideo =
    { id = ""
    , url = ""
    , title = Nothing
    , description = Nothing
    , extractor_key = Nothing
    , thumbnail = Nothing
    , status = Submitted
    , progress = Nothing
    }


type Msg
    = UpdateURL String
    | DownloadVideo
    | DeleteVideo Video
    | ViewInfo Video
    | ViewStdOut Video
    | ViewStdErr Video
    | CloseModal
    | GotDownloadResponse String (Result Http.Error String)
    | GotDeleteResponse String (Result Http.Error ())
    | GotStatusResponse String (Result Http.Error VideoStatus)
    | GotProgressResponse String (Result Http.Error (Maybe Float))
    | GotVideosResponse (Result Http.Error (List Video))
    | GotInfoResponse String (Result Http.Error VideoInfo)
    | GotStdOutResponse String (Result Http.Error String)
    | GotStdErrResponse String (Result Http.Error String)
    | GotConfigResponse (Result Http.Error Config)
    | ShowVideoInfo (Maybe String)


statusToColor : VideoStatus -> Color
statusToColor videostatus =
    case videostatus of
        Done ->
            Success

        Submitted ->
            Warning

        Error ->
            Danger

        Pending ->
            Info

initialConfig =
    { title = "video-dl"
    , message = "Video URL"
    , actionInfo = True
    , actionStdout = True
    , actionStderr = True
    , actionDelete = True
    , bannerBackgroundColor = "#209cee"
    , bannerTextColor = "#ffffff"
    }

initialModel =
    { url = ""
    , videos = []
    , showingModal = False
    , videoInModal = Nothing
    , textInModal = PendingLog
    , showingVideoInfo = Nothing
    , config = Nothing
    }


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


fontAwesomeCDN =
    Html.node "link"
        [ rel "stylesheet"
        , href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
        ]
        []


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.batch [ getVideoList, getConfig ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateURL u ->
            ( { model | url = u }, Cmd.none )

        DownloadVideo ->
            ( { model | url = "" }
            , if model.url == "" then Cmd.none else requestDownloadVideo model.url
            )

        DeleteVideo video ->
            ( model, requestDeleteVideo video.id )

        ViewInfo video ->
            ( { model | showingModal = True, videoInModal = Just video }
            , Cmd.none
            )

        ViewStdOut video ->
            ( { model | showingModal = True, videoInModal = Just video }
            , requestStdOut video.id
            )

        ViewStdErr video ->
            ( { model | showingModal = True, videoInModal = Just video }
            , requestStdErr video.id
            )

        CloseModal ->
            ( { model | showingModal = False, videoInModal = Nothing, textInModal = PendingLog }, Cmd.none )

        GotDownloadResponse url res ->
            case res of
                Ok id ->
                    ( { model | videos = { defaultVideo | id = id, url = url } :: model.videos }
                    , Task.attempt (GotStatusResponse id) (requestStatus id)
                    )

                Err _ ->
                    ( { model | url = "ERROR!" }, Cmd.none )

        GotDeleteResponse id res ->
            case res of
                Ok _ ->
                    ( { model | videos = List.filter (\video -> video.id /= id) model.videos }, Cmd.none )

                Err _ ->
                    ( { model | url = "ERROR!" }, Cmd.none )

        GotStatusResponse id res ->
            case res of
                Ok status ->
                    ( { model | videos = List.map (updateVideoStatus id status) model.videos }
                    , Cmd.batch
                        [ if status == Submitted || status == Pending then
                            Task.attempt (GotStatusResponse id) (requestStatus id)

                          else
                            Cmd.none
                        , if status == Pending then
                            requestProgress id

                          else
                            Cmd.none
                        , if (getVideo id model.videos |> Maybe.andThen .title) == Nothing then
                            requestInfo id

                          else
                            Cmd.none
                        ]
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotProgressResponse id res ->
            case res of
                Ok progress ->
                    ( { model | videos = List.map (updateVideoProgress id progress) model.videos }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotVideosResponse res ->
            case res of
                Ok videos ->
                    ( { model | videos = videos }
                    , Cmd.batch (List.map (\v -> getVideoStatus v.id) videos)
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotInfoResponse id res ->
            case res of
                Ok info ->
                    ( { model | videos = List.map (updateVideoInfo id info) model.videos }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotStdOutResponse id res ->
            case res of
                Ok stdout ->
                    ( { model | textInModal = Log stdout }, Cmd.none )

                Err _ ->
                    ( { model | textInModal = MissingLog }, Cmd.none )

        GotStdErrResponse id res ->
            case res of
                Ok stderr ->
                    ( { model | textInModal = Log stderr }, Cmd.none )

                Err _ ->
                    ( { model | textInModal = MissingLog }, Cmd.none )

        ShowVideoInfo id ->
            ( { model | showingVideoInfo = id }, Cmd.none )

        GotConfigResponse res ->
            case res of
                Ok config ->
                    ( { model | config = Just config }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )


getVideo : String -> List Video -> Maybe Video
getVideo id videos =
    List.filter (\video -> video.id == id) videos |> List.head


updateVideoInfo id info video =
    if video.id == id then
        { video
            | title = info.title
            , description = info.description
            , extractor_key = Just info.extractor_key
            , thumbnail = info.thumbnail
        }

    else
        video


updateVideoStatus id status video =
    if video.id == id then
        { video | status = status }

    else
        video


updateVideoProgress id progress video =
    if video.id == id then
        { video | progress = progress }

    else
        video


handleJsonResponse : Decode.Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


requestStdOut id =
    Http.get
        { url = "/api/download/" ++ id ++ "/log/stdout"
        , expect = Http.expectString (GotStdOutResponse id)
        }


requestStdErr id =
    Http.get
        { url = "/api/download/" ++ id ++ "/log/stderr"
        , expect = Http.expectString (GotStdErrResponse id)
        }


requestStatus id =
    Process.sleep 2000
        |> Task.andThen
            (\_ ->
                Http.task
                    { method = "GET"
                    , headers = []
                    , url = "/api/download/" ++ id ++ "/status"
                    , body = Http.emptyBody
                    , resolver = Http.stringResolver <| handleJsonResponse <| decodeStatus
                    , timeout = Nothing
                    }
            )


decodeInfo : Decode.Decoder VideoInfo
decodeInfo =
    Decode.map4 VideoInfo
        (Decode.maybe <| Decode.field "title" Decode.string)
        (Decode.maybe <| Decode.field "description" Decode.string)
        (Decode.field "extractor_key" Decode.string)
        (Decode.maybe <| Decode.field "thumbnail" Decode.string)


requestInfo id =
    Http.get
        { url = "/api/download/" ++ id ++ "/info"
        , expect = Http.expectJson (GotInfoResponse id) decodeInfo
        }


getVideoStatus id =
    Http.get
        { url = "/api/download/" ++ id ++ "/status"
        , expect = Http.expectJson (GotStatusResponse id) decodeStatus
        }


getVideoList =
    Http.get
        { url = "/api/download"
        , expect = Http.expectJson GotVideosResponse decodeVideoList
        }


getConfig =
    Http.get
        { url = "/api/config"
        , expect = Http.expectJson GotConfigResponse decodeConfig
        }


fromDefaultVideo id url =
    { defaultVideo | id = id, url = url }


decodeVideoList : Decode.Decoder (List Video)
decodeVideoList =
    Decode.list
        (Decode.map2 fromDefaultVideo
            (Decode.field "id" Decode.string)
            (Decode.field "url" Decode.string)
        )

decodeConfig : Decode.Decoder Config
decodeConfig =
    Decode.map8 Config
        (Decode.field "title" Decode.string)
        (Decode.field "message" Decode.string)
        (Decode.field "action_info" Decode.bool)
        (Decode.field "action_stdout" Decode.bool)
        (Decode.field "action_stderr" Decode.bool)
        (Decode.field "action_delete" Decode.bool)
        (Decode.field "banner_background_color" Decode.string)
        (Decode.field "banner_text_color" Decode.string)

requestProgress id =
    Http.get
        { url = "/api/download/" ++ id ++ "/progress"
        , expect = Http.expectJson (GotProgressResponse id) (Decode.maybe Decode.float)
        }


stringToStatus : String -> VideoStatus
stringToStatus s =
    case s of
        "done" ->
            Done

        "pending" ->
            Pending

        "error" ->
            Error

        _ ->
            Submitted


decodeStatus : Decode.Decoder VideoStatus
decodeStatus =
    Decode.map stringToStatus Decode.string


requestDeleteVideo : String -> Cmd Msg
requestDeleteVideo id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/api/download/" ++ id
        , body = Http.emptyBody
        , expect = Http.expectWhatever (GotDeleteResponse id)
        , timeout = Nothing
        , tracker = Nothing
        }


requestDownloadVideo : String -> Cmd Msg
requestDownloadVideo url =
    Http.post
        { url = "/api/download"
        , body = Http.stringBody "text/plain" url
        , expect = Http.expectString (GotDownloadResponse url)
        }


view : Model -> Browser.Document Msg
view model =
    case model.config of
        Nothing ->
            { title = "Loading..."
            , body = [
                div [ ]
                    [ Loading.render
                        BouncingBalls
                        { defaultConfig | color = "#333" } -- Config
                        Loading.On -- LoadingState
                    ]
                ]
            }
        Just config ->
            { title = config.title 
            , body =
                [ stylesheet
                , fontAwesomeCDN
                , videoModal model
                , videoHeader config model
                , videoList config model
                , videoFooter
                ]
            }


videoModal : Model -> Html Msg
videoModal model =
    modal model.showingModal
        []
        [ modalBackground [] []
        , modalCard []
            [ modalCardHead []
                [ modalCardTitle []
                    [ text
                        (model.videoInModal
                            |> Maybe.andThen .title
                            |> orElse (model.videoInModal |> Maybe.map .url)
                            |> Maybe.withDefault "N/A"
                        )
                    ]
                ]
            , modalCardBody []
                [ viewLog model.textInModal
                ]
            , modalCardFoot [] []
            ]
        , modalClose Large [ onClick CloseModal ] []
        ]


viewLog : DownloadLog -> Html Msg
viewLog log =
    case log of
        PendingLog ->
            icon Standard [] [ i [ class "fa fa-spinner fa-pulse" ] [] ]

        MissingLog ->
            text "😱 Log not found"

        Log s ->
            pre [ style "display" "flex" ] [ text s ]


columnSizes =
    { mobile = Just Auto
    , tablet = Just Width9
    , desktop = Just Width10
    , widescreen = Just Width10
    , fullHD = Just Width10
    }


videoHeader : Config -> Model -> Html Msg
videoHeader config model =
    hero { heroModifiers | color = Info, size = Small }
        [ style "background-color" config.bannerBackgroundColor
        ]
        [ heroBody []
            [ container []
                [ title H1 [ style "color" config.bannerTextColor ] [ text config.message ]
                , columns columnsModifiers
                    []
                    [ column { columnModifiers | widths = columnSizes }
                        []
                        [ controlInput { controlInputModifiers | size = Large }
                            []
                            [ onInput UpdateURL
                            , placeholder "URL"
                            , value model.url
                            , onEnter DownloadVideo
                            ]
                            []
                        ]
                    , column columnModifiers
                        []
                        [ button
                            { buttonModifiers
                                | color = Success
                                , size = Large
                                , iconLeft = Just ( Standard, [], i [ class "fa fa-check" ] [] )
                            }
                            [ onClick DownloadVideo ]
                            [ text "Download" ]
                        ]
                    ]
                ]
            ]
        ]


myImage : String -> Html msg
myImage s =
    image SixteenByNine
        []
        [ img
            [ src s
            , style "height" "auto"
            ]
            []
        ]


myCardImage : String -> Html msg
myCardImage s =
    cardImage [] [ myImage s ]


myCardHeader : String -> String -> Html msg
myCardHeader s url =
    let
        favicon =
            Url.fromString url
                |> Maybe.map (\u -> { u | path = "/favicon.ico", query = Nothing, fragment = Nothing })
                |> Maybe.map Url.toString
                |> Maybe.withDefault ""
    in
    cardHeader []
        [ cardIcon [ style "padding-left" "1.5rem" ]
            [ icon Standard
                []
                [ img
                    [ src favicon
                    , style "text-align" "center"
                    , style "width" "auto"
                    , style "height" "auto"
                    ]
                    []
                ]
            ]
        , cardTitle [] [ text s ]
        ]


myCard video =
    card []
        [ Maybe.withDefault (div [] []) (Maybe.map myCardImage video.thumbnail)
        , myCardHeader (Maybe.withDefault video.url video.title) video.url
        , cardContent [] [ text <| Maybe.withDefault "😿 Description unavailable" video.description ]
        ]



--


myDropdownTrigger video =
    hoverableDropdown dropdownModifiers [] [ text (Maybe.withDefault video.url video.title) ]


myDropdownMenu video =
    dropdownMenu [ style "width" "512px" ]
        []
        [ dropdownItem False [] [ myCard video ]
        ]


hover model video =
    dropdown (model.showingVideoInfo == Just video.id)
        dropdownModifiers
        []
        [ myDropdownTrigger video
        , myDropdownMenu video
        ]


videoToRow : Config -> Model -> Video -> Html Msg
videoToRow config model video =
    tableRow False
        []
        [ tableCell [] [ hover model video ]
        , tableCell [] [ easyProgress { progressModifiers | color = statusToColor video.status } [] (videoToProgress video) ]
        , tableCell [] ([] ++
        (if config.actionInfo
         then [ a [ onMouseEnter (ShowVideoInfo (Just video.id))
                  , onMouseLeave (ShowVideoInfo Nothing)
                  ]
                  [ icon Standard [] [ i [ class "fa fa-info" ] [] ] ]
              ] 
         else []) ++
        (if config.actionStdout
         then [ a [ onClick (ViewStdOut video) ] [ icon Standard [] [ i [ class "fa fa-file-text-o" ] [] ] ] ]
         else []) ++
        (if config.actionStderr
         then [ a [ onClick (ViewStdErr video) ] [ icon Standard [] [ i [ class "fa fa-file-text" ] [] ] ] ]
         else []) ++
        (if config.actionDelete
         then [ a [ onClick (DeleteVideo video) ] [ icon Standard [] [ i [ class "fa fa-trash" ] [] ] ] ]
         else [])
        )
        ]


videoToProgress : Video -> Float
videoToProgress video =
    if video.status == Pending then
        Maybe.withDefault 0 video.progress

    else
        1


videoList : Config -> Model -> Html Msg
videoList config model =
    let
        headerCells =
            [ tableCellHead [ style "width" "61.80%" ] [ text "Video" ]
            , tableCellHead [] [ text "Status" ]
            , tableCellHead [ style "width" "128px" ] [ text "Actions" ]
            ]
    in
    section NotSpaced
        []
        [ container []
            [ table
                { tableModifiers
                    | bordered = False
                    , fullWidth = True
                    , striped = True
                    , hoverable = True
                }
                []
                [ tableHead []
                    [ tableRow False [] headerCells
                    ]
                , tableBody [] (List.map (videoToRow config model) model.videos)
                , tableFoot []
                    [ tableRow False [] headerCells
                    ]
                ]
            ]
        ]


videoFooter : Html Msg
videoFooter =
    footer []
        [ container []
            [ content Standard
                [ textCentered ]
                [ p []
                    [ strong [] [ text "video-dl" ]
                    , text " a tacky web ui for "
                    , a [ href "https://youtube-dl.org" ] [ text "youtube-dl" ]
                    , text " by "
                    , a [ href "https://github.com/laetitiae" ] [ text "Leticia García Martín" ]
                    , text " and "
                    , a [ href "https://github.com/nilp0inter" ] [ text "Roberto Abdelkader Martínez Pérez" ]
                    , text ". The source code is licensed "
                    , a [ href "http://opensource.org/licenses/mit-license.php" ] [ text "MIT" ]
                    , text ". The website content is licensed "
                    , a [ href "http://creativecommons.org/licenses/by-nc-sa/4.0" ] [ text "CC BY NC SA 4.0" ]
                    , text "."
                    ]
                ]
            ]
        ]
