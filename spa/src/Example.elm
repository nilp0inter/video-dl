module Example exposing (..)

import Browser
import Bulma.CDN exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Bulma.Form exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Components exposing (..)
import Bulma.Columns as Columns exposing (..)
import Bulma.Layout exposing (..)
import Maybe.Extra exposing (orElse)

import Html exposing ( Html, Attribute, main_, span, a, p, img ,br, text, strong, option, small, input, i, code)
import Html.Events exposing ( onInput, onClick )
import Html.Attributes exposing ( attribute, style, src, placeholder, type_, href, rel, class, value)

import Http

type alias Model = 
  { url : String
  , videos : List Video 
  , showingModal : Bool
  , videoInModal : Maybe Video
  , textInModal : Maybe String
  }

type VideoStatus = Done
                 | Submitted
                 | Error
                 | Pending

type alias Video = 
  { id : String
  , url : String
  , title : Maybe String
  , status : VideoStatus
  , progress: Maybe Float
  }

type Msg = UpdateURL String
         | DownloadVideo
         | DeleteVideo Video
         | ViewInfo Video
         | ViewStdOut Video
         | ViewStdErr Video
         | CloseModal
         | GotDownloadResponse String (Result Http.Error String)
         | GotDeleteResponse String (Result Http.Error ())

myVideos = 
  [ { id = "id1"
    , url = "www.miurl.es"
    , title = Just "Video1"
    , status = Pending
    , progress = Just 0.2
    }
  , { id = "id2"
    , url = "www.miurl2.es"
    , title = Just "Video2"
    , status = Error
    , progress = Nothing
    }
  , { id = "id3"
    , url = "www.miurl3.es"
    , title = Just "Video3"
    , status = Done
    , progress = Just 1
    }
  , { id = "id4"
    , url = "www.miurl4.es"
    , title = Nothing
    , status = Submitted
    , progress = Nothing
    }
  ]

statusToColor: VideoStatus -> Color
statusToColor videostatus
  = case videostatus of
          Done -> Success
          Submitted -> Warning
          Error -> Danger
          Pending -> Info

initialModel
  = { url = ""
    , videos = myVideos
    , showingModal = False
    , videoInModal = Nothing
    , textInModal = Just "Logging" }

main
  = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

fontAwesomeCDN
  = Html.node "link"
    [ rel "stylesheet"
    , href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
    ]
    []

init : () -> (Model, Cmd Msg)
init _ = (initialModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model
  = case msg of
      UpdateURL u -> ({ model | url = u }, Cmd.none)
      DownloadVideo -> ({ model | url = "" }, requestDownloadVideo model.url)
      DeleteVideo video -> ( model, requestDeleteVideo video.id )
      ViewInfo video -> ({ model | showingModal = True, videoInModal = Just video }, Cmd.none)
      ViewStdOut video -> ({ model | showingModal = True, videoInModal = Just video }, Cmd.none)
      ViewStdErr video -> ({ model | showingModal = True, videoInModal = Just video }, Cmd.none)
      CloseModal -> ({ model | showingModal = False, videoInModal = Nothing }, Cmd.none)
      GotDownloadResponse url res -> case res of
              Ok id -> ({ model | videos = { id = id, url = url, title = Nothing, status = Submitted, progress = Nothing } :: model.videos }, Cmd.none)
              Err _ -> ({ model | url = "ERROR!" }, Cmd.none)
      GotDeleteResponse id res -> case res of
              Ok _  -> ( { model | videos = List.filter ( \video -> video.id /= id ) model.videos }, Cmd.none )
              Err _ -> ({ model | url = "ERROR!" }, Cmd.none)
                                  
requestDeleteVideo: String -> Cmd Msg
requestDeleteVideo id
  = Http.request
      { method = "DELETE"
      , headers = []
      , url = "http://localhost:8080/download/" ++ id
      , body = Http.emptyBody
      , expect = Http.expectWhatever (GotDeleteResponse id)
      , timeout = Nothing
      , tracker = Nothing
      }

requestDownloadVideo : String -> Cmd Msg
requestDownloadVideo url
  = Http.post
      { url = "http://localhost:8080/download"
      , body = Http.stringBody "text/plain" url
      , expect = Http.expectString (GotDownloadResponse url)
      }

view : Model -> Html Msg
view model
  = main_ []
    [ stylesheet
    , fontAwesomeCDN
    , videoModal model
    , videoHeader model
    , videoList model
    , videoFooter
    ]


videoModal : Model -> Html Msg
videoModal model
  = modal model.showingModal []
    [ modalBackground [] []
    , modalCard [] [ modalCardHead [] [ modalCardTitle [] [ text (model.videoInModal
                                                                  |> Maybe.andThen .title
                                                                  |> orElse (model.videoInModal |> Maybe.map .url)
                                                                  |> Maybe.withDefault "N/A") ] ]
                   , modalCardBody [] [ model.textInModal
                                        |> Maybe.map (\t -> code [ style "display" "flex" ] [ text t ])
                                        |> Maybe.withDefault (icon Standard [] [ i [ class "fa fa-spinner fa-pulse" ] [] ])
                                      ]
                   , modalCardFoot [] [ ]
                   ]
    , modalClose Large [ onClick CloseModal ] []
    ]

columnSizes = { mobile = Just Auto
              , tablet = Just Width9
              , desktop = Just Width10
              , widescreen = Just Width10
              , fullHD = Just Width10
              }

videoHeader : Model -> Html Msg
videoHeader model
  = hero { heroModifiers | color = Info, size = Small } []
    [ heroBody []
      [ container []
        [ title H1 [] [ text "Video URL" ]
        , columns columnsModifiers []
          [ column { columnModifiers | widths = columnSizes } [] [
                  controlInput { controlInputModifiers | size = Large } [] [ onInput UpdateURL
                                                                           , placeholder "URL"
                                                                           , value model.url ] [ ] ]
          , column columnModifiers [] [
                  button { buttonModifiers | color = Success
                                           , size = Large
                                           , iconLeft = Just (Standard, [], i [ class "fa fa-check" ] [] )
                         } [ onClick DownloadVideo ] [ text "Download" ] ]
          ]
        ]
      ]
    ]

videoToRow : Video -> Html Msg
videoToRow video
  = tableRow False [] [ tableCell [] [ text (Maybe.withDefault video.url video.title) ]
                                     , tableCell [] [ easyProgress { progressModifiers | color = statusToColor video.status } [] (Maybe.withDefault 1 video.progress) ]
                                     , tableCell [] [ a [ onClick (ViewInfo video) ] [ icon Standard [] [ i [ class "fa fa-info" ] [] ] ]
                                                    , a [ onClick (ViewStdOut video) ] [ icon Standard [] [ i [ class "fa fa-file-text-o" ] [] ] ] 
                                                    , a [ onClick (ViewStdErr video) ] [ icon Standard [] [ i [ class "fa fa-file-text" ] [] ] ]
                                                    , a [ onClick (DeleteVideo video) ] [ icon Standard [] [ i [ class "fa fa-trash" ] [] ] ]
                                                    ]
                                     ]

videoList : Model -> Html Msg
videoList model
  = let
      headerCells = [ tableCellHead [ style "width" "61.80%" ] [ text "Video" ]
                    , tableCellHead [] [ text "Status" ]
                    , tableCellHead [ style "width" "128px" ] [ text "Actions" ]
                    ]

    in
        section NotSpaced [] [
          container [] [
                    table { tableModifiers | bordered = False
                                           , fullWidth = True
                                           , striped = True
                                           , hoverable = True } []
                    [ tableHead [] [ tableRow False [] headerCells
                                   ]
                    , tableBody [] (List.map videoToRow model.videos)
                    , tableFoot [] [ tableRow False [] headerCells
                                   ]
                    ]
            ]
    ]


videoFooter : Html Msg
videoFooter
  = footer []
    [ container []
      [ content Standard [ textCentered ]
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
  
