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

import Html exposing ( Html, Attribute, main_, span, a, p, img ,br, text, strong, option, small, input, i)
import Html.Events exposing ( onInput, onClick )
import Html.Attributes exposing ( attribute, style, src, placeholder, type_, href, rel, class, value)

type alias Model = {
        url : String
        }

type Msg = UpdateURL String
         | DownloadVideo

main : Program () Model Msg
main
  = Browser.sandbox
    { init = {url=""}
    , view = view
    , update = update
    }

fontAwesomeCDN
  = Html.node "link"
    [ rel "stylesheet"
    , href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
    ]
    []

update : Msg -> Model -> Model
update msg model
  = case msg of
      UpdateURL u -> { model | url = u }
      DownloadVideo -> { model | url = "" }

view : Model -> Html Msg
view model
  = main_ []
    [ stylesheet
    , fontAwesomeCDN
    , videoHeader model
    , videoList
    , videoFooter
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

videoList : Html Msg
videoList
  = let
      headerCells = [ tableCellHead [ style "width" "60%" ] [ text "Video" ]
                    , tableCellHead [] [ text "Status" ]
                    , tableCellHead [ style "width" "10%" ] [ text "Actions" ]
                    ]
      tableEntry = tableRow False [] [ tableCell [] [ text "https://www.youtube.com/watch?v=SGBP3sG3a9Y" ]
                                     , tableCell [] [ easyProgress { progressModifiers | color = Success } [] 50.0 ]
                                     , tableCell [] [ icon Standard [] [ i [ class "fa fa-file-text-o" ] [] ]
                                                    , icon Standard [] [ i [ class "fa fa-file-text" ] [] ]
                                                    , icon Standard [] [ i [ class "fa fa-trash" ] [] ]
                                                    ]
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
                    , tableBody [] [ tableEntry
                                   , tableEntry
                                   , tableEntry ]
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
          , a [ href "https://github.com/" ] [ text "Leticia García Martín" ]
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
  
