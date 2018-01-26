port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, on)
import Svg
import Svg.Attributes
import BeeTrip exposing (BeeTrip)
import ParseBee exposing (parseBeeData)
import DrawBee exposing (drawConcentricCircles, DrawParams)
import Time.DateTime
import Json.Decode as JD
import List.Extra exposing (groupWhileTransitively)

port fileSelected : String -> Cmd msg
port fileContentRead : (CSVPortData -> msg) -> Sub msg

main = Html.program {
    init = (initModel, Cmd.none),
    view = view,
    update = update,
    subscriptions = subscriptions
    }

type alias CSVPortData = {
    contents : String,
    filename : String
    }

type alias Model = {
    csvFilename : Maybe String,
    error : Maybe String,
    beeTrips : List BeeTrip,
    currentTab : Tab
    }

type Tab = Edit | Image

initModel : Model
initModel = {
    csvFilename = Nothing,
    error = Nothing,
    beeTrips = [],
    currentTab = Edit
    }

type Msg =
      ChangeTab Tab
    | CSVSelected String
    | CSVParse CSVPortData

subscriptions : Model -> Sub Msg
subscriptions model = fileContentRead CSVParse

update : Msg -> Model -> (Model, Cmd msg)
update msg model = case msg of
    ChangeTab tab -> ({ model | currentTab = tab }, Cmd.none)
    CSVSelected elementId -> (model, fileSelected elementId)
    CSVParse data ->
        case ParseBee.parseBeeData data.contents of
            Ok beeTrips -> ({ model |
                beeTrips = beeTrips,
                csvFilename = Just data.filename,
                error = Nothing
                }, Cmd.none)
            Err str -> ({ model |
                beeTrips = [],
                csvFilename = Just data.filename,
                error = Just str
                }, Cmd.none)

view : Model -> Html Msg
view model =
    div [ style [
              ("margin-left", "5%"),
              ("margin-right", "5%"),
              ("margin-top","30px")] ] [
        fileUpload model,
        div [] (List.map beeView
                   (groupWhileTransitively
                     (\t1 t2->t1.uid == t2.uid)
                     (List.sortWith (\t1 t2->compare t1.uid t2.uid)
                                    model.beeTrips))) ]

fileUpload : Model -> Html Msg
fileUpload model =
    div [] [
        input [ type_ "file",
                id "CSVInput",
                on "change" (JD.succeed (CSVSelected "CSVInput"))] [],
        -- FIX ME make this div hidden if no error
        div [] [ case model.error of
                   Nothing -> text ""
                   Just str -> text str ] ]

beeView : List BeeTrip -> Html Msg
beeView trips = div [style [("display","flex")]] [
    div [] [tableView trips],
    imageView (600, 600) trips ]

tableView : List BeeTrip -> Html Msg
tableView trips =
  let makeRow trip = tr [] [
          cell trip.uid, dateCell trip.start, dateCell trip.end]
      cell str = td [] [ text str ]
      dateCell date = cell (Time.DateTime.toISO8601 date)
      header = tr [] [
          td [] [text "UID"],
          td [] [text "Start Time"],
          td [] [text "End Time"]]
      rows = List.map makeRow trips
  in table [ style [("border","1")] ] (header :: rows)

imageView : (Float, Float) -> List BeeTrip -> Html Msg
imageView (width, height) trips =
    let w = toString width
        h = toString height
    in Svg.svg [ Svg.Attributes.width w,
                 Svg.Attributes.height h,
                 Svg.Attributes.viewBox ("0 0 " ++ w ++ " " ++ h) ]
               (drawConcentricCircles (DrawParams width height) trips)

parseErrorDisplay : String -> Html Msg
parseErrorDisplay str = div [] [text ("Parser error: " ++ str)]

buttonBar : Tab -> Html Msg
buttonBar tab =
    let makeButton (t, name) =
            button [
                style [
                    ("padding", "10px"),
                    ("color", if tab == t then "grey" else "white"),
                    ("background-color", "blue"),
                    ("border", "none"),
                    ("margin", "2px")],
                disabled (tab == t),
                onClick (ChangeTab t)]
                [text name]
    in div [] (List.map makeButton [
                  (Edit,"Edit"),
                  (Image,"Image")])
