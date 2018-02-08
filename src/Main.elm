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
    debugIndex : Int
    }

type Tab = Edit | Image

initModel : Model
initModel = {
    csvFilename = Nothing,
    error = Nothing,
    beeTrips = [],
    debugIndex = 0
    }

defaultParams : DrawParams
defaultParams = DrawParams 600 600 5 20

type Msg =
      CSVSelected String
    | CSVParse CSVPortData
    | Decrement
    | Increment

subscriptions : Model -> Sub Msg
subscriptions model = fileContentRead CSVParse

update : Msg -> Model -> (Model, Cmd msg)
update msg model = case msg of
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
    Decrement -> ({ model | debugIndex = model.debugIndex - 1 }, Cmd.none)
    Increment -> ({ model | debugIndex = model.debugIndex + 1 }, Cmd.none)

view : Model -> Html Msg
view model =
    div [ style [
              ("margin-left", "5%"),
              ("margin-right", "5%"),
              ("margin-top","30px")] ] [
        fileUpload model,
        if False
        then debugView model
        else div [] (List.map beeView
                   (groupWhileTransitively
                     (\t1 t2->t1.uid == t2.uid)
                     (List.sortWith (\t1 t2->compare t1.uid t2.uid)
                                    model.beeTrips)))
        ]

debugView : Model -> Html Msg
debugView model =
    div [] [
        div [] [
            button [ onClick Decrement ] [ text "-" ],
            div [] [text (toString model.debugIndex)],
            button [ onClick Increment ] [ text "+" ]
            ],
        case List.head model.beeTrips of
          Just firstTrip ->
            case List.head (List.drop model.debugIndex model.beeTrips) of
              Just trip ->
                div [] [
                    div [] [ text (toString trip) ],
                    div [] [ text (toString (DrawBee.tripToArcData (Time.DateTime.date firstTrip.start) trip)) ],
                    imageView [trip]
                    ]
              Nothing -> div [] [ text "No data" ]
          Nothing -> div [] [ text "Empty list" ]
        ]
        

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
beeView trips =
  div [] [
    div [style [("background-color","blue"),
                ("color", "white"),
                ("margin-top", "20px")]] [
        text (Maybe.withDefault "error"
                 (Maybe.map .uid (List.head trips))) ],
    div [style [("display","flex")]] [
      div [] [tableView trips],
      imageView trips ] ]

tableView : List BeeTrip -> Html Msg
tableView trips =
  let makeRow trip = tr [] [dateCell trip.start, dateCell trip.end]
      cell str = td [] [ text str ]
      dateCell date = cell (Time.DateTime.toISO8601 date)
      header = tr [] [
          td [] [text "Start Time"],
          td [] [text "End Time"]]
      rows = List.map makeRow trips
  in table [ style [("border","1")] ] (header :: rows)

imageView : List BeeTrip -> Html Msg
imageView trips =
  let rx = defaultParams.width / 2
      ry = defaultParams.height / 2
      viewBoxStr = String.join " "
          (List.map toString [-rx,-ry,defaultParams.width,defaultParams.height])
  in Svg.svg [ Svg.Attributes.width (toString defaultParams.width),
               Svg.Attributes.height (toString defaultParams.height),
               Svg.Attributes.viewBox viewBoxStr ]
             (drawConcentricCircles defaultParams trips)

parseErrorDisplay : String -> Html Msg
parseErrorDisplay str = div [] [text ("Parser error: " ++ str)]
