port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, on)
import Svg
import Svg.Attributes
import BeeTrip exposing (BeeTrip)
import ParseBee exposing (parseBeeData)
import DrawBee
import Time.DateTime
import Json.Decode as JD

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

type Tab = Edit | Table | Image

initModel : Model
initModel = {
    csvFilename = Nothing,
    error = Nothing,
    beeTrips = [],
    currentTab = Edit
    }

type Msg =
      ChangeTab Tab
    | CSVSelected
    | CSVParse CSVPortData

subscriptions : Model -> Sub Msg
subscriptions model = fileContentRead CSVParse

update : Msg -> Model -> (Model, Cmd msg)
update msg model = case msg of
    ChangeTab tab -> ({ model | currentTab = tab }, Cmd.none)
    CSVSelected -> (model, fileSelected "CSVInput")
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
        documentation,
        div [ style [("text-align", "right")]]
            [ buttonBar model.currentTab ],
        div [ style [("text-align", "center")] ] [
            case model.currentTab of
              Edit -> editView model
              Table -> tableView model
              Image -> imageView model ]]

editView : Model -> Html Msg
editView model =
    div [] [
        input [ type_ "file",
                id "CSVInput",
                on "change" (JD.succeed CSVSelected)] [],
        div [] [ case model.error of
                   Nothing -> text "no error"
                   Just str -> text str ]
        ]

imageView : Model -> Html Msg
imageView model =
    Svg.svg [ Svg.Attributes.width "600",
              Svg.Attributes.height "600",
              Svg.Attributes.viewBox "0 0 600 600" ]
        (DrawBee.drawConcentricCircles model.beeTrips)

tableView : Model -> Html Msg
tableView model =
  let makeRow trip = tr [] [makeCell trip.start, makeCell trip.end]
      makeCell date = td [] [ text (Time.DateTime.toISO8601 date) ]
  in table [ style [("border","1")] ]
         ((tr [] [td [] [text "Start Time"], td [] [text "End Time"]]) ::
          (List.map makeRow model.beeTrips))

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
                  (Image,"Image"),
                  (Table,"Table")])

documentation : Html Msg
documentation = div [] [
    p [] [text "Hey, Nat! Here's a quick user interface for entering bee data. There will be better documentation and settings controls where you can change the colors, line thickness, and radii coming. I'm also working on the spiral version to see if that looks any better. For now, this should let you play with the view without having to send me the data to run the program. Just cut and paste a CSV file into the edit box here, click on image to see what it looks like, or table to see what the parsed data looks like in a table. The CSV needs to have a header. This is the one it's using below. The first line you cut and paste into the box should be that line and all subsequent lines should be data."],
    p [] [ text "\"\",\"Date_PST\",\"Time_end\",\"Time_start\",\"Date_PST_start\",\"overnight\",\"guarding\",\"pforage\",\"prob.sanitation\""]]
