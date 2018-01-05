{-

TODO

necessary:
  * implement svg drawing of concentric circles
  * implement svg drawing of spiral

visual upgrade:
  * separate the buttons and the text box a little maybe
  * dynamically control the height of the text input
  * make the table look nicer and potentially have sorting

additional features:
  * create a settings page to control the drawing parameters
  * make it possible to eliminate certain rows from being displayed in the table

-}

import Html exposing (Html, Attribute, button, div, text, textarea, input, table, tr, td, th )
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Svg exposing (svg)
import Svg.Attributes exposing (width, height, viewBox)
import BeeTrip exposing (BeeTrip)
import ParseBee exposing (..)
import DrawBee
import Time.DateTime

main = Html.beginnerProgram { model = initModel, view = view, update = update }

type alias Model = {
  inputData : String,
  currentTab : Tab
  }

type Tab = Edit | Table | Image

initModel : Model
initModel = { inputData = "", currentTab = Edit }

type Msg =
      EditData String
    | ChangeTab Tab

update : Msg -> Model -> Model
update msg model =
  case msg of
    EditData str -> { model | inputData = str }
    ChangeTab currentTab -> { model | currentTab = currentTab }

view : Model -> Html Msg
view model =
    div [ style [
              ("margin-left", "5%"),
              ("margin-right", "5%"),
              ("margin-top","30px")] ] [
        documentation,
        div [ style [("text-align", "right")]] [ buttonBar model.currentTab ],
        div [ style [("text-align", "center")] ] [
            case model.currentTab of
              Edit -> editView model
              Table -> tableView model
              Image -> imageView model ]]

editView : Model -> Html Msg
editView model =
    div [] [
        textarea [ placeholder "CSV Data goes here",
                   onInput EditData,
                   rows 30,
                   style [("width","100%")] ] [
            text model.inputData ]]

imageView : Model -> Html Msg
imageView model =
  case parseBeeData model.inputData of
    Ok beeTrips ->
        svg [ Svg.Attributes.width "600",
              Svg.Attributes.height "600",
              Svg.Attributes.viewBox "0 0 600 600" ]
            (DrawBee.drawConcentricCircles beeTrips)
    Err str -> parseErrorDisplay str        

tableView : Model -> Html Msg
tableView model =
  let makeRow trip = tr [] [makeCell trip.start, makeCell trip.end]
      makeCell date = td [] [ text (Time.DateTime.toISO8601 date) ]
  in case parseBeeData model.inputData of
       Ok beeTrips ->
        table [ style [("border","1")] ]
            ((tr [] [td [] [text "Start Time"], td [] [text "End Time"]]) ::
             (List.map makeRow beeTrips))
       Err str -> parseErrorDisplay str

parseErrorDisplay : String -> Html Msg
parseErrorDisplay str = div [] [text ("Parser error: " ++ str)]

buttonBar : Tab -> Html Msg
buttonBar view =
    let makeButton (v, name) =
            button [disabled (view == v), onClick (ChangeTab v)] [text name]
    in div [] (List.map makeButton [
                  (Edit,"Edit"),
                  (Image,"Image"),
                  (Table,"Table")])

documentation : Html Msg
documentation = div [] [
    div [] [text "Hey, Nat! Here's a quick user interface for entering bee data. There will be better documentation and settings controls where you can change the colors, line thickness, and radii coming. I'm also working on the spiral version to see if that looks any better. For now, this should let you play with the view without having to send me the data to run the program. Just cut and paste a CSV file into the edit box here, click on image to see what it looks like, or table to see what the parsed data looks like in a table. The CSV needs to have a header. This is the one it's using below. The first line you cut and paste into the box should be that line and all subsequent lines should be data."],
    div [] [ text "\"\",\"Date_PST\",\"Time_end\",\"Time_start\",\"Date_PST_start\",\"overnight\",\"guarding\",\"pforage\",\"prob.sanitation\""]]
