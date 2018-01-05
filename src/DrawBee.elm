module DrawBee exposing (..)

import List exposing (concatMap)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time.DateTime exposing (DateTime, date, hour, minute, second,
                                setHour, setMinute, setSecond, setMillisecond)
import Time.Date exposing (Date, delta)
import Html exposing (Html)
import BeeTrip exposing (..)

type alias ArcData = {
    numDay : Int,
    startAngle : Float,
    endAngle : Float
    }

centerX = 300
centerY = 300
strokeWidth_ = 5
startRadius = 20

drawConcentricCircles : List BeeTrip -> List (Svg a)
drawConcentricCircles trips =
    List.map describeArc (tripsToArcData (splitTripsOfDayBreak trips))

describeArc : ArcData -> Svg a
describeArc arc =
    let radius = numberedDayToRadius arc.numDay
        (startX, startY) = polarToCartesian (radius, arc.startAngle)
        (endX, endY) = polarToCartesian (radius, arc.endAngle)
        largeArcFlag =
            (arc.startAngle > arc.endAngle)
            == (arc.endAngle - arc.startAngle > pi)
    in Svg.path [
        fill "none",
        stroke "black",
        strokeWidth (toString strokeWidth_),
        d (arcPath startX startY radius largeArcFlag endX endY)
        ] []

arcPath : Float -> Float -> Float -> Bool -> Float -> Float -> String
arcPath startX startY radius largeArcFlag endX endY =
    "M " ++ toString startX ++ "," ++ toString startY ++
    " A " ++ toString radius ++ " " ++ toString radius ++
    " 0 " ++ (if largeArcFlag then "1" else "0") ++
    " 1 " ++ toString endX ++ "," ++ toString endY

  --"M 297.81387,324.90424 A 25.0,25.0 0.0 0,1 297.55502,324.88016 "

numberedDayToRadius : Int -> Float
numberedDayToRadius n = startRadius + toFloat n * strokeWidth_

polarToCartesian (radius, angle) =
    (centerX + radius * cos angle, centerY - radius * sin angle)

tripsToArcData : List BeeTrip -> List ArcData
tripsToArcData trips = case trips of
    [] -> []
    (t1::_) -> List.map (tripToArcData (date t1.start)) trips

tripToArcData : Date -> BeeTrip -> ArcData
tripToArcData startDate trip =
    let timeTuple t = (hour t, minute t, second t)
    in ArcData
        (diffDays (date trip.start) startDate)
        (timeToAngle (timeTuple trip.start))
        (timeToAngle (timeTuple trip.end))

-- NOTE: This throws out the year and months in diff, it's probably over
-- simplified but works if the days are close together. Revisit this.
diffDays : Date -> Date -> Int
diffDays d1 d2 = (delta d1 d2).days

timeToAngle : (Int, Int, Int) -> Float
timeToAngle (hours, minutes, seconds) =
    let secondsIntoDay = (((hours * 60) + minutes) * 60) + seconds
        secondsPerDay = 24 * 60 * 60
        timeRatio = toFloat secondsIntoDay / toFloat secondsPerDay
        radians = timeRatio * 2 * pi
        reversed = 2 * pi - radians
    in if reversed >= 3/2 * pi
       then reversed - 3/2 * pi
       else reversed + pi / 2

splitTripsOfDayBreak : List BeeTrip -> List BeeTrip
splitTripsOfDayBreak trips =
    let startOfDay = (0,0,0,0)
        endOfDay = (23,59,59,999)
        setTime date (hours, minutes, seconds, milliseconds) =
            date
            |> setHour hours
            |> setMinute minutes
            |> setSecond seconds
            |> setMillisecond milliseconds
        splitTrip trip =
            if date trip.start == date trip.end
            then [trip]
            else [ BeeTrip trip.start (setTime trip.start endOfDay),
                   BeeTrip (setTime trip.end startOfDay) trip.end ]
    in concatMap splitTrip trips


