module DrawBee exposing (..)

import List exposing (concatMap)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time.DateTime exposing (DateTime, date, hour, minute, second,
                                setHour, setMinute, setSecond, setMillisecond)
import Time.Date exposing (Date, delta)
import Html exposing (Html)
import BeeTrip exposing (..)

type alias DrawParams = {
    width : Float,
    height : Float,
    strokeWidth : Float,
    startRadius : Float
    }

type alias ArcData = {
    numDay : Int,
    startAngle : Float,
    endAngle : Float
    }

drawConcentricCircles : DrawParams -> List BeeTrip -> List (Svg a)
drawConcentricCircles params trips =
    List.map (describeArc params) (tripsToArcData (splitTripsOfDayBreak trips))

describeArc : DrawParams -> ArcData -> Svg a
describeArc params arc =
    let radius = params.startRadius + toFloat arc.numDay * params.strokeWidth
    in Svg.path [
        fill "none",
        stroke "black",
        strokeWidth (toString params.strokeWidth),
        d (arcClockwise radius arc.startAngle arc.endAngle)
        ] []

polarToCartesian : (Float, Float) -> (Float, Float)
polarToCartesian (radius, angle) = (radius * cos angle, radius * sin angle)

arcClockwise : Float -> Float -> Float -> String
arcClockwise r startAngle endAngle =
    let (startX, startY) = polarToCartesian (r, startAngle)
        (endX, endY) = polarToCartesian (r, endAngle)
        largeArcFlag =
            if startAngle > endAngle
            then startAngle - endAngle > pi
            else endAngle - startAngle < pi
        sweepFlag = True
    in arcPath startX startY r largeArcFlag sweepFlag endX endY

arcPath : Float -> Float -> Float -> Bool -> Bool -> Float -> Float -> String
arcPath startX startY radius largeArcFlag sweepFlag endX endY =
    String.join " " [
        "M", toString startX, toString startY,
        "A", toString radius, toString radius,
        "0",
        if largeArcFlag then "1" else "0",
        if sweepFlag then "1" else "0",
        toString endX, toString endY
        ]

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
            else [ BeeTrip trip.uid trip.start (setTime trip.start endOfDay),
                   BeeTrip trip.uid (setTime trip.end startOfDay) trip.end ]
    in concatMap splitTrip trips
