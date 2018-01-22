module ParseBee exposing (parseBeeData)

import Maybe
import Result
import Result.Extra
import Csv
import List
import BeeTrip exposing (BeeTrip)
import Time.DateTime exposing (DateTime, fromISO8601)

parseBeeData : String -> Result String (List BeeTrip)
parseBeeData inputStr =
    let csvData = Csv.parseWith "," inputStr
    in Result.Extra.combine (List.map rowToBeeTrip csvData.records)

rowToBeeTrip : List String -> Result String BeeTrip
rowToBeeTrip row =
  Result.andThen
    (\uid->
      Result.andThen
        (\start->
          Result.andThen
            (\end->Ok (BeeTrip uid start end))
            (parseDateAndTimeFrom row 2 3))
        (parseDateAndTimeFrom row 5 4))
    (Result.fromMaybe "out of bounds" (listAt 1 row))

parseDateAndTimeFrom : List String -> Int -> Int -> Result String DateTime
parseDateAndTimeFrom row dateIndex timeIndex =
    Result.andThen
        (\date->
            Result.andThen
                (\time-> parseDateAndTime date time)
                (Result.fromMaybe "out of bounds" (listAt timeIndex row)))
        (Result.fromMaybe "out of bounds" (listAt dateIndex row))

parseDateAndTime : String -> String -> Result String DateTime
parseDateAndTime dateStr timeStr =
    fromISO8601 (dateStr ++ "T" ++ timeStr ++ "Z")

listAt : Int -> List a -> Maybe a
listAt i l = List.head (List.drop i l)
