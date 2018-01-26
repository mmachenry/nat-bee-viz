module ParseBee exposing (parseBeeData)

import Maybe
import Result
import Result.Extra
import Csv
import List
import BeeTrip exposing (BeeTrip)
import Time.DateTime exposing (DateTime, fromISO8601)
import Regex exposing (..)

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
            (parseDateAndTimeFrom row 8))
        (parseDateAndTimeFrom row 7))
    (Result.fromMaybe "out of bounds" (listAt 1 row))

parseDateAndTimeFrom : List String -> Int -> Result String DateTime
parseDateAndTimeFrom row index =
    Result.andThen parseDateAndTime
        (Result.fromMaybe "out of bounds" (listAt index row))

parseDateAndTime : String -> Result String DateTime
parseDateAndTime str =
    let withT = replace (AtMost 1) (regex " ") (\m->"T") (String.trim str)
        withTandZ = replace (AtMost 1) (regex "$") (\m->"Z") withT
    in case fromISO8601 withTandZ of
        Ok x -> Ok x
        Err e -> Err (e ++ " Argument:" ++ withTandZ)

listAt : Int -> List a -> Maybe a
listAt i l = List.head (List.drop i l)
