module BeeTrip exposing (BeeTrip)

import Time.DateTime exposing (DateTime)

type alias BeeTrip = {
    uid : String,
    start : DateTime,
    end : DateTime
    }
