module JapaneseCalendar exposing (JapaneseCalendar, ymd, calendar, fromEraWithYear, fromYMD, toString)

import Array
import List.Extra
import Maybe.Extra


type alias JapaneseCalendar =
    { era : Era
    , gregorianYear : Int
    , japaneseYear : Int
    , japaneseYearString : String
    }


toString : JapaneseCalendar -> String
toString jc =
    if jc.era == unknownEra then
        "unknown"

    else
        jc.era.name ++ jc.japaneseYearString ++ "年"


ymd : Int -> Int -> Int -> Result String YMD
ymd year month day =
    let
        yearChecker y =
            if y >= 1868 then
                Nothing

            else
                Just <| "Too past year! `" ++ String.fromInt y ++ "` < 1868"

        monthChecker m =
            if 1 <= m && m <= 12 then
                Nothing

            else
                Just <| "Month `" ++ String.fromInt m ++ "` is out of range 1 and 12"

        isLeapYear y =
            modBy 4 y
                == 0
                && (modBy 100 y /= 0 || modBy 400 y == 0)

        dayChecker y m d =
            let
                lastDays =
                    Array.fromList [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

                lastDay =
                    if isLeapYear y && m == 2 then
                        29

                    else
                        Array.get (m - 1) lastDays
                            |> Maybe.withDefault 0
            in
            if 1 <= d && d <= lastDay then
                Nothing

            else
                Just <| "Day `" ++ String.fromInt d ++ "` is out of range 1 and " ++ String.fromInt lastDay

        ymdErrors y m d =
            Maybe.Extra.values [ yearChecker y, monthChecker m, dayChecker y m d ]

        monthRange =
            List.range 1 12

        errors = ymdErrors year month day
    in
    case errors of
        [] ->
            Ok <| YMD { year = year, month = month, day = day }
        _ ->
            Err <| String.join ", " errors


type YMD
    = YMD YMDRecord


type alias YMDRecord =
    { year : Int
    , month : Int
    , day : Int
    }


extract : YMD -> YMDRecord
extract ymdValue =
    case ymdValue of
        YMD record ->
            record


ymdLessThanEqual : YMD -> YMD -> Bool
ymdLessThanEqual (YMD d1) (YMD d2) =
    if d1.year /= d2.year then
        d1.year < d2.year

    else if d1.month /= d2.month then
        d1.month < d2.month

    else
        d1.day <= d2.day


toJapaneseYearString : Int -> String
toJapaneseYearString year =
    if year == 1 then
        "元"

    else
        String.fromInt year


fromYMD : YMD -> JapaneseCalendar
fromYMD (YMD date) =
    let
        dateYMD =
            YMD date

        maybeEra =
            List.Extra.find (\e -> ymdLessThanEqual e.startedOn dateYMD) calendar

        era =
            Maybe.withDefault unknownEra maybeEra

        startedOn =
            extract era.startedOn

        japaneseYear =
            date.year - startedOn.year + 1

        japaneseYearString =
            toJapaneseYearString japaneseYear
    in
    { era = era
    , gregorianYear = date.year
    , japaneseYear = japaneseYear
    , japaneseYearString = japaneseYearString
    }


fromEraWithYear : String -> Int -> JapaneseCalendar
fromEraWithYear eraName japaneseYear =
    let
        maybeEra =
            List.Extra.find (\e -> e.name == eraName) calendar

        era =
            Maybe.withDefault unknownEra maybeEra

        startedOn =
            extract era.startedOn

        gregorianYear =
            startedOn.year + japaneseYear - 1

        japaneseYearString =
            toJapaneseYearString japaneseYear
    in
    { era = era
    , gregorianYear = gregorianYear
    , japaneseYear = japaneseYear
    , japaneseYearString = japaneseYearString
    }


type alias Era =
    { name : String
    , startedOn : YMD
    }


unknownEra : Era
unknownEra =
    { name = "unknown", startedOn = YMD { year = round (-1 / 0), month = 1, day = 1 } }


calendar : List Era
calendar =
    [ { name = "令和", startedOn = YMD { year = 2019, month = 5, day = 1 } }
    , { name = "平成", startedOn = YMD { year = 1989, month = 1, day = 8 } }
    , { name = "昭和", startedOn = YMD { year = 1926, month = 12, day = 25 } }
    , { name = "大正", startedOn = YMD { year = 1912, month = 7, day = 30 } }
    , { name = "明治", startedOn = YMD { year = 1868, month = 1, day = 1 } }

    -- more?
    ]
