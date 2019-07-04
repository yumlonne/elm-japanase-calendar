# elm-japanese-calendar

A simple Japanese Calendar.
It can convert gregorian-date(YMDRecord) to Japanese Calendar.

# example
```.elm
> import JapaneseCalendar as JC
> JC.ymd 2000 11 24 |> JC.fromYMD |> Result.map JC.toString
Ok "平成12年" : Result (List String) String
```
