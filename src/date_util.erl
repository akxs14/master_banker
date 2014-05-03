-module(date_util).

-export([day_difference/2, add/2, subtract/2]).


day_difference({D1, _}, D2) ->
    day_difference(D1, D2);
day_difference(D1, {D2, _}) ->
    day_difference(D1, D2);
day_difference(D1, D2) ->
    Days1 = calendar:date_to_gregorian_days(D1),
    Days2 = calendar:date_to_gregorian_days(D2),
    Days1 - Days2.


add(Date, {days, N}) ->
    New = calendar:date_to_gregorian_days(Date) + N,
    calendar:gregorian_days_to_date(New).


subtract(Date, {days, N}) ->
    New = calendar:date_to_gregorian_days(Date) - N,
    calendar:gregorian_days_to_date(New).
