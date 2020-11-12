-module(leap).

-export([leap_year/1]).

leap_year(Year) ->
    divisible_by_4(Year) and not divisible_by_100(Year) or divisible_by_400(Year).

divisible_by_4(Year) ->
    Year rem 4 == 0.

divisible_by_100(Year) ->
    Year rem 100 == 0.

divisible_by_400(Year) ->
    Year rem 400 == 0.
