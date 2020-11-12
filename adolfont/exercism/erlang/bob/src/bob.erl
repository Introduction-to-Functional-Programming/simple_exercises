-module(bob).

-export([response/1]).

response(String) ->
    case process(string:trim(String)) of
        silence ->
            "Fine. Be that way!";
        question ->
            "Sure.";
        question_no_letters ->
            "Sure.";
        no_letters ->
            "Whatever.";
        yell_question ->
            "Calm down, I know what I'm doing!";
        yell ->
            "Whoa, chill out!";
        rest ->
            "Whatever."
    end.

process([]) ->
    silence;
process(String) ->
    case contains_char(string:uppercase(String)) of
        true ->
            process(String, string:uppercase(String), lists:last(String));
        _ ->
            process_no_letters(String, lists:last(String))
    end.

process(S, S, $?) ->
    yell_question;
process(S, S, _) ->
    yell;
process(_, _, $?) ->
    question;
process(_, _, _) ->
    rest.

process_no_letters(_String, $?) ->
    question_no_letters;
process_no_letters(_String, _) ->
    no_letters.

contains_char(String) ->
    lists:any(fun(X) -> (X >= $A) and (X =< $Z) end, String).
