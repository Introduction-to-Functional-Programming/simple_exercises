-module(exercism_parse_transform).

%% API exports
-export([parse_transform/2]).

%%====================================================================
%% API functions
%%====================================================================

parse_transform(Form, _Option) ->
    {ok, TestModuleName} = find_module_name(Form),
    {ok, ModuleName} = extract_tested_module(TestModuleName),

    case is_example() of
        false ->
            Form;
        true ->
            parse_trans:plain_transform(fun (F) -> do_transform(F, ModuleName) end, Form)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

is_example() ->
    {ok, Files} = file:list_dir("./src"),
    lists:member("example.erl", Files).

find_module_name([]) -> {error, notfound};
find_module_name([{attribute,_Line,module,Module}|_]) -> {ok, Module};
find_module_name([_|T]) -> find_module_name(T).

extract_tested_module(ModuleName) when is_atom(ModuleName) ->
    ModuleName1 = atom_to_list(ModuleName),
    case re:run(ModuleName1, "^(.*)_tests$") of
        {match, [_, {0, Length}]} ->
            {ModuleName2, _} = lists:split(Length, ModuleName1),
            {ok, list_to_atom(ModuleName2)};
        _ ->
            error
    end.

do_transform({call, LCall, {remote, LRemote, {atom, L1, ModuleName}, Fn}, Args}, ModuleName) ->
    TransformedArgs = parse_trans:plain_transform(fun (F) -> do_transform(F, ModuleName) end, Args),
    {call, LCall, {remote, LRemote, {atom, L1, example}, Fn}, TransformedArgs};
do_transform(_Form, _ModuleName) ->
    continue.

%%====================================================================
%% Internal testing
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(_assertExtract(Exp, Module), ?_assertMatch(Exp, extract_tested_module(Module))).

extract_tested_module_test_() ->
    [?_assertExtract({ok, hello_world}, hello_world_tests)
    ,?_assertExtract({ok, zipper},      zipper_tests)
    ,?_assertExtract(error,             hello_world)
    ,?_assertExtract(error,             zipper)
    ].

-endif.
