-module(rna_transcription).

-export([to_rna/1]).

to_rna([]) ->
    [];
to_rna(Strand) ->
    RnaMap = #{$A => $U, $C => $G, $G => $C, $T => $A},
    lists:map(fun (X) ->
                      maps:get(X, RnaMap)
              end,
              Strand).
