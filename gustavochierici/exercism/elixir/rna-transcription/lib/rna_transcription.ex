defmodule RnaTranscription do
  @guanine ?G
  @cytosine ?C
  @adenine ?A
  @thymine ?T
  @uracil ?U
  @doc """
  Transcribes a character list representing DNA nucleotides to RNA

  ## Examples

  iex> RnaTranscription.to_rna('ACTG')
  'UGAC'
  """
  @spec to_rna([char]) :: [char]
  def to_rna(dna) do
    dna
    |> Enum.map(fn dna_char ->
      case dna_char do
        @guanine  -> @cytosine
        @cytosine -> @guanine
        @thymine  -> @adenine
        @adenine  -> @uracil
        _ -> 'Error'
      end
    end)
  end
end
