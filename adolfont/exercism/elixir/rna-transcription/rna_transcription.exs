defmodule RNATranscription do
  @doc """
  Transcribes a character list representing DNA nucleotides to RNA

  ## Examples

  iex> RNATranscription.to_rna('ACTG')
  'UGAC'
  """
  @rna_map %{?G => ?C, ?C => ?G, ?T => ?A, ?A => ?U}

  @spec to_rna([char]) :: [char]

  def to_rna(list) do
    Enum.map(list, fn x -> @rna_map[x] end)
  end
end
