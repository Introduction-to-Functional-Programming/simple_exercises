defmodule RnaTranscription do
  
  @spec to_rna([char]) :: [char]
  def to_rna(dna) do
    nucleotides =
      Enum.map(String.graphemes(to_string(dna)), fn nucleotide ->
        case nucleotide do
          "G" -> "C"
          "C" -> "G"
          "T" -> "A"
          "A" -> "U"
          _ -> "Unknown"
        end
      end)

    nucleotides
    |> to_string
    |> to_charlist	
  end  
end
