defmodule NucleotideCount do
  @nucleotides [?A, ?C, ?G, ?T]

  @doc """
  Counts individual nucleotides in a DNA strand.

  ## Examples

  iex> NucleotideCount.count('AATAA', ?A)
  4

  iex> NucleotideCount.count('AATAA', ?T)
  1
  """
  @spec count(charlist(), char()) :: non_neg_integer()
  def count([head|tail], nucleotide) do
    cond do 
      head == nucleotide ->
        1 + count(tail, nucleotide)
      true ->
        count(tail, nucleotide)
    end
  end
  def count([], nucleotide) do
    0
  end  
  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> NucleotideCount.histogram('AATAA')
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram(charlist()) :: map()
  def histogram(strand) do
    strand
    |> Enum.reduce(%{?A => 0, ?T => 0, ?C => 0, ?G => 0}, fn c, acc ->
        case Map.get(acc, c) do
          nil -> acc
          n -> Map.put(acc, c, n+1)
        end
      end)
  end
end
