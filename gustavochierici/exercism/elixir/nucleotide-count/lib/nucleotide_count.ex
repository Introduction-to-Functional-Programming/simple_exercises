defmodule NucleotideCount do
  @nucleotides [?A, ?C, ?G, ?T] #I don't know how to use that

  @doc """
  Counts individual nucleotides in a DNA strand.

  ## Examples

  iex> NucleotideCount.count('AATAA', ?A)
  4

  iex> NucleotideCount.count('AATAA', ?T)
  1
  """
  @spec count(charlist(), char()) :: non_neg_integer()
  def count(strand, nucleotide) do
    strand
    |> Enum.count(fn x -> x == nucleotide end)
  end

  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> NucleotideCount.histogram('AATAA')
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram(charlist()) :: map()
  def histogram(strand) do
    %{?A => count(strand, ?A), ?T => count(strand, ?T), ?C => count(strand, ?C), ?G => count(strand, ?G)}
  end
end
