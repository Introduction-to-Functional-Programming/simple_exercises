defmodule Words do
  @regex ~r/[!&:$%^@,]/

  defp remove_punctuation_list([]), do: []

  defp remove_punctuation_list([head | tail]) do
    [head | remove_punctuation_list(tail)]
  end

  defp make_word_list(sentence) do
    sentence
    |> String.replace(@regex, "")
    |> String.downcase()
    |> String.split([" ", "_"], trim: true)
    |> remove_punctuation_list
  end

  defp ocurrences(key, map) do
    Map.get(map, key, 1)
  end

  defp add_to_map(map, []), do: map

  defp add_to_map(map, [head | tail]) do
    Map.update(map, head, ocurrences(head, map), &(&1 + 1))
    |> add_to_map(tail)
  end

  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t()) :: map
  def count(sentence) do
    add_to_map(%{}, make_word_list(sentence))
  end
end
