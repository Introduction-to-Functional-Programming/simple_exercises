defmodule BeerSong do
  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(x) do
    """
    #{String.capitalize(number_or_word(x))} #{bottles_wall(x)}, #{number_or_word(x)} #{
      bottles_beer(x)
    }.
    #{second_phrase(x)}
    """
  end

  defp bottles_wall(x) do
    "#{bottles_beer(x)} on the wall"
  end

  defp bottles_beer(x) do
    "bottle#{add_s(x)} of beer"
  end

  defp add_s(0), do: "s"
  defp add_s(n) when n > 1, do: "s"
  defp add_s(_), do: ""

  defp number_or_word(0), do: "no more"
  defp number_or_word(x), do: Integer.to_string(x)

  defp one_or_it(1), do: "it"
  defp one_or_it(_), do: "one"

  defp second_phrase(0) do
    "Go to the store and buy some more, 99 #{bottles_wall(99)}."
  end

  defp second_phrase(x) do
    "Take #{one_or_it(x)} down and pass it around, #{number_or_word(x - 1)} #{bottles_wall(x - 1)}."
  end

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range \\ 99..0) do
    for(i <- range, do: verse(i))
    |> Enum.join("\n")
  end
end
