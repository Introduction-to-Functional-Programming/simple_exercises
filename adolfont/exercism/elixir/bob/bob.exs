defmodule Bob do
  defp question?(string) do
    String.ends_with?(string, "?")
  end

  defp only_spaces?(string) do
    String.trim(string) == ""
  end

  defp only_uppercase?(string) do
    String.upcase(string) == string
  end

  defp only_numbers?(string) do
    String.upcase(string) == String.downcase(string)
  end

  def hey(input) do
    cond do
      question?(input) && only_uppercase?(input) && not(only_numbers?(input)) ->
        "Calm down, I know what I'm doing!"

      question?(input) ->
        "Sure."

      only_spaces?(input) ->
        "Fine. Be that way!"

      only_numbers?(input) ->
        "Whatever."

      only_uppercase?(input) ->
        "Whoa, chill out!"

      true ->
        "Whatever."
    end
  end
end
