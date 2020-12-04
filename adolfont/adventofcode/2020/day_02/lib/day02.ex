defmodule Day02 do
  @moduledoc """
  Documentation for Advent of Code 2020 - Day 2
  """

  def process_rule(rule_as_string) do
    rule_as_string
    |> parse_rule()
    |> do_process_rule()
  end

  defp parse_rule(rule_as_string) do
    rule_as_string
    |> String.split([" ", ":", "-"])
    |> Enum.filter(fn x -> x != "" end)
  end

  defp do_process_rule([interval_start, interval_end, letter, data]) do
    letter_ocurrences = data |> String.graphemes() |> Enum.count(fn x -> x == letter end)

    letter_ocurrences >= String.to_integer(interval_start) and
      letter_ocurrences <= String.to_integer(interval_end)
  end
end
