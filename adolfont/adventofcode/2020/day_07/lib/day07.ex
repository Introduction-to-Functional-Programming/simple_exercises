defmodule Day07 do
  @moduledoc """
  Advent of Code 2020 - Day 07 - Handy Haversacks 
  https://adventofcode.com/2020/day/7
  """

  def parse_rule(string) do
    [first, second | rest] = String.split(string, " ")
    rest = rest |> Enum.join(" ")
    process_rest(restore_color(first, second), rest)
  end

  defp process_rest(first_color, "bags contain no other bags.") do
    [{"", 0, first_color}]
  end

  defp process_rest(first_color, string) do
    string
    |> String.replace_prefix("bags contain", "")
    |> String.split(",", trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.map(&process_second_part(&1, first_color))
  end

  def process_second_part(string, first_color) do
    [num, second_color_1, second_color_2 | _] = String.split(string, " ")
    {restore_color(second_color_1, second_color_2), String.to_integer(num), first_color}
  end

  defp restore_color(first, second) do
    Enum.join([first, second], " ")
  end


  def process_set_of_rules(rules) do
    rules
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_rule/1)
    |> List.flatten()
  end

  def can_contain(color, rules) do
    do_can_contain(color, process_set_of_rules(rules). 0)
  end

  defp do_can_contain(color, [], total) do
    total
  end
  defp do_can_contain(color, [head_rule | tail_rules], total) do
    total + 
  end
end
