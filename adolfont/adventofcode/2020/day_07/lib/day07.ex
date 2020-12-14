defmodule Day07 do
  @moduledoc """
  Advent of Code 2020 - Day 07 - Handy Haversacks
  https://adventofcode.com/2020/day/7
  """

  @doc """
  Parses a string that describes as rule as a list of tuples where:
  {contained_bag, number, container_bag} meaning that
   container_bag can contain a number of contained_bag

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

  defp process_second_part(string, first_color) do
    [num, second_color_1, second_color_2 | _] = String.split(string, " ")
    {restore_color(second_color_1, second_color_2), String.to_integer(num), first_color}
  end

  defp restore_color(first, second) do
    Enum.join([first, second], " ")
  end

  @spec process_set_of_rules(binary) :: [{binary, integer(), binary}]
  @doc """
  Converts a long string that contains the description of rules into a list of tuples.
  See Task 1 of https://adventofcode.com/2020/day/7
  """
  def process_set_of_rules(rules) do
    rules
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_rule/1)
    |> List.flatten()
  end

  @spec can_contain(binary, binary) :: [binary]
  @doc """
  Given the color of a bag 1 (a string) and a set of rules (also a string), returns the names of the colors the bags
  that can contain bag 1.

  See Task 1 of https://adventofcode.com/2020/day/7
  """
  def can_contain(color, rules) do
    do_can_contain(color, process_set_of_rules(rules), 0, [])
    |> Enum.map(fn {x, _y} -> x end)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp do_can_contain(_color, rules, current_position, total)
       when current_position == length(rules) do
    total
  end

  defp do_can_contain(color, rules, current_position, total) do
    total ++
      can_contain_recursively(color, Enum.at(rules, current_position), rules) ++
      do_can_contain(color, rules, current_position + 1, total)
  end

  defp can_contain_recursively(color, {color, number, container_color}, rules) do
    [{container_color, number}] ++ do_can_contain(container_color, rules, 0, [])
  end

  defp can_contain_recursively(_color, _head_rule, _tail_rules) do
    []
  end

  @spec must_contain(binary, binary) :: number
  @doc """
  Given the color of a bag (a string) and a set of rules (also a string), returns the number of bags
  that a bag must contain.

  See Task 2 of https://adventofcode.com/2020/day/7
  """
  def must_contain(color, rules) do
    rules
    |> process_set_of_rules()
    |> do_must_contain({color, 1}, 0)
  end

  defp do_must_contain(rules, {color, _number}, total) do
    rules
    |> Enum.filter(fn {_contained, _number, rule_color} -> color == rule_color end)
    |> Enum.map(fn {contained, number, _} -> {contained, number} end)
    |> recursively_find_number_of_bags(rules, total)
  end

  defp recursively_find_number_of_bags([], _rules, total) do
    total
  end

  defp recursively_find_number_of_bags([head | tail], rules, total) do
    {color, number} = head

    number + number * do_must_contain(rules, {color, 1}, 0) +
      recursively_find_number_of_bags(tail, rules, total)
  end
end
