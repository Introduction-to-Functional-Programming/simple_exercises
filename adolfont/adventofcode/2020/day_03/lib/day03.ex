defmodule Day03 do
  @moduledoc """
  Advent of Code 2020 - Day 3 - Read description at https://adventofcode.com/2020/day/3
  """

  def task_1(input, right, down) do
    list_of_lines = process_input_task_1(input)

    do_task_1(list_of_lines, right, down, 0, 1)
  end

  defp do_task_1(list, _right, down, counter, _current_x) when length(list) <= down do
    counter
  end

  defp do_task_1(list_of_lines, right, down, counter, current_x) do
    next = Enum.drop(list_of_lines, down)

    char =
      list_of_lines
      |> get_line(down)
      |> Enum.join()
      |> circularStringAt(current_x + right)

    result = if char == "#", do: 1, else: 0

    do_task_1(next, right, down, counter + result, current_x + right)
  end

  defp circularStringAt(string, position) do
    result = String.at(string, position - 1)

    if result != nil do
      result
    else
      circularStringAt(string, position - String.length(string))
    end
  end

  defp get_line(array, line) do
    Enum.at(array, line)
  end

  def process_input_task_1(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.graphemes/1)
  end

  def process_file(filename) do
    File.read!(filename)
  end
end
