defmodule Day05 do
  @moduledoc """
  Advent of Code - Day 5 - Binary Boarding https://adventofcode.com/2020/day/5
  """

  def convert_to_row(string) do
    do_convert(string, "F", "B")
  end

  def convert_to_column(string) do
    do_convert(string, "L", "R")
  end

  def do_convert(string, zero_char, one_char) do
    string
    |> String.replace(zero_char, "0")
    |> String.replace(one_char, "1")
    |> String.to_integer(2)
  end

  def convert_to_seat_id(string) do
    {row_string, column_string} = String.split_at(string, -3)

    convert_to_row(row_string) * 8 + convert_to_column(column_string)
  end

  def find_max_seat(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&convert_to_seat_id/1)
    |> Enum.max()
  end
end
