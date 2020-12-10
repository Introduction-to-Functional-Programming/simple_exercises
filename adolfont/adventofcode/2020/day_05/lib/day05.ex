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
    filename
    |> find_all_seats()
    |> Enum.max()
  end

  defp find_all_seats(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&convert_to_seat_id/1)
  end

  def find_my_seat(filename) do
    filename
    |> find_all_seats()
    |> do_find_my_seat()
  end

  def find_my_seat_v1(filename) do
    filename
    |> find_all_seats()
    |> do_find_my_seat_v1()
  end

  def do_find_my_seat(seat_list) do
    sorted_list = seat_list
    max = Enum.max(sorted_list)
    min = Enum.min(sorted_list)

    min..max
    |> Enum.filter(fn x -> not (x in sorted_list) end)
    |> hd()
  end

  def do_find_my_seat_v1(seat_list) do
    sorted_list = seat_list |> Enum.sort()

    find_my_seat_on_sorted_list(sorted_list)
  end

  defp find_my_seat_on_sorted_list([i]) do
    i
  end

  defp find_my_seat_on_sorted_list([i, j | tail]) when j == i + 1 do
    find_my_seat_on_sorted_list([j | tail])
  end

  defp find_my_seat_on_sorted_list([i | _tail]) do
    i + 1
  end
end
