defmodule Day08 do
  @moduledoc """
  Advent of Code - Day 8: Handheld Halting https://adventofcode.com/2020/day/8
  """

  def parse_list_of_instructions_from_file(filename) do
    filename
    |> File.read!()
    |> parse_list_of_instructions_from_string()
  end

  def parse_list_of_instructions_from_string(string) do
    string
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_instruction/1)
  end

  defp parse_instruction("acc " <> value) do
    {:acc, String.to_integer(value)}
  end

  defp parse_instruction("jmp " <> value) do
    {:jmp, String.to_integer(value)}
  end

  defp parse_instruction("nop " <> value) do
    {:nop, String.to_integer(value)}
  end

  def execute_instructions(instructions) do
    parse_list_of_instructions_from_string(instructions)
    |> do_execute_instructions(0, [])
  end

  defp do_execute_instructions(instructions, acc, executed_instructions) do
    {:infinite_loop, 5}
  end
end
