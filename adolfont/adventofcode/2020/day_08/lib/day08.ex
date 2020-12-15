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
    |> do_execute_instructions(0, 0, [])
  end

  defp do_execute_instructions(
         instructions,
         acc,
         current_instruction_index,
         _executed_instructions
       )
       when current_instruction_index == length(instructions) do
    {:ok, acc}
  end

  defp do_execute_instructions(
         instructions,
         acc,
         current_instruction_index,
         executed_instructions
       ) do
    current_instruction = Enum.at(instructions, current_instruction_index)

    {offset, new_acc} = process_instruction(current_instruction, acc)

    cond do
      (current_instruction_index + offset) in executed_instructions ->
        {:infinite_loop, new_acc}

      true ->
        do_execute_instructions(instructions, new_acc, current_instruction_index + offset, [
          current_instruction_index | executed_instructions
        ])
    end
  end

  def process_instruction({:nop, _}, acc) do
    {+1, acc}
  end

  def process_instruction({:acc, value}, acc) do
    {+1, acc + value}
  end

  def process_instruction({:jmp, value}, acc) do
    {value, acc}
  end
end
