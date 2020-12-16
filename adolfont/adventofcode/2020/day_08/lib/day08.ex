defmodule Day08 do
  @moduledoc """
  Advent of Code - Day 8: Handheld Halting https://adventofcode.com/2020/day/8
  """

  def parse_instructions(string) do
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

  def execute_instructions(instructions) when is_list(instructions) do
    do_execute_instructions(instructions, 0, 0, [])
  end

  def execute_instructions(instructions) do
    parse_instructions(instructions)
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
        {:infinite_loop, new_acc, current_instruction_index}

      # {:infinite_loop, new_acc, current_instruction_index, current_instruction}

      true ->
        do_execute_instructions(instructions, new_acc, current_instruction_index + offset, [
          current_instruction_index | executed_instructions
        ])
    end
  end

  defp process_instruction({:nop, _}, acc) do
    {+1, acc}
  end

  defp process_instruction({:acc, value}, acc) do
    {+1, acc + value}
  end

  defp process_instruction({:jmp, value}, acc) do
    {value, acc}
  end

  def task_2(instructions) do
    instruction_list = parse_instructions(instructions)

    do_execute_instructions(instruction_list, 0, 0, [])

    do_task_2(instruction_list, instruction_list, [])
  end

  defp do_task_2(instruction_list, original_instruction_list, already_tried) do
    case do_execute_instructions(instruction_list, 0, 0, []) do
      {:infinite_loop, _acc, _instruction_index} ->
        {changed_instruction_list, already_tried} =
          change_next_instruction(original_instruction_list, already_tried)

        do_task_2(changed_instruction_list, original_instruction_list, already_tried)

      {:ok, acc} ->
        {:ok, acc}
    end
  end

  def change_next_instruction(original_instruction_list, already_tried) do
    {first_part, second_part} =
      Enum.split(
        original_instruction_list,
        max_tried(already_tried) + 1
      )

    next_change = find_next_jmp_or_nop(second_part)
    instruction = Enum.at(second_part, next_change)

    {first_part ++ change_instruction_list(second_part, next_change, instruction),
     [next_change + length(first_part) | already_tried]}
  end

  defp max_tried([]), do: -1
  defp max_tried(list), do: Enum.max(list)

  defp find_next_jmp_or_nop(list) do
    Enum.find_index(list, fn {instruction, _} -> instruction in [:nop, :jmp] end)
  end

  defp change_instruction_list(instruction_list, instruction_index, {:jmp, value}) do
    List.replace_at(instruction_list, instruction_index, {:nop, value})
  end

  defp change_instruction_list(instruction_list, instruction_index, {:nop, value}) do
    List.replace_at(instruction_list, instruction_index, {:jmp, value})
  end
end
