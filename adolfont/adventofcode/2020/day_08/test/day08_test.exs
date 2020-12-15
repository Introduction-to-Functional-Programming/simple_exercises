defmodule Day08Test do
  use ExUnit.Case

  test "Parse list of instructions from a string" do
    instructions = """
    acc -13
    jmp +37
    acc -19
    jmp +1
    jmp +1
    jmp +413
    """

    assert Day08.parse_list_of_instructions_from_string(instructions) ==
             [acc: -13, jmp: 37, acc: -19, jmp: 1, jmp: 1, jmp: 413]
  end

  test "Parse list of instructions from input.txt" do
    assert Day08.parse_list_of_instructions_from_file("input.txt") |> Enum.take(6) == [
             acc: -13,
             jmp: 37,
             acc: -19,
             jmp: 1,
             jmp: 1,
             jmp: 413
           ]
  end

  test "Task 1 - Example" do
    instructions = """
    nop +0
    acc +1
    jmp +4
    acc +3
    jmp -3
    acc -99
    acc +1
    jmp -4
    acc +6
    """

    assert Day08.execute_instructions(instructions) == {:infinite_loop, 5, 1, {:jmp, -3}}
  end

  test "Task 1" do
    instructions = File.read!("input.txt")

    assert Day08.execute_instructions(instructions) == {:infinite_loop, 1939, 458, {:jmp, -83}}
  end

  test "Task 2 - Example" do
    instructions = """
    nop +0
    acc +1
    jmp +4
    acc +3
    jmp -3
    acc -99
    acc +1
    jmp -4
    acc +6
    """

    task_2_result =
      Day08.execute_instructions(instructions)
      |> Day08.change_instruction(Day08.parse_list_of_instructions_from_string(instructions))
      |> IO.inspect()
      |> Day08.execute_instructions()
      |> IO.inspect()

    assert task_2_result == {:ok, 8}
  end
end
