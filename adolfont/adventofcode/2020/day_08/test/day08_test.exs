defmodule Day08Test do
  use ExUnit.Case

  test "Parses a string containing a list of instructions" do
    instructions = """
    acc -13
    jmp +37
    acc -19
    jmp +1
    jmp +1
    jmp +413
    """

    assert Day08.parse_instructions(instructions) ==
             [acc: -13, jmp: 37, acc: -19, jmp: 1, jmp: 1, jmp: 413]
  end

  test "Parse list of instructions from input.txt" do
    input_file = File.read!("input.txt")

    assert Day08.parse_instructions(input_file) |> Enum.take(6) == [
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

    assert Day08.execute_instructions(instructions) == {:infinite_loop, 5, 4}
  end

  test "Task 1" do
    instructions = File.read!("input.txt")

    assert Day08.execute_instructions(instructions) == {:infinite_loop, 1939, 541}
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

    assert Day08.task_2(instructions) == {:ok, 8}
  end

  test "Task 2 with MY input file" do
    instructions = File.read!("input.txt")

    assert Day08.task_2(instructions) == {:ok, 2212}
  end
end
