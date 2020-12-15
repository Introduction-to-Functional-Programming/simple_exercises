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

    assert Day08.execute_instructions(instructions) == {:infinite_loop, 5}
  end
end
