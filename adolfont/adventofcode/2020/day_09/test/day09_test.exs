defmodule Day09Test do
  use ExUnit.Case

  test "Task 1 example" do
    input = """
    35
    20
    15
    25
    47
    40
    62
    55
    65
    95
    102
    117
    150
    182
    127
    219
    299
    277
    309
    576
    """

    number_list = Day09.processString(input)
    assert Day09.task1(number_list, 5) == 127
    assert Day09.checksum(number_list, 5) == {:error, {:invalid, 127}}
  end

  test "Task 1 with input provided by Advent of Code" do
    number_list = Day09.processString(File.read!("input.txt"))
    assert Day09.task1(number_list, 25) == 20_874_512
    assert Day09.checksum(number_list, 25) == {:error, {:invalid, 20_874_512}}
  end
end
