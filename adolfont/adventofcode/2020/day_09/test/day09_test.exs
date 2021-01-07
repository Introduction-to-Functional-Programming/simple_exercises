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

  test "Task 2 example" do
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

    assert Day09.find_contiguous_set_which_sums_to(number_list, 5) ==
             {:ok, [15, 25, 47, 40]}

    assert Day09.find_encryption_weakness(number_list, 5) == 62
  end

  test "Task 2 with input provided by Advent of Code" do
    number_list = Day09.processString(File.read!("input.txt"))

    assert Day09.find_contiguous_set_which_sums_to(number_list, 25) ==
             {
               :ok,
               [
                 925_549,
                 966_007,
                 1_012_779,
                 1_054_812,
                 1_220_613,
                 1_147_519,
                 1_158_705,
                 1_163_492,
                 1_204_640,
                 1_209_427,
                 1_215_826,
                 1_227_012,
                 1_231_799,
                 1_344_496,
                 1_349_283,
                 1_355_682,
                 2_086_871
               ]
             }

    assert Day09.find_encryption_weakness(number_list, 25) == 3_012_420
  end
end
