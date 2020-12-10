defmodule Day5Test do
  use ExUnit.Case

  test "Converts a string to a row number" do
    assert Day5.to_binary("FFFFFFF") == 0
    assert Day5.to_binary("BBBBBBB") == 127
    assert Day5.to_binary("FBFBBFF") == 44
    assert Day5.to_binary("BFFFBBF") == 70
    assert Day5.to_binary("FFFBBBF") == 14
    assert Day5.to_binary("BBFFBBF") == 102
  end

  test "Converts a string to a column number" do
    assert Day5.to_binary("RLR") == 5
    assert Day5.to_binary("RRR") == 7
    assert Day5.to_binary("RLL") == 4
  end

  test "Finds the seat ID for a seat string" do
    assert Day5.seat_id("FBFBBFFRLR") == 357
  end

  test "Task 1 - fin max seat" do
    assert Day5.find_max_seat("input.txt") == 828
  end

  test "Task 2 - find my seat" do
    assert Day5.find_my_seat("input.txt") == 565
  end

end
