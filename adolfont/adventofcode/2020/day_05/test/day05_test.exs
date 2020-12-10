defmodule Day05Test do
  use ExUnit.Case

  test "Converts a string to a row number" do
    assert Day05.convert_to_row("FFFFFFF") == 0
    assert Day05.convert_to_row("BBBBBBB") == 127
    assert Day05.convert_to_row("FBFBBFF") == 44
    assert Day05.convert_to_row("BFFFBBF") == 70
    assert Day05.convert_to_row("FFFBBBF") == 14
    assert Day05.convert_to_row("BBFFBBF") == 102
  end

  test "Converts a string to a column number" do
    assert Day05.convert_to_column("RLR") == 5
    assert Day05.convert_to_column("RRR") == 7
    assert Day05.convert_to_column("RLL") == 4
  end

  test "Finds the seat ID for a seat string" do
    assert Day05.convert_to_seat_id("FBFBBFFRLR") == 357
  end

  test "Task 1" do
    assert Day05.find_max_seat("input.txt") == 888
  end

  test "Task 2 - find my seat" do
    assert Day05.find_my_seat("input.txt") == 522
    assert Day05.find_my_seat_v1("input.txt") == 522
  end
end
