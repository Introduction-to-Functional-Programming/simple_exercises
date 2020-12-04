defmodule Day02Test do
  use ExUnit.Case

  test "Reads the input and verifies the third line" do
    {:ok, all_lines} = File.read("input.txt")

    line_list = all_lines |> String.split("\n", trim: true)
    assert Enum.at(line_list, 2) == "6-7 j: jjjjjwrj"
  end

  test "Processes the rule 1-3 a for some examples" do
    rule = "1-3 a: abcde"
    assert Day02.process_rule(rule)
    rule = "1-3 a: bbcde"
    refute Day02.process_rule(rule)
    rule = "1-3 b: bbbdbbeb"
    refute Day02.process_rule(rule)
  end

  test "Reads the input and counts how many are valid passwords" do
    {:ok, all_lines} = File.read("input.txt")

    line_list = all_lines |> String.split("\n", trim: true)

    valid_passwords_amount =
      line_list
      |> Enum.count(&Day02.process_rule/1)

    assert valid_passwords_amount == 564
  end
end
