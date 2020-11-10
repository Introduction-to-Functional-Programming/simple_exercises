defmodule CashmachineTest do
  use ExUnit.Case
  doctest CashMachine

  test "There is no bills for 1 dollar" do
    assert CashMachine.withdraw(1) == %{error: "Invalid amount"}
  end

  test "2 dollars give me a 2-dollar bill" do
    assert CashMachine.withdraw(2) == %{2 => 1}
  end

  test "187 dollars gives me one of each bill" do
    assert CashMachine.withdraw(187) == %{2 => 1, 5 => 1, 10 => 1, 20 => 1, 50 => 1, 100 => 1}
  end

  test "200 dollars gives me two 100 dollar bills" do
    assert CashMachine.withdraw(200) == %{100 => 2}
  end

  test "Tests using Enum.reduduce_while" do
    assert CashMachine.withdraw_reduce(200) ==  [{100, 2}]

    assert CashMachine.withdraw_reduce(187) == [{2, 1}, {5, 1}, {10, 1}, {20, 1}, {50, 1}, {100, 1}]

    assert CashMachine.withdraw_reduce(289) ==  [{5, 1}, {10, 1}, {20, 1}, {50, 1}, {2, 2}, {100, 2}]
  end
end