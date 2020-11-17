defmodule CashMachineTest do
  use ExUnit.Case
  doctest CashMachine

  test "Withdraw R$ 30,00" do
    assert CashMachine.withdraw(30) == %{20 => 1, 10 => 1}
  end

  test "Withdraw R$ 80,00" do
    assert CashMachine.withdraw(80) == %{50 => 1, 20 => 1, 10 => 1}
  end

    test "Withdraw R$ 100,00" do
    assert CashMachine.withdraw(250) == %{100 => 2, 50 => 1}
  end

  test "Withdraw R$ 4,00" do
    assert CashMachine.withdraw(4) == %{error: "Invalid amount"}
  end

  test "Withdraw R$ 21,00" do
    assert CashMachine.withdraw(21) == %{error: "Invalid amount"}
  end

  test "Withdraw R$ 3,00" do
    assert CashMachine.withdraw(3) == %{error: "Invalid amount"}
  end
end
