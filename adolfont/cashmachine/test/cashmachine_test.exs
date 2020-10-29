defmodule CashmachineTest do
  use ExUnit.Case
  doctest CashMachine

  test "There is no bills for 1 dollar" do
    assert CashMachine.withdraw(1) == %{error: "Invalid amount"}
  end

  test "2 dollars give me a 2-dollar bill" do
    assert CashMachine.withdraw(2) == %{2 => 1}
  end
end

# IO.gets("Digite o valor: ")
# |> String.trim()
# |> String.to_integer()
# |> CashMachine.withdraw_reduce()
# |> IO.inspect()
