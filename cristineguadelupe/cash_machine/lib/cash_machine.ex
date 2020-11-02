defmodule CashMachine do
  defstruct amount: 0, bills: []

  def withdraw(amount) do
    __struct__()
    |> amount(amount)
    |> bills()
  end

  def amount(atm, amount) do
    %{atm | amount: valid?(amount)}
  end

  def bills(atm) when atm.amount == 0 do
    %{atm | amount: Enum.sum(atm.bills)}
  end
  def bills(atm) when atm.amount >= 100 do
    %{atm | amount: atm.amount - 100, bills: [100 | atm.bills]}
    |> bills
  end
  def bills(atm) when atm.amount >= 50 do
    %{atm | amount: atm.amount - 50, bills: [50 | atm.bills]}
    |> bills
  end
  def bills(atm) when atm.amount >= 20 do
    %{atm | amount: atm.amount - 20, bills: [20 | atm.bills]}
    |> bills
  end
  def bills(atm) when atm.amount == 10 do
    %{atm | amount: atm.amount - 10, bills: [10 | atm.bills]}
    |> bills
  end

  defp valid?(amount) when rem(amount, 10) == 0, do: amount
  defp valid?(_amount), do: 0
end
