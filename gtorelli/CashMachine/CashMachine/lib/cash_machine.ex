"""
INTRODUCTION TO FUNCTIONAL PROGRAMING
STUDENT: GABRIEL DE A. TORELLI
PROFESSOR: ADOLFO NETO

Develop a program that simulates the delivery of bills when a customer makes a withdrawal at an ATM. 
The basic requirements are as follows:

- Deliver the fewest notes;
- It is possible to withdraw the requested amount with the available notes;
- Infinite customer balance;
- Infinite banknote quantity (a finite amount of banknotes can be added to increase the difficulty of the problem);
- Available notes of R 100.00; R 50.00; R 20.00 and R 10.00
"""

defmodule CashMachine do
  # DECLARE "notes_value" AS CONSTANT
  @notes_value [100, 50, 20, 10]
  # COUNT NOTES PRIVATE FUNCTIONS WITH PATTERN MATCHING.
  defp count_notes(0, _, result) do
    result
  end

  # CASE LIST ARE EMPTY
  defp count_notes(_, [], _) do
    %{:error => "Invalid amount"}
  end

  # DEFAULT CASE
  defp count_notes(amount, [note_value | notes_value], result) do
    quantity = div(amount, note_value)
    sub_amount = amount - quantity * note_value

    count_notes(
      sub_amount,
      notes_value,
      Map.put_new(result, note_value, quantity)
    )
  end

  def withdraw(amount) do
    amount
    |> count_notes(@notes_value, %{})
    |> Enum.reject(fn {_note_value, note_quantity} -> note_quantity == 0 end)
    |> Map.new()
  end
end
