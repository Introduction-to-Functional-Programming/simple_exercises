defmodule CashMachine do
  @bank_notes [100, 50, 20, 10, 5, 2]

  defp count_notes(0, _, notes_compound_result) do
    notes_compound_result
  end

  defp count_notes(_, [], _) do
    %{:error => "Invalid amount"}
  end

  defp count_notes(amount, [note_value | bank_notes], notes_compound_result) do
    notes_quantity = div(amount, note_value)
    subtracted_amount = amount - notes_quantity * note_value

    count_notes(
      subtracted_amount,
      bank_notes,
      Map.put_new(notes_compound_result, note_value, notes_quantity)
    )
  end

  def withdraw(amount) do
    amount
    |> count_notes(@bank_notes, %{})
    |> Enum.filter(fn {_note_value, note_quantity} -> note_quantity > 0 end)
    |> Map.new()
  end

  """
  ordeno o map retornado para que os primeiros elementos sejam os que tenham qtde > 0
  Reduce possui um acumulador. Então executo o reduce enquanto a qtde de notas do elemento for > 0, adicionando no acumulador do reduce este elemento
  se qtde de notas = 0, paro o reduce (:halt), não é necessário verificar os demais elementos pq todos eles estarão com a qtde notas = 0
  (foram ordenados no passo anterior: Enum.sort)
  """
  def withdraw_reduce(amount) do
    amount |> count_notes(@bank_notes, %{})
           |> Enum.sort(fn ({_k1, val1}, {_k2, val2}) -> val1 > val2 end)
           |> Enum.reduce_while([], fn(x, acc) ->
                                      qtde_note = elem(x, 1)
                                      if qtde_note > 0, do: {:cont, [x | acc]}, else: {:halt, acc}
                                    end)
						
  end
end