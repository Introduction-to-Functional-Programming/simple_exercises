defmodule HarryPotterBookstore do
  def books_bought(shopping_cart) do
    Enum.map(shopping_cart, fn {livro, _quantidade} -> livro end)
  end
  defp number_books(shopping_cart) do
    Enum.map(shopping_cart, fn {_, quantid} -> quantid end) |> Enum.sum()
  end

  def full_price(shopping_cart, price) do
    (Enum.map(shopping_cart, fn {_livro, quantidade} -> quantidade end) |> Enum.sum()) * price
  end

  def discount_two_copies(shopping_cart, price) do
    quantity = number_books(shopping_cart)
    (div_by_2(quantity) + rem_by_2(quantity)) * price
  end

  defp div_by_2(quantidade) do
    div(quantidade, 2) * 0.95 * 2
  end

  defp rem_by_2(quantidade) do
    rem(quantidade, 2)
  end
end
