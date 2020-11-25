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

  def shopping_cart_map({book, quantity}) do
    %{book => quantity}
  end

  defstruct [{book,quantity}]
  def shopping_cart_keyword(:livro, quantidade) do
    [{livro:quantidade}]
  end

  def discount_three_copies(shopping_cart, price) do
    quantidade = number_books(shopping_cart)
    ((div(quantidade,3) * 0.90 * 3) + (rem(quantidade,3))) * price
  end

  def discount_four_copies(shopping_cart, price) do
    quantidade = number_books(shopping_cart)
    ((div(quantidade,4) * 0.80 * 4) + (rem(quantidade,4))) * price
  end

  def discount_five_copies(shopping_cart, price) do
    quantidade = number_books(shopping_cart)
    ((div(quantidade,5) * 0.75 * 5) + (rem(quantidade,5))) * price
  end

  def discount_all(shopping_cart, price) do
    quantidade = number_books(shopping_cart)
    cond do
      quantidade == 6 ->
        ((4 * 0.80) + 2) * price
      true ->
        disc_helper(quantidade, 5)
    end
  end

  defp disc_helper(quantidade, valor) do
    cond do 
      valor == 5 ->
        (div(quantidade, 5) * 0.75 * 5) + disc_helper(rem(quantidade,5), valor - 1)
      valor == 4 ->
        (div(quantidade, 4) * 0.80 * 4) + disc_helper(rem(quantidade,4), valor - 1)
      valor == 3 ->
        (div(quantidade, 3) * 0.90 * 3) + disc_helper(rem(quantidade,3), valor - 1)
      valor == 2 ->
        (div(quantidade, 2) * 0.95 * 2) + rem(quantidade, 2)
    end
  end
end
