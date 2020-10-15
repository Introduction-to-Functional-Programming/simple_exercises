defmodule HarryPotterBookstore do
  @moduledoc """
  This is the HarryPotterBookstore module.

  It calculates discounts for Harry Potter books as described here:
  https://github.com/xurxodev/HarryPotter-Kata
  """

  @doc """
  Given a shopping cart, returns the list with the codes of the  books
  bought in that shopping cart.
  """
  @spec books_bought(any) :: [any]
  def books_bought(shopping_cart) do
    shopping_cart
    |> Enum.map(fn {livro, _quantidade} -> livro end)
  end

  @doc """
  Given a shopping cart, returns the total price, without any discount,
  of that shopping cart.
  """
  @spec full_price(any, number) :: number
  def full_price(shopping_cart, price) do
    total_books_bought(shopping_cart) * price
  end

  defp total_books_bought(shopping_cart) do
    shopping_cart
    |> Enum.map(fn {_book, amount} -> amount end)
    |> Enum.sum()
  end

  @doc """
  Given a shopping cart,
  returns the price of all books giving a discount when two copies
  of different books are bought.
  """
  @spec discount_two_copies(any, number) :: float
  def discount_two_copies(shopping_cart, price) do
    discount_two_copies(shopping_cart, price, two_copies(shopping_cart))
  end

  defp discount_two_copies(shopping_cart, price, number_two_copies) do
    (total_books_bought(shopping_cart) - number_two_copies) * price +
      number_two_copies * price * 0.95
  end

  @doc """
  Given a shopping cart,
  returns how many copies of two different books were bought.
  """
  def two_copies(shopping_cart) do
    two_copies(shopping_cart, 0)
  end

  defp two_copies(shopping_cart, total) do
    shopping_cart
    |> Enum.filter(fn {_book, units} -> units > 0 end)
    |> two_copies_remove(total)
  end

  defp two_copies_remove([{first, units_first}, {second, units_second} | rest], total) do
    two_copies(
      [
        {first, units_first - 1},
        {second, units_second - 1} | rest
      ],
      total + 2
    )
  end

  defp two_copies_remove(_shopping_cart, total) do
    total
  end
end
