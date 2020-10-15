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
  Given a shopping cart,n
  returns how many copies of two different books were bought.
  """
  def two_copies(shopping_cart) do
    two_copies(shopping_cart, 0)
  end

  defp two_copies(shopping_cart, total) do
    shopping_cart
    |> Enum.sort_by(fn {_, units} -> units end, :desc)
    |> Enum.filter(fn {_book, units} -> units > 0 end)
    |> two_copies_remove(total, 2)
  end

  defp two_copies_remove(shopping_cart, total, number_items) when length(shopping_cart) >= 2 do
    {first_n_elements, rest} = Enum.split(shopping_cart, number_items)

    two_copies(
      subtract_quantity_by_one(first_n_elements) ++ rest,
      total + number_items
    )
  end

  defp two_copies_remove(_shopping_cart, total, _) do
    total
  end

  defp subtract_quantity_by_one(shopping_cart_items) do
    shopping_cart_items
    |> Enum.map(fn {book, quantity} -> {book, quantity - 1} end)
  end
end
