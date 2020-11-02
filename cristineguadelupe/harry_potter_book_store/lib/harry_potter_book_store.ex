defmodule HarryPotterBookStore do
  @price 8
  @discount %{1 => 1, 2 => 0.95, 3 => 0.9, 4 => 0.8, 5 => 0.75}

  @spec books_list(map) :: [any]
  def books_list(books \\ %{}) do
    books
    |> Map.keys()
  end

  @spec full_price(map, number) :: number
  def full_price(books, price) do
    books
    |> Map.values()
    |> Enum.map(&(&1 * price))
    |> Enum.sum()
  end

  @spec total(map, number) :: number
  def total(books, price \\ @price) do
    %{books: books, total: 0}
    |> discount(price)
  end

  @spec discount(map, number) :: number
  def discount(cart, price) when map_size(cart.books) > 0 do
    apply_discount(cart, price, @discount[map_size(cart.books)])
  end
  def discount(cart, _price), do: cart.total

  @spec apply_discount(%{books: map, total: number}, number, number) :: any
  def apply_discount(cart, price, discount) do
    new_total = cart.total + (map_size(cart.books) * price * discount)
    books =
      cart.books
      |> Enum.map(fn {k, v} -> {k, v-1} end)
      |> Enum.filter(fn {_k, v} -> v > 0 end)
      |> Enum.into(%{})

    %{cart | total: new_total, books: books}
    |> discount(price)
  end
end
