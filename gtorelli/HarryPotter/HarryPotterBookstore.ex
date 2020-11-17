

defmodule HarryPotterBookstore do
  def books_bought(shopping_cart) do
    Enum.map(shopping_cart, fn {livro, _quantidade} -> livro end)
  end

  def full_price(shopping_cart, price) do
    (Enum.map(shopping_cart, fn {_livro, quantidade} -> quantidade end) |> Enum.sum()) * price
  end

  def discount_5(shopping_cart, price) do #2 different books
    full_price(shopping_cart, price) * 0.95
  end

  def discount_10(shopping_cart, price) do #3 different books
    full_price(shopping_cart, price) * 0.90
  end

  def discount_20(shopping_cart, price) do #4 different books
    full_price(shopping_cart, price) * 0.80
  end

  def discount_25(shopping_cart, price) do #5 different books
    full_price(shopping_cart, price) * 0.75
  end

  def cart_total(shopping_cart, price) do
    # each book as a value:
    book_n = (Enum.map(shopping_cart, fn {_livro, quantidade} -> quantidade end) |> Enum.sum())
    # each tuple as book title:
    # book_n = Enum.count(shopping_cart)
    cond do
      book_n == 2 ->
        discount_5(shopping_cart, price)
      book_n == 3 ->
        discount_10(shopping_cart, price)
      book_n == 4 ->
        discount_20(shopping_cart, price)
      book_n == 5 ->
        discount_25(shopping_cart, price)
    end
  end
end

