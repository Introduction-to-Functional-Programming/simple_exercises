defmodule HarryPotterBookstoreTest do
  use ExUnit.Case

  test "obtains the list of books bought" do
    shopping_cart = [{1, 1}]
    assert HarryPotterBookstore.books_bought(shopping_cart) == [1]

    assert HarryPotterBookstore.books_bought([{1, 1}, {7, 3}, {5, 5}, {2, 4}]) ==
             [1, 7, 5, 2]
  end

  test "calculate the full price of a shopping cart" do
    shopping_cart = [{1, 1}]
    assert HarryPotterBookstore.full_price(shopping_cart, 8) == 8

    shopping_cart = [{1, 2}]
    assert HarryPotterBookstore.full_price(shopping_cart, 8) == 16

    shopping_cart = [{1, 2}, {2, 3}]
    assert HarryPotterBookstore.full_price(shopping_cart, 8) == 40
  end
end
