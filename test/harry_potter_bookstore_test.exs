defmodule HarryPotterBookstoreTest do
  use ExUnit.Case

  test "obtains the list of books bought" do
    shopping_cart = [{1, 1}]
    assert HarryPotterBookstore.books_bought(shopping_cart) == [1]

    assert HarryPotterBookstore.books_bought([{1, 1}, {7, 3}, {5, 5}, {2, 4}]) ==
             [1, 7, 5, 2]
  end
end
