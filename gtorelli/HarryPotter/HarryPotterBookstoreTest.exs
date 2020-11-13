

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

  test "5% discount on 2 different books" do
    shopping_cart = [{1, 1}, {2, 1}]
    assert HarryPotterBookstore.discount_5(shopping_cart, 8) == 15.20
  end

  test "10% discount on 3 different books" do
    shopping_cart = [{1, 1}, {2, 1}, {3, 1}]
    assert HarryPotterBookstore.discount_10(shopping_cart, 8) == 21.6
  end

  test "20% discount on 4 different books" do
    shopping_cart = [{1, 1}, {2, 1}, {3, 1}, {4, 1}]
    assert HarryPotterBookstore.discount_20(shopping_cart, 8) == 25.6
  end

  test "25% discount on 5 different books" do
    shopping_cart = [{1, 1}, {2, 1}, {3, 1}, {4, 1}, {5, 1}]
    assert HarryPotterBookstore.discount_25(shopping_cart, 8) == 30
  end

  test "TOTAL PARA 2 LIVROS DIFERENTES" do
    shopping_cart = [{1, 1}, {2, 1}]
    assert HarryPotterBookstore.cart_total(shopping_cart, 8) == 15.20
  end

end

