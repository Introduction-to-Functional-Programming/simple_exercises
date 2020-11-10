defmodule HarryPotterBookstoreTest do
  use ExUnit.Case

#   def testBasics
#   assert_equal(0, price([]))
#   assert_equal(8, price([1]))
#   assert_equal(8, price([2]))
#   assert_equal(8, price([3]))
#   assert_equal(8, price([4]))
#   assert_equal(8 * 3, price([1, 1, 1]))
# end




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

    shopping_cart = [{1, 2}, {1, 3}]
    assert HarryPotterBookstore.full_price(shopping_cart, 8) == 40
  end


  test "second task: apply first rule" do
    #"If, you buy two different books, you get a 5% discount on those two books."

    shopping_cart = [{1, 2}, {2, 3}]
    assert HarryPotterBookstore.full_price_second_task(shopping_cart, 8) == 38
  end

  test "third task: apply second rule" do
    #"If you buy 3 different books, you get a 10% discount."

    shopping_cart = [{1, 2}, {2, 3}, {3,1}]
    # books 1, 2, 3 (shopping cart has 3 items)
    # (2 + 3 + 1) = (6 books * $8) = 48 * 0.9 = 43.2
    assert HarryPotterBookstore.full_price_third_task(shopping_cart, 8) == 43.2

    shopping_cart = [{1, 2}, {2, 3}, {3,1}, {3,1}]
    # books 1, 2, 3, 3 (shopping cart has 4 items, 3 different books)
    # (2 + 3 + 1 + 1) = (7 books * $8) = 56 * 0.9 = 43.2
    assert HarryPotterBookstore.full_price_third_task(shopping_cart, 8) == 50.4
  end

  test "fourth task: apply third rule" do
    #"If you buy 4 different books, you get a 20% discount."

    shopping_cart = [{1, 2}, {2, 3}, {3,1}, {4,5}]
    # books 1, 2, 3, 4 (shopping cart has 4 items)
    # (2 + 3 + 1 + 5) = (11 books * $8) = 88 * 0.8 = 76.8
    assert HarryPotterBookstore.full_price_fourth_task(shopping_cart, 8) == 70.4

    shopping_cart = [{1, 2}, {2, 3}, {3,1}, {4,5}, {4,5}]
    # books 1, 2, 3, 4 (shopping cart has 5 items, 4 different books)
    # (2 + 3 + 1 + 5 + 5) = (16 books * $8) = 128 * 0.8 = 102.4
    assert HarryPotterBookstore.full_price_fourth_task(shopping_cart, 8) == 102.4
  end

  test "fifth task: apply fourth rule" do
    #"If you buy 5 different books, you get a 25% discount."

    shopping_cart = [{1, 2}, {2, 3}, {3,1}, {4,5}, {5,1}]
    # books 1, 2, 3, 4, 5 (shopping cart has 5 items)
    # (2 + 3 + 1 + 5 + 1) = (12 books * $8) = 96 * 0.75 = 72
    assert HarryPotterBookstore.full_price_fifth_task(shopping_cart, 8) == 72

    shopping_cart = [{1, 2}, {2, 3}, {3,1}, {4,5}, {5,1}, {5,1}]
    # books 1, 2, 3, 4, 5, 5 (shopping cart has 6 items, 5 different books)
    # (2 + 3 + 1 + 5 + 1 + 1) = (13 books * $8) = 104 * 0.75 = 78
    assert HarryPotterBookstore.full_price_fifth_task(shopping_cart, 8) == 78
  end

  test "sixth task: apply all rules" do
    # execute all the tasks again, but this time in the full_price_all_rules()

    shopping_cart = [{1, 1}]
    assert HarryPotterBookstore.full_price_all_rules(shopping_cart, 8) == 8

    shopping_cart = [{1, 2}]
    assert HarryPotterBookstore.full_price_all_rules(shopping_cart, 8) == 16

    shopping_cart = [{1, 2}, {1, 3}]
    assert HarryPotterBookstore.full_price_all_rules(shopping_cart, 8) == 40

    # 2 books: 5% discount
    shopping_cart = [{1, 2}, {2, 3}]
    assert HarryPotterBookstore.full_price_all_rules(shopping_cart, 8) == 38

    # 3 books: 10% discount
    shopping_cart = [{1, 2}, {2, 3}, {3,1}]
    # books 1, 2, 3 (shopping cart has 3 items)
    # (2 + 3 + 1) = (6 books * $8) = 48 * 0.9 = 43.2
    assert HarryPotterBookstore.full_price_all_rules(shopping_cart, 8) == 43.2

    shopping_cart = [{1, 2}, {2, 3}, {3,1}, {3,1}]
    # books 1, 2, 3, 3 (shopping cart has 4 items, 3 different books)
    # (2 + 3 + 1 + 1) = (7 books * $8) = 56 * 0.9 = 43.2
    assert HarryPotterBookstore.full_price_all_rules(shopping_cart, 8) == 50.4

    # 4 books: 20% discount
    shopping_cart = [{1, 2}, {2, 3}, {3,1}, {4,5}]
    # books 1, 2, 3, 4 (shopping cart has 4 items)
    # (2 + 3 + 1 + 5) = (11 books * $8) = 88 * 0.8 = 76.8
    assert HarryPotterBookstore.full_price_all_rules(shopping_cart, 8) == 70.4

    shopping_cart = [{1, 2}, {2, 3}, {3,1}, {4,5}, {4,5}]
    # books 1, 2, 3, 4 (shopping cart has 5 items, 4 different books)
    # (2 + 3 + 1 + 5 + 5) = (16 books * $8) = 128 * 0.8 = 102.4
    assert HarryPotterBookstore.full_price_all_rules(shopping_cart, 8) == 102.4

    # 5 books: 25% discount
    shopping_cart = [{1, 2}, {2, 3}, {3,1}, {4,5}, {5,1}]
    # books 1, 2, 3, 4, 5 (shopping cart has 5 items)
    # (2 + 3 + 1 + 5 + 1) = (12 books * $8) = 96 * 0.75 = 72
    assert HarryPotterBookstore.full_price_all_rules(shopping_cart, 8) == 72

    shopping_cart = [{1, 2}, {2, 3}, {3,1}, {4,5}, {5,1}, {5,1}]
    # books 1, 2, 3, 4, 5, 5 (shopping cart has 6 items, 5 different books)
    # (2 + 3 + 1 + 5 + 1 + 1) = (13 books * $8) = 104 * 0.75 = 78
    assert HarryPotterBookstore.full_price_all_rules(shopping_cart, 8) == 78

  end
end
