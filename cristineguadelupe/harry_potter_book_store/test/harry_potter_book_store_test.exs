defmodule HarryPotterBookStoreTest do
  use ExUnit.Case
  doctest HarryPotterBookStore

  describe "books_list/1" do
    test "success: get the bought books list" do
      assert HarryPotterBookStore.books_list(%{1 => 2, 2 => 2}) == [1, 2]
      assert HarryPotterBookStore.books_list(%{3 => 1, 4 => 1, 5 => 3}) == [3, 4, 5]
    end
  end

  describe "full_price/2" do
    test "success: get the full price" do
      assert HarryPotterBookStore.full_price(%{1 => 2, 2 => 1}, 8) == 24
      assert HarryPotterBookStore.full_price(%{3 => 1, 4 => 1, 5 => 3}, 8) == 40
    end
  end

  describe "shopping_cart/2" do
    test "success: get the sale price" do
      assert HarryPotterBookStore.total(%{1 => 1, 2 => 1}, 8) == 15.20
    end
    test "success: get the sale price applying all rules without combinatory"  do
      assert HarryPotterBookStore.total(%{1 => 2, 2 => 2, 3 => 2, 4 => 1, 5 => 1 } == 51.60)
    end
  end
end
