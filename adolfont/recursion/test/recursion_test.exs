defmodule RecursionTest do
  use ExUnit.Case
  doctest Recursion

  test "greets the world" do
    assert Recursion.hello() == :world
  end

  test "Factorial of 1 is 1" do
    assert :recursion.fac(1) == 1
    assert :recursion.fac2(1) == 1
  end

  test "Factorial of 2 is 2" do
    assert :recursion.fac(2) == 2
    assert :recursion.fac2(2) == 2
  end

  test "Factorial of 3 is 6" do
    assert :recursion.fac(3) == 6
    assert :recursion.fac2(3) == 6
  end

  test "Factorial of 10 is 3_628_800" do
    assert :recursion.fac(10) == 3_628_800
    assert :recursion.fac2(10) == 3_628_800
  end

  test "Fibonacci of 1 is 1" do
    assert :recursion.fib(1) == 1
  end

  test "Fibonacci of 2 is 1" do
    assert :recursion.fib(2) == 1
  end

  test "Fibonacci of 3 is 2" do
    assert :recursion.fib(3) == 2
  end

  test "Fibonacci of 10 is 55" do
    assert :recursion.fib(10) == 55
    assert :recursion.fib2(10) == 55
  end

  test "The even numbers on [1, 2, 6, 8, 7] are [2, 6, 8]" do
    assert Recursion.even([1, 2, 6, 8, 7]) == [2, 6, 8]
  end

  test "The odd numbers on [1, 2, 6, 8, 7] are [1, 7]" do
    assert Recursion.odd([1, 2, 6, 8, 7]) == [1, 7]
  end

  test "The even and odd numbers on [1, 2, 6, 8, 7] are {[2, 6, 8], [1, 7]}" do
    assert Recursion.even_odd([1, 2, 6, 8, 7]) == {[2, 6, 8], [1, 7]}
  end
end
