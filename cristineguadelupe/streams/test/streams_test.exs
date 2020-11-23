defmodule StreamsTest do
  use ExUnit.Case

  test "returns the fibonnaci sum of n" do
    assert Streams.fibonacci_of(0) == 0
    assert Streams.fibonacci_of(1) == 1
    assert Streams.fibonacci_of(2) == 1
    assert Streams.fibonacci_of(5) == 5
    assert Streams.fibonacci_of(10) == 55
    assert Streams.fibonacci_of(20) == 6765
  end

  test "returns the first n fibonnaci numbers" do
    assert Streams.fibonacci_sequence(5) == [0, 1, 1, 2, 3]
    assert Streams.fibonacci_sequence(8) == [0, 1, 1, 2, 3, 5, 8, 13]
    assert Streams.fibonacci_sequence(10) == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
  end

  test "returns the n factorial" do
    assert Streams.factorial_of(0) == 1
    assert Streams.factorial_of(1) == 1
    assert Streams.factorial_of(5) == 120
    assert Streams.factorial_of(6) == 720
    assert Streams.factorial_of(10) == 3_628_800
  end

  test "returns a sequence from 0! to n!" do
    assert Streams.factorial_sequence(0) == [1]
    assert Streams.factorial_sequence(1) == [1, 1]
    assert Streams.factorial_sequence(5) == [1, 1, 2, 6, 24, 120]
    assert Streams.factorial_sequence(6) == [1, 1, 2, 6, 24, 120, 720]
    assert Streams.factorial_sequence(10) == [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]
  end

end
