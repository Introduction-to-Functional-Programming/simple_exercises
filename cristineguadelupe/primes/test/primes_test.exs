defmodule PrimesTest do
  use ExUnit.Case

  test "Return if a number is prime" do
    assert Primes.is_prime?(2)
    assert Primes.is_prime?(3)
    assert Primes.is_prime?(5)
    assert Primes.is_prime?(59)
    assert Primes.is_prime?(653)
    assert Primes.is_prime?(9323)

    refute Primes.is_prime?(6)
    refute Primes.is_prime?(6598)
    refute Primes.is_prime?(8541)
    refute Primes.is_prime?(85)
  end

  test "Return the biggest prime number from a given list" do
    assert Primes.biggest_prime([4,4,4,4,4]) == :error
    assert Primes.biggest_prime([1,2,2,2,2]) == 2
    assert Primes.biggest_prime([3,1,4,1,5]) == 41
    assert Primes.biggest_prime([5,9,2,6,5]) == 59
    assert Primes.biggest_prime([2,6,5,3,5]) == 653
    assert Primes.biggest_prime([7,9,3,2,3]) == 9323
    assert Primes.biggest_prime([1,4,1,5,9,2,6,5,3,5,8,9,7,9,3,2,3,8,4,6]) == 9323
  end
end
