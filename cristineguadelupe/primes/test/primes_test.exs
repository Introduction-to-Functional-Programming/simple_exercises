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
end
