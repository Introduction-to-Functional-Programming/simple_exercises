defmodule PrimesPiTest do
  use ExUnit.Case

  test "all ordered primes - return all primes in the right order" do
    challenge = [1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6]
    assert PrimesPi.all_primes(challenge) == ["41", "4159", "5", "59", "2", "653", "5", "53", "3", "5", "5897", "89", "97",
    "7", "79", "9323", "3", "2", "23", "3"]

    assert PrimesPi.all_primes("2220123") == ["2", "2", "2", "2", "23", "3"]
    assert PrimesPi.all_primes("44444253125478000") == ["4253", "2", "2531", "5", "53", "3", "31", "2", "5", "547", "47", "7"]
  end

  test "biggest sequence - return the biggest sequence of primes" do
    challenge = "14159265358979323846"
    assert PrimesPi.biggest_sequence(challenge) == "4159265358979323"

    assert PrimesPi.biggest_sequence("2220123") == "222"
    assert PrimesPi.biggest_sequence("44444253125478000") == "4253"
  end

end
