defmodule PrimesPiTest do
  use ExUnit.Case

  test "all primes - returns all primes " do
    list1 = [7, 9, 3, 2, 3] |> prepare_slices()
    list2  = [5, 9, 2, 6, 5] |> prepare_slices()
    challenge = [1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6] |> prepare_slices()

    assert PrimesPi.all_primes(list1) == ["9323", "79", "23", "7", "3", "2", "3"]
    assert PrimesPi.all_primes(list2) == ["59", "5", "2", "5"]
    assert PrimesPi.all_primes(challenge) == ["4159", "5897", "9323", "653", "41", "59", "53", "89", "97", "79", "23", "5", "2", "5", "3", "5", "7", "3", "2", "3"]

  end

  def prepare_slices(list_of_digitis) do
    list_of_digitis
    |> Enum.join()
    |> PrimesPi.all_slices()
  end

end
