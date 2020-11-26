defmodule PrimesPiTest do
  use ExUnit.Case

  test "all ordered primes - return all primes in the right order" do
    challenge = [1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6] |> prepare_ordered_slices()
    assert PrimesPi.all_primes(challenge) == ["41", "4159", "5", "59", "2", "653", "5", "53", "3", "5", "5897", "89", "97",
    "7", "79", "9323", "3", "2", "23", "3"]
  end

  def prepare_ordered_slices(list_of_digitis) do
    list_of_digitis
    |> Enum.join()
    |> PrimesPi.prepare_combinations()
  end

end
