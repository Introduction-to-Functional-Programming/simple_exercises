defmodule PrimesPi do

  def biggest_sequence(number) do
    number
    |> Enum.join()
    # |> String.slice(2..-1)
  end

  def prepare_combinations(number) do
    number
    |> String.graphemes()
    |> Stream.unfold(fn n -> {Enum.take(n, 4), tl(n)} end)
    |> Enum.take(20)
    |> Enum.map(&generate_combinations/1)
    |> List.flatten()
  end

  def generate_combinations(number) do
    number
    |> Enum.reverse()
    |> Stream.unfold(fn n -> {Enum.take(n, length(n)), tl(n)} end)
    |> Enum.take(length(number))
    |> Enum.reverse()
    |> Enum.map(&Enum.reverse/1)
    |> Enum.map(&Enum.join/1)
    |> all_primes()
  end

  def all_primes(slices) do
    slices
    |> Enum.filter(&is_prime?/1)
  end

  def is_prime?(n) when is_binary(n), do: is_prime?(String.to_integer(n))
  def is_prime?(n) when n in [2, 3], do: true
  def is_prime?(n) when n <= 9973 do
    Enum.to_list(2..floor(:math.sqrt(n)))
    |> calc(n)
  end
  def is_prime?(_n), do: false

  def calc([h | _t], n) when rem(n, h) == 0, do: false
  def calc([_h | t], n), do: calc(t, n)
  def calc([], _n), do: true

end
