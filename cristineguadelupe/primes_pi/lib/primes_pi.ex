defmodule PrimesPi do

  @max_digits_constant 4

  def biggest_sequence(number) do
    number
    |> Enum.join()
    # |> String.slice(2..-1)
    |> all_slices()
    |> all_primes()
  end

  def all_slices(number) do
    number
    |> max_digits()
    |> Stream.unfold(fn
      0 -> nil
      n -> {slices(number, n), n-1}
    end)
    |> Enum.to_list()
    |> List.flatten()
  end

  def all_primes(slices) do
    slices
    |> Enum.filter(&is_prime?/1)
  end

  def all_primes_ordered(number) do
    number
    |> String.graphemes()
    |> Stream.unfold(fn n -> {Enum.take(n, 4), tl(n)} end)
    |> Enum.take(20)
    |> Enum.map(&combinations/1)
    |> List.flatten()
  end

  def combinations(number) do
    number
    |> Enum.reverse()
    |> Stream.unfold(fn n -> {Enum.take(n, length(n)), tl(n)} end)
    |> Enum.take(length(number))
    |> Enum.reverse()
    |> Enum.map(&Enum.reverse/1)
    |> Enum.map(&Enum.join/1)
    |> all_primes()
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

  def slices(_, size) when size <= 0, do: []
  def slices(s, size) do
    max = String.length(s)
    for iterate <- 0..max, iterate + size <= max, do: String.slice(s, iterate, size)
  end

  def max_digits(numbers) when length(numbers) < @max_digits_constant, do: length(numbers)
  def max_digits(_), do: @max_digits_constant

end
