defmodule Primes do
  @max_digits_constant 4

  def biggest_prime(numbers) do
    numbers
    |> Enum.join()
    |> do_biggest_prime(max_digits(numbers))
  end

  def do_biggest_prime(numbers_as_string, max_digits) do
    numbers_as_string
    |> slices(max_digits)
    # |> Enum.sort(fn x, y -> String.to_integer(x) > String.to_integer(y) end)
    |> check(numbers_as_string, max_digits)
  end

  def check(slices, numbers, max_digits) do
    slices
    |> Enum.filter(&is_prime?/1)
    |> primes(numbers, max_digits)
  end

  def primes([], _numbers, 1) do
    :error
  end

  def primes([], numbers, acc) do
    numbers
    |> do_biggest_prime(acc - 1)
  end

  def primes(primes, _numbers, _acc) do
    primes
    |> Enum.map(&String.to_integer/1)
    |> Enum.max()
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
