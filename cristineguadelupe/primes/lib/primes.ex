defmodule Primes do

  def biggest_prime(numbers) do
    numbers
    |> Enum.join()
    |> biggest_prime(Enum.count(numbers))
  end

  def biggest_prime(numbers, acc) do
    numbers
    |> slices(acc)
    |> check(numbers, acc)
  end

  def check(slices, numbers, acc) do
    slices
    |> Enum.filter(&is_prime?/1)
    |> primes(numbers, acc)
  end

  def primes([], numbers, acc) do
    numbers
    |> biggest_prime(acc-1)
  end
  def primes(primes, _numbers, _acc) do
    primes
    |> IO.inspect()
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

  def calc([h| _t], n) when rem(n, h) == 0, do: false
  def calc([_h | t], n), do: calc(t, n)
  def calc([], _n), do: true

  def slices(_, size) when size <= 0, do: []
  def slices(s, size) do
    max = String.length(s)
    for iterate <- 0..max, iterate+size <= max, do: String.slice(s, iterate, size)
  end

end
