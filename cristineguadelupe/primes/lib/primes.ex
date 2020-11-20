defmodule Primes do

  def biggest_prime(numbers) do
    numbers
    |> biggest_prime([])
  end

  def biggest_prime([], acc) do
    acc
    |> List.flatten()
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.filter(&check/1)
    |> Enum.map(&List.flatten/1)
    |> Enum.map(&Enum.join/1)
    |> Enum.map(&String.to_integer/1)
    |> Enum.max()
  end
  def biggest_prime(numbers, acc) do
    acc =
      [ acc | numbers
        |> Stream.unfold(fn [h | t] -> {{h, t}, t} end)
        |> Enum.take(length(numbers))
      ]
    next = List.delete_at(numbers, -1)
    biggest_prime(next, acc)
  end

  def check(number) do
    number
    |> List.flatten()
    |> Enum.join()
    |> String.to_integer()
    |> is_prime?()
  end

  def is_prime?(n) when n in [2, 3], do: true
  def is_prime?(n) do
    2..floor(:math.sqrt(n))
    |> Enum.all?(&(rem(n, &1) != 0))
  end

end
