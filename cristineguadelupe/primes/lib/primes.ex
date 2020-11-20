defmodule Primes do

  def biggest_prime(numbers) do
    numbers
    |> biggest_prime([])
  end
  def biggest_prime([], acc) do
    acc
    |> clean()
    |> Stream.filter(&is_prime?/1)
    |> Enum.max()
  end
  def biggest_prime(numbers, acc) do
    acc =
      [ acc | numbers
        |> Stream.unfold(fn [h | t] -> {{h, t}, t} end)
        |> Enum.take(length(numbers))
      ]
      |> List.flatten()

    next = List.delete_at(numbers, -1)
    biggest_prime(next, acc)
  end

  def is_prime?(n) when n in [2, 3], do: true
  def is_prime?(n) when n <= 9973 do
    Enum.to_list(2..floor(:math.sqrt(n)))
    |> calc(n)
  end
  def is_prime?(_n), do: false

  def calc([h| _t], n) when rem(n, h) == 0, do: false
  def calc([_h | t], n), do: calc(t, n)
  def calc([], _n), do: true

  def clean(numbers) do
    numbers
    |> Stream.map(
      &Tuple.to_list(&1)
      |> List.flatten
      |> Enum.join()
      |> String.to_integer()
    )
    |> Enum.uniq()
  end

end
