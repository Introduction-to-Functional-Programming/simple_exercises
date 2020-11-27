defmodule PrimesPi do

  def biggest_sequence(number) do
    number
    |> all_primes()
    |> Enum.with_index()
    |> remove_dups([])
    |> clear(all_primes(number))
    |> String.reverse()
    |> String.myers_difference(String.reverse(number))
    |> Keyword.get_values(:eq)
    |> Enum.map(&String.reverse/1)
    |> Enum.reverse()
    |> Enum.max_by(&String.length/1)
  end

  def all_primes(number) do
    number
    |> as_string()
    |> number_to_slices()
    |> Enum.map(&slice_to_combinations/1)
    |> Enum.map(&filter_primes/1)
    |> List.flatten()
  end

  def number_to_slices(number) do
    number
    |> String.graphemes()
    |> Stream.unfold(fn n -> {Enum.take(n, 4), tl(n)} end)
    |> Enum.take(String.length(number))
  end

  def slice_to_combinations(slice) do
    slice
    |> Enum.reverse()
    |> Stream.unfold(fn n -> {Enum.take(n, length(n)), tl(n)} end)
    |> Enum.take(length(slice))
    |> Enum.reverse()
    |> Enum.map(&Enum.reverse/1)
    |> Enum.map(&Enum.join/1)
  end

  def filter_primes(combinations) do
    combinations
    |> Enum.filter(&is_prime?/1)
  end

  def remove_dups([], acc), do: acc |> List.flatten() |> Enum.sort() |> Enum.uniq()
  def remove_dups(list_of_primes, acc) do
    current = hd(list_of_primes)
    primes = all_primes(elem(current, 0))
    index  = primes |> Enum.find_index(&(&1 == elem(current, 0)))
    size = primes |> length()
    list_to_remove = Enum.filter(0..size-1, fn x -> x != index end) |> adjust_indexes(elem(current, 1), index)
    acc = [list_to_remove | acc]
    remove_dups(tl(list_of_primes), acc)
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

  def as_string(number) when is_binary(number), do: number
  def as_string(number), do: Enum.join(number)

  def adjust_indexes(indexes, index, _adjust) when index <= 1, do: indexes
  def adjust_indexes(indexes, index, adjust) do
    indexes
    |> Enum.map(&(&1-adjust))
    |> Enum.map(&(&1+index))
  end


  def clear(indexes, list) do
    cleaned =
      indexes
      |> Enum.reduce(list, &List.replace_at(&2, &1, "0"))
      |> Enum.filter(&(&1 != "0"))

    cleaned
    |> special_case([])
    |> Enum.zip(cleaned)
    |> Keyword.get_values(:false)
    |> Enum.join()
  end

  def special_case([a, b, c | _rest] = list, acc) do
    last = String.graphemes(a) |> List.last()
    current = b
    next = String.graphemes(c) |> List.first()
    acc = [special_case?(last, current, next) | acc]
    special_case(tl(list), acc)
  end
  def special_case(_, acc), do: [false | acc] |> Enum.reverse() |> (&([false | &1])).()

  def special_case?(a, b, c) do
    Enum.join([a, c]) == b
  end

end
