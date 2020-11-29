defmodule PrimesPi do

  def biggest_sequence(number) when is_list(number) do
    number
    |> as_string()
    |> biggest_sequence()
  end

  def biggest_sequence(number) do
    number
    |> number_to_slices()
    |> get_all_primes()
    |> find_duplicated_primes()
    |> remove_duplicated_primes()
    |> remove_special_cases()
    |> find_diffs(number)
    |> find_biggest_sequence()
  end

  def number_to_slices(number) do
    number
    |> String.graphemes()
    |> Stream.unfold(fn n -> {Enum.take(n, 4), tl(n)} end)
    |> Enum.take(String.length(number))
  end

  def get_all_primes(slices) do
    slices
    |> Enum.map(&slice_to_combinations/1)
    |> Enum.map(&filter_primes/1)
    |> List.flatten()
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

  def find_duplicated_primes(original_primes) do
    original_primes
    |> Enum.with_index()
    |> find_duplicated_primes(original_primes, [])
  end
  def find_duplicated_primes([], original_primes, acc) do
    acc
    |> List.flatten()
    |> Enum.sort()
    |> Enum.uniq()
    |> (&{&1, original_primes}).()
  end
  def find_duplicated_primes(list_of_primes, original_primes, acc) do
    current = hd(list_of_primes)
    primes = all_primes(elem(current, 0))
    index  = primes |> Enum.find_index(&(&1 == elem(current, 0)))
    size = primes |> length()
    list_to_remove = Enum.filter(0..size-1, fn x -> x != index end) |> adjust_indexes(elem(current, 1), index)
    acc = [list_to_remove | acc]
    find_duplicated_primes(tl(list_of_primes), original_primes, acc)
  end

  def remove_duplicated_primes({indexes, list}) do
    indexes
    |> Enum.reduce(list, &List.replace_at(&2, &1, "0"))
    |> Enum.filter(&(&1 != "0"))
  end

  def remove_special_cases(cleaned) do
    cleaned
    |> special_case([])
    |> Enum.zip(cleaned)
    |> Keyword.get_values(:false)
    |> Enum.join()
  end

  def find_diffs(primes, number) do
    primes
    |> String.reverse()
    |> String.myers_difference(String.reverse(number))
    |> Keyword.get_values(:eq)
  end

  def find_biggest_sequence(diffs) do
    diffs
    |> Enum.map(&String.reverse/1)
    |> Enum.reverse()
    |> Enum.max_by(&String.length/1)
  end

  defp is_prime?(n) when is_binary(n), do: is_prime?(String.to_integer(n))
  defp is_prime?(n) when n in [2, 3], do: true
  defp is_prime?(n) when n <= 9973 do
    Enum.to_list(2..floor(:math.sqrt(n)))
    |> calc(n)
  end
  defp is_prime?(_n), do: false

  defp calc([h | _t], n) when rem(n, h) == 0, do: false
  defp calc([_h | t], n), do: calc(t, n)
  defp calc([], _n), do: true

  defp as_string(number) when is_binary(number), do: number
  defp as_string(number), do: Enum.join(number)

  defp adjust_indexes(indexes, index, _adjust) when index <= 1, do: indexes
  defp adjust_indexes(indexes, index, adjust) do
    indexes
    |> Enum.map(&(&1-adjust))
    |> Enum.map(&(&1+index))
  end

  defp special_case([a, b, c | _rest] = list, acc) do
    last = String.graphemes(a) |> List.last()
    current = b
    next = String.graphemes(c) |> List.first()
    acc = [special_case?(last, current, next) | acc]
    special_case(tl(list), acc)
  end
  defp special_case(_, acc), do: [false | acc] |> Enum.reverse() |> (&([false | &1])).()

  defp special_case?(a, b, c) do
    Enum.join([a, c]) == b
  end


  # For test purposes only
  def all_primes(number) do
    number
    |> as_string()
    |> number_to_slices()
    |> get_all_primes()
  end

end
