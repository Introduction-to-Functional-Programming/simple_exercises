defmodule Day09 do
  def task1(numbers, size) do
    refactored_part1(numbers, size)
  end

  def processString(string) do
    string
    |> String.trim()
    |> String.split()
    |> Enum.map(fn x -> String.to_integer(x) end)
  end

  # Source: https://github.com/LostKobrakai/aoc2020/blob/master/lib/aoc2020/day9.ex
  def part1(input, preamble \\ 25) do
    input
    |> Stream.chunk_every(preamble + 1, 1)
    |> Enum.find_value(fn list ->
      {total, options} = List.pop_at(list, -1)

      combinations =
        for a <- options, b <- options, uniq: true do
          [a, b] |> Enum.sort() |> List.to_tuple()
        end

      valid? = Enum.any?(combinations, fn {a, b} -> a + b == total end)

      unless valid?, do: total
    end)
  end

  def refactored_part1(input, preamble) do
    input
    |> chunk_in_pieces_of_size(preamble + 1)
    |> Enum.find_value(&verify_if_last_number_is_valid/1)
  end

  defp chunk_in_pieces_of_size(list, size) do
    Stream.chunk_every(list, size, 1)
  end

  defp get_last_element_an_tail_of_a_list(list) do
    List.pop_at(list, -1)
  end

  defp get_all_unique_combinations(list) do
    for a <- list, b <- list, uniq: true do
      [a, b] |> Enum.sort() |> List.to_tuple()
    end
  end

  defp verify_if_last_number_is_valid(list) do
    {last_number, options} = get_last_element_an_tail_of_a_list(list)

    combinations = get_all_unique_combinations(options)

    valid? = Enum.any?(combinations, fn {a, b} -> a + b == last_number end)

    if not valid? do
      last_number
    end
  end

  # Task 1 adpated from Martin Gausby's Task 1 in
  # https://github.com/gausby/aoc2020/blob/main/test/aoc2020/day_09_test.exs

  def checksum([_ | remaining] = list, preamble_length) do
    # gets the preamble (a list of numbers)
    # and the first number after the preamble
    {preamble, [number | _]} = Enum.split(list, preamble_length)

    # if do_checksum returns :ok, call checksum recursively
    # with the remaining numbers (all but the first)
    # otherwise, returns an error.
    case do_checksum(preamble, number) do
      :found ->
        checksum(remaining, preamble_length)

      {:error, {:invalid, _n}} = error ->
        error
    end
  end

  defp do_checksum([_], number), do: {:error, {:invalid, number}}

  defp do_checksum([first_element | [_ | _] = rest], number) do
    case find_first_element_with_property_X(rest, number, first_element) do
      :not_found ->
        do_checksum(rest, number)

      :found ->
        :found
    end
  end

  defp find_first_element_with_property_X(rest, number, first_element) do
    do_find_first_element_with_property_X(
      rest,
      fn an_element ->
        checks_if_number_equals_to_sum_of_two_elements_of_preamble(
          number,
          first_element,
          an_element
        )
      end
    )
  end

  defp checks_if_number_equals_to_sum_of_two_elements_of_preamble(number, element1, element2) do
    number == element1 + element2
  end

  defp do_find_first_element_with_property_X([], _function) do
    :not_found
  end

  defp do_find_first_element_with_property_X([head | tail], function) do
    case function.(head) do
      true -> :found
      _ -> do_find_first_element_with_property_X(tail, function)
    end
  end

  # TASK 2
  def find_contiguous_set_which_sums_to(list, preamble_length) do
    scan_for_window(list, task1(list, preamble_length))
  end

  def find_encryption_weakness(list, preamble_length) do
    {:ok, result} = find_contiguous_set_which_sums_to(list, preamble_length)
    Enum.min(result) + Enum.max(result)
  end

  # Two functions below adapted from
  # https://github.com/gausby/aoc2020/blob/main/test/aoc2020/day_09_test.exs
  defp scan_for_window(numbers, target) do
    initial_state = {:queue.new(), 0, target}

    case Enum.reduce_while(numbers, initial_state, &do_scan/2) do
      {:ok, [_ | _]} = success -> success
      {_window, _sum, _target} -> {:error, :not_found}
    end
  end

  defp do_scan(value, {window, sum, target}) do
    case value do
      _target_found when target == value + sum ->
        result = :queue.to_list(:queue.in(value, window))
        {:halt, {:ok, result}}

      _grow_window when target > value + sum ->
        window = :queue.in(value, window)
        {:cont, {window, sum + value, target}}

      _shrink_window_and_retry_value when target < value + sum ->
        {{:value, leaving}, window} = :queue.out(window)
        do_scan(value, {window, sum - leaving, target})
    end
  end
end
