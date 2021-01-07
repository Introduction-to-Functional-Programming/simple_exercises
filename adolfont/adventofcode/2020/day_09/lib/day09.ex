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

  # Task 1 from Martin Gausby
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

  # if the number is equal to the sum
  # of two numbers from the preamble (a list of numbers)
  # returns :ok
  # otherwise, retuns an error
  defp do_checksum([_], number), do: {:error, {:invalid, number}}

  defp do_checksum([first_element | [_ | _] = rest], number) do
    # great pattern matching above!

    # this is a bit strange because it is a kind of double negation

    #  let rest = [r1, r2, r3, ..., rn]
    #  The result of the Enum.drop_while/2 below is
    # [r1, r2, ..., ri]
    # such that
    # for all j<=i
    # rj + first_element != number
    # So, an empty list returned means that
    # there are no element of rest such that number is equal

    # while the sum of the first element with another element is DIFFERENT
    # from the number that we want to be the sum of two numbers from
    # the preamble,

    case find_first_element_with_property_X(rest, number, first_element) do
      :not_found ->
        do_checksum(rest, number)

      :found ->
        :found
    end
  end

  def find_first_element_with_property_X(rest, number, first_element) do
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

  # Enum.drop_while "drops elements at the beginning of the"
  # "enumerable while fun returns a truthy value."
  # Enum.drop_while([1, 2, 3, 2, 1], fn x -> x < 4 end)
  # []

  # do_find_first_element_with_property_X/2 started as
  # a reimplementation of Enum.drop_while/2
  # returning different values
  # but now it is different as it needs a function with a different polarity

  defp do_find_first_element_with_property_X([], _function) do
    :not_found
  end

  defp do_find_first_element_with_property_X([head | tail], function) do
    case function.(head) do
      true -> :found
      _ -> do_find_first_element_with_property_X(tail, function)
    end
  end
end
