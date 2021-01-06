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
end
