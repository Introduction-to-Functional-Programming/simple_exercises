defmodule Day06 do
  @moduledoc """
  Documentation for `Day06`.
  """

  def get_groups_for(a_plane) do
    String.split(a_plane, "\n\n", trim: true)
  end

  def get_answers(a_group) do
    String.split(a_group, "\n", trim: true)
  end

  def get_yes_answers(a_plane) do
    a_plane
    |> get_groups_for()
    |> Enum.map(&get_answers/1)
    |> Enum.map(&count_yes_answers/1)
    |> Enum.sum()
  end

  defp count_yes_answers(list_of_answers) do
    list_of_answers
    |> Enum.join()
    |> String.graphemes()
    |> Enum.uniq()
    |> length()
  end

  def get_yes_answers_for_everyone_in_a_group(a_plane) do
    a_plane
    |> get_groups_for()
    |> Enum.map(&get_answers/1)
    |> Enum.map(&count_yes_answers_for_everyone/1)
    |> Enum.sum()
  end

  defp count_yes_answers_for_everyone(list_of_answers) do
    list_of_answers
    |> Enum.map(&create_set/1)
    |> get_intersection
    |> MapSet.to_list()
    |> length()
  end

  defp create_set(set_as_string) do
    set_as_string |> String.graphemes() |> MapSet.new()
  end

  def get_intersection([only_one]) do
    only_one
  end

  def get_intersection(set) do
    do_get_intersection(set, get_union(set))
  end

  defp do_get_intersection([head], result) do
    MapSet.intersection(head, result)
  end

  defp do_get_intersection([head | tail], result) do
    result = MapSet.intersection(head, result)
    do_get_intersection(tail, result)
  end

  def get_union(set) do
    do_get_union(set, MapSet.new())
  end

  def do_get_union([head], result) do
    MapSet.union(head, result)
  end

  def do_get_union([head | tail], result) do
    result = MapSet.union(head, result)
    do_get_union(tail, result)
  end
end
