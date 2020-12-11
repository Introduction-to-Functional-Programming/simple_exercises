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
end
