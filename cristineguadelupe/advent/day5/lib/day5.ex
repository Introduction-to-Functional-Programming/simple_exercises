defmodule Day5 do

  @letters ["F", "B", "L", "R"]
  @to_binary %{"F" => "0", "B" => "1", "L" => "0", "R" => "1"}

  def find_max_seat(file) do
    file
    |> all_seats()
    |> Enum.max()
  end

  def find_my_seat(file) do
    file
    |> all_seats()
    |> my_seat()
  end

  def all_seats(file) do
    File.read!(file)
    |> String.split("\n", trim: true)
    |> Enum.map(&seat_id/1)
  end

  def my_seat(seats) do
    seats
    |> (&Enum.min(&1)..Enum.max(&1)).()
    |> Enum.filter(&(&1 not in seats))
    |> hd()
  end

  def seat_id(string) do
    {row, column} = String.split_at(string, 7)
    to_binary(row) * 8 + to_binary(column)
  end

  def to_binary(string) do
    string
    |> String.replace(@letters, &@to_binary[&1])
    |> String.to_integer(2)
  end

end
