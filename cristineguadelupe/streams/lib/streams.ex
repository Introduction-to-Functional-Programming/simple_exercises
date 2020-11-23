defmodule Streams do

  def fibonacci() do
    Stream.unfold({0, 1}, fn {last, next} -> {last, {next, last + next}} end)
  end
  def fibonacci_of(n) do
    fibonacci()
    |> Enum.at(n)
  end
  def fibonacci_sequence(n) do
    fibonacci()
    |> Enum.take(n)
  end

  def factorial(0), do: 1
  def factorial(n) do
    Stream.unfold({1, 2}, fn {last, next} -> {last, {next * last, next + 1}} end)
    |> Enum.take(n)
    |> List.last()
  end

end
