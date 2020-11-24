defmodule Streams do
  def fibonacci_of(n) do
    fibonacci_generator()
    |> Enum.at(n)
  end

  def fibonacci_sequence(n) do
    fibonacci_generator()
    |> Enum.take(n)
  end

  defp fibonacci_generator() do
    Stream.unfold({0, 1}, fn {last, next} -> {last, {next, last + next}} end)
  end


  def factorial_of(n) do
    factorial_generator()
    |> Enum.at(n)
  end

  def factorial_sequence(n) do
    factorial_generator()
    |> Enum.take(n+1)
  end

  defp factorial_generator() do
    Stream.unfold({1, 1}, fn {last, next} -> {last, {next * last, next + 1}} end)
  end

  def tribonacci_of(n) do
    tribonacci_sequence(n)
    |> List.last()
  end

  def tribonacci_sequence(n) do
    tribonacci_generator()
    |> Enum.take(n)
  end

  def tribonacci_generator() do
    Stream.unfold({0, 1, 1}, fn {a, b, c} -> {a, {b, c, a+b+c}} end)
  end

end
