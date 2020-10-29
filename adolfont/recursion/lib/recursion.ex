defmodule Recursion do
  @moduledoc """
  Documentation for `Recursion`.
  """
  require Integer

  @doc """
  Hello world.

  ## Examples

      iex> Recursion.hello()
      :world

  """
  def hello do
    hello(:world)
  end

  defp hello(name) do
    name
  end

  def even([]) do
    []
  end

  def even([head | tail]) when Integer.is_even(head) do
    [head | even(tail)]
  end

  def even([_head | tail]) do
    even(tail)
  end

  def odd([]) do
    []
  end

  def odd([head | tail]) when Integer.is_odd(head) do
    [head | odd(tail)]
  end

  def odd([_head | tail]) do
    odd(tail)
  end

  def even_odd_inefficient(list) do
    {even(list), odd(list)}
  end

  def even_odd(list) do
    even_odd(list, [], [])
  end

  def even_odd([head | tail], even, odd) when Integer.is_even(head) do
    even_odd(tail, [head | even], odd)
  end

  def even_odd([head | tail], even, odd) do
    even_odd(tail, even, [head | odd])
  end

  def even_odd([], even, odd) do
    {Enum.reverse(even), Enum.reverse(odd)}
  end
end
