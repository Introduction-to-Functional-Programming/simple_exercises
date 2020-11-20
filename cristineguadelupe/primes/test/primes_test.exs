defmodule PrimesTest do
  use ExUnit.Case
  doctest Primes

  test "greets the world" do
    assert Primes.hello() == :world
  end
end
