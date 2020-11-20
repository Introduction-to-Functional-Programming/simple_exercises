defmodule Primes do

  def is_prime?(n) when n in [2, 3], do: true
  def is_prime?(n) do
    2..floor(:math.sqrt(n))
    |> Enum.all?(&(rem(n, &1) != 0))
  end

end
