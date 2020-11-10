defmodule RomanNumerals do
  @doc """
  Convert the number to a roman number.
  """

  @map %{1 => "I", 5 => "V", 10 => "X", 50 => "L", 100 => "C", 500 => "D", 1000 => "M"}
  @spec numeral(pos_integer) :: String.t()
  def numeral(number) when number <= 3 do
    String.duplicate(@map[1], number)
  end

  def numeral(number) when number <= 5 do
    "#{String.duplicate(@map[1], 5 - number)}#{@map[5]}"
  end

  def numeral(number) when number in 6..8 do
    "#{@map[5]}#{String.duplicate(@map[1], number - 5)}"
  end

  def numeral(number) when number <= 10 do
    "#{String.duplicate(@map[1], 10 - number)}#{@map[10]}"
  end

  def numeral(number) when number < 40 do
    tens = div(number, 10)
    String.duplicate(@map[10], tens) <> numeral(number - tens * 10)
  end

  def numeral(number) when number < 50 do
    tens = div(number, 10)

    "#{String.duplicate(@map[10], 5 - tens)}#{@map[50]}" <>
      numeral(number - tens * 10)
  end

  def numeral(number) when number < 90 do
    tens = div(number - 50, 10)

    "#{@map[50]}#{String.duplicate(@map[10], tens)}" <>
      numeral(number - 50 - tens * 10)
  end

  def numeral(number) when number < 100 do
    tens = div(number, 10)

    "#{String.duplicate(@map[10], 10 - tens)}#{@map[100]}" <>
      numeral(number - tens * 10)
  end

  def numeral(number) when number < 400 do
    hundreds = div(number, 100)

    "#{String.duplicate(@map[100], hundreds)}" <>
      numeral(number - hundreds * 100)
  end

  def numeral(number) when number < 500 do
    hundreds = div(number, 100)

    "#{@map[100]}#{@map[500]}" <>
      numeral(number - hundreds * 100)
  end

  def numeral(number) when number < 900 do
    hundreds = div(number, 100)

    "#{@map[500]}#{String.duplicate(@map[100], hundreds - 5)}" <>
      numeral(number - hundreds * 100)
  end

  def numeral(number) when number < 1000 do
    hundreds = div(number, 100)

    "#{@map[100]}#{@map[1000]}" <>
      numeral(number - hundreds * 100)
  end

  def numeral(number)  do
    thousands = div(number, 1000)

    "#{String.duplicate(@map[1000], thousands)}" <>
      numeral(number - thousands * 1000)
  end
end
