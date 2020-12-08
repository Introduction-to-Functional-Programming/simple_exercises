defmodule Day04 do
  @moduledoc """
  Advent of Code 2020 - Day 4: Passport Processing https://adventofcode.com/2020/day/4
  """

  defp process_input(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n\n")
  end

  defp get_fields(string) do
    string
    |> String.trim()
    |> String.split(["\n", " "])
    |> Enum.map(&String.split(&1, ":"))
  end

  def is_valid?(passport_as_string) do
    passport_fields =
      get_fields(passport_as_string)
      |> Enum.map(fn [field, _value] -> field end)
      |> Enum.sort()
      |> Enum.join("-")

    passport_fields == "byr-cid-ecl-eyr-hcl-hgt-iyr-pid" or
      passport_fields == "byr-ecl-eyr-hcl-hgt-iyr-pid"
  end

  def is_valid_task_2?(passport_as_string) do
    passport_fields =
      get_fields(passport_as_string)
      |> Enum.map(&valid_field/1)
      |> Enum.sort()
      |> Enum.join("-")

    passport_fields == "byr-cid-ecl-eyr-hcl-hgt-iyr-pid" or
      passport_fields == "byr-ecl-eyr-hcl-hgt-iyr-pid"
  end

  defp valid_field([field, value]) do
    cond do
      field == "byr" and String.to_integer(value) >= 1920 and String.to_integer(value) <= 2002 ->
        field

      field == "iyr" and String.to_integer(value) >= 2010 and String.to_integer(value) <= 2020 ->
        field

      field == "eyr" and String.to_integer(value) >= 2020 and String.to_integer(value) <= 2030 ->
        field

      field == "hgt" and is_valid_height?(value) ->
        field

      field == "hcl" and is_valid_hair_color?(value) ->
        field

      field == "ecl" and is_valid_eye_color?(value) ->
        field

      field == "pid" and is_valid_passport_id?(value) ->
        field

      field == "cid" ->
        field

      true ->
        "erro"
    end
  end

  defp is_valid_height?(value) do
    (String.ends_with?(value, "cm") or
       String.ends_with?(value, "in")) and
      is_valid_height_cm_in?(value)
  end

  defp is_valid_height_cm_in?(value) do
    unit = String.slice(value, -2, 2)
    [value_number, _] = String.split(value, ["in", "cm"])
    value_number = String.to_integer(value_number)

    (unit == "cm" and value_number >= 150 and value_number <= 193) or
      (unit == "in" and value_number >= 59 and value_number <= 76)
  end

  defp is_valid_hair_color?(value) do
    String.starts_with?(value, "#") and
      String.length(value) == 7 and
      is_valid_hair_color_aux?(String.slice(value, 1..-1))
  end

  def is_valid_hair_color_aux?(value) do
    value
    |> String.graphemes()
    |> Enum.all?(fn x -> String.match?(x, ~r/["0-9"]|["a-f"]/) == true end)
  end

  defp is_valid_eye_color?(value) do
    value in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  end

  def is_valid_passport_id?(value) do
    String.length(value) == 9 and
      is_valid_passport_id_all_0_to_9_digits?(value)
  end

  defp is_valid_passport_id_all_0_to_9_digits?(value) do
    value
    |> String.graphemes()
    |> Enum.all?(fn x -> String.match?(x, ~r/["0-9"]/) == true end)
  end

  def count_valid_passports(filename) do
    process_input(filename)
    |> Enum.count(fn x -> is_valid?(x) end)
  end

  def count_valid_passports_task_2(filename) do
    process_input(filename)
    |> Enum.count(fn x -> is_valid_task_2?(x) end)
  end
end
