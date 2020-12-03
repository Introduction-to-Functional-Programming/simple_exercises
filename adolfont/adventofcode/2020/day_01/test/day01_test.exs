defmodule Day01Test do
  use ExUnit.Case
  doctest Day01

  test "Encontra dois valores em uma lista cuja soma é 2020 e os multiplica" do
    {:ok, numeros} = File.read("./test/entrada.txt")

    lista_numeros = numeros |> String.split("\n", trim: true)

    assert Day01.encontra_dois_valores_cuja_soma_eh_2020(lista_numeros) == 719_796
  end

  test "Encontra três valores em uma lista cuja soma é 2020 e os multiplica" do
    {:ok, numeros} = File.read("./test/entrada.txt")

    lista_numeros = numeros |> String.split("\n", trim: true)

    assert Day01.encontra_tres_valores_cuja_soma_eh_2020(lista_numeros) == 144_554_112
  end
end
