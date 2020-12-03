defmodule Day01 do
  @moduledoc """
  Documentation for day 1 of Advent of Code 2020 https://adventofcode.com/2020/day/1
  """
  def encontra_dois_valores_cuja_soma_eh_2020(lista_strings) do
    lista_strings
    |> converte_em_lista_numeros()
    |> encontra_dois_valores_cuja_soma_eh(2020)
  end

  defp encontra_dois_valores_cuja_soma_eh(lista_numeros, valor_soma) do
    [primeiro, segundo] = do_encontra_dois_valores_cuja_soma_eh(lista_numeros, valor_soma)
    primeiro * segundo
  end

  defp do_encontra_dois_valores_cuja_soma_eh(lista_numeros, valor_soma) do
    for x <- lista_numeros, y <- lista_numeros do
      if x + y == valor_soma do
        [x, y]
      end
    end
    |> Enum.reject(&is_nil/1)
    |> hd()
  end

  defp converte_em_lista_numeros(lista_strings) do
    lista_strings
    |> Enum.map(&String.to_integer/1)
  end

  def encontra_tres_valores_cuja_soma_eh_2020(lista_strings) do
    lista_strings
    |> converte_em_lista_numeros()
    |> encontra_tres_valores_cuja_soma_eh(2020)
  end

  defp encontra_tres_valores_cuja_soma_eh(lista_numeros, valor_soma) do
    [primeiro, segundo, terceiro] =
      do_encontra_tres_valores_cuja_soma_eh(lista_numeros, valor_soma)

    primeiro * segundo * terceiro
  end

  defp do_encontra_tres_valores_cuja_soma_eh(lista_numeros, valor_soma) do
    for x <- lista_numeros, y <- lista_numeros, z <- lista_numeros do
      if x + y + z == valor_soma do
        [x, y, z]
      end
    end
    |> Enum.reject(&is_nil/1)
    |> hd()
  end
end
