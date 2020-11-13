notas = [10, 20, 50, 100]
valor_saque = 80
# valor_saque = 250
tmp = Enum.map(notas, fn number -> number > valor_saque end)

cond do
  # SAQUE = 10
  tmp == [false, true, true, true] ->
    notas_saque = [{10, 1}, {20, 0}, {50, 0}, {100, 0}]
    IO.inspect(notas_saque)

  tmp == [false, false, true, true] ->
    # SAQUE = 20
    if Enum.at(notas, 1) == valor_saque do
      notas_saque = [{10, 0}, {20, 1}, {50, 0}, {100, 0}]
      IO.inspect(notas_saque)
    end

    # SAQUE = 3O
    if Enum.at(notas, 1) + 10 == valor_saque do
      notas_saque = [{10, 1}, {20, 1}, {50, 0}, {100, 0}]
      IO.inspect(notas_saque)
    end

    # SAQUE = 40
    if Enum.at(notas, 1) + 20 == valor_saque do
      notas_saque = [{10, 0}, {20, 2}, {50, 0}, {100, 0}]
      IO.inspect(notas_saque)
    end

  tmp == [false, false, false, true] ->
    # SAQUE = 50
    if Enum.at(notas, 2) == valor_saque do
      notas_saque = [{10, 0}, {20, 0}, {50, 1}, {100, 0}]
      IO.inspect(notas_saque)
    end

    # SAQUE = 60
    if Enum.at(notas, 2) + 10 == valor_saque do
      notas_saque = [{10, 1}, {20, 0}, {50, 1}, {100, 0}]
      IO.inspect(notas_saque)
    end

    # SAQUE = 70
    if Enum.at(notas, 2) + 20 == valor_saque do
      notas_saque = [{10, 0}, {20, 1}, {50, 1}, {100, 0}]
      IO.inspect(notas_saque)
    end

    # SAQUE = 80
    if Enum.at(notas, 2) + 30 == valor_saque do
      notas_saque = [{10, 1}, {20, 1}, {50, 1}, {100, 0}]
      IO.inspect(notas_saque)
    end

    # SAQUE = 90
    if Enum.at(notas, 2) + 40 == valor_saque do
      notas_saque = [{10, 0}, {20, 2}, {50, 1}, {100, 0}]
      IO.inspect(notas_saque)
    end

  # SAQUE = 100
  valor_saque == 100 ->
    notas_saque = [{10, 0}, {20, 0}, {50, 0}, {100, 1}]
    IO.inspect(notas_saque)

  # SAQUES ACIMA DE 100 REAIS
  valor_saque > 100 ->
    v1 = div(valor_saque, 100)
    v2 = rem(valor_saque, 100)

    if v2 == 90 do
      notas_saque = [[10, 0], [20, 2], [50, 1], [100, v1]]
      IO.inspect(notas_saque)
    end

    if v2 == 80 do
      notas_saque = [[10, 11], [20, 1], [50, 1], [100, v1]]
      IO.inspect(notas_saque)
    end

    if v2 == 70 do
      notas_saque = [[10, 10], [20, 2], [50, 1], [100, v1]]
      IO.inspect(notas_saque)
    end

    if v2 == 60 do
      notas_saque = [[10, 1], [20, 0], [50, 1], [100, v1]]
      IO.inspect(notas_saque)
    end

    if v2 == 50 do
      notas_saque = [[10, 0], [20, 0], [50, 1], [100, v1]]
      IO.inspect(notas_saque)
    end

    if v2 == 40 do
      notas_saque = [[10, 0], [20, 2], [50, 0], [100, v1]]
      IO.inspect(notas_saque)
    end

    if v2 == 30 do
      notas_saque = [[10, 1], [20, 1], [50, 0], [100, v1]]
      IO.inspect(notas_saque)
    end

    if v2 == 20 do
      notas_saque = [[10, 0], [20, 1], [50, 0], [100, v1]]
      IO.inspect(notas_saque)
    end

    if v2 == 10 do
      notas_saque = [[10, 1], [20, 0], [50, 0], [100, v1]]
      IO.inspect(notas_saque)
    end
end
