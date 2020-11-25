arquivo = IO.gets("Nome do arquivo: ") |> String.trim()

words = File.read!(arquivo)
 |> String.split(~r{(\\n|[^\w'])+})
 |> Enum.filter(fn x -> x != "" end)
IO.inspect(words)

words |> Enum.count() |> IO.puts()

