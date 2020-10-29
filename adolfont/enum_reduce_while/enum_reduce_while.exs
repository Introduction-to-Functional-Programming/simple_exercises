# [{1, true}, {2, true}, {3, true}, {4, false}, {5, false} ]
# |> Enum.reduce(0, fn {x, boolean}, acc ->
#             if boolean, do: x + acc, else: acc
#     end)
# |> IO.inspect()

# [{1, true}, {2, true}, {3, true}, {4, false}, {5, false} ]
# |> Enum.reduce_while(0, fn {x, boolean}, acc ->
#             if boolean, do: {:cont, x + acc}, else: {:halt, acc}
#     end)
# |> IO.inspect()

x = 10000000
y = 300
lista = for i <- 1..x, do: {i, i<y}

lista
|> Enum.reduce_while(1, fn {x, boolean}, acc ->
  if boolean, do: {:cont, x * acc}, else: {:halt, acc}
end)
|> IO.inspect()

lista
|> Enum.reduce(1, fn {x, boolean}, acc ->
  if boolean, do: x * acc, else: acc
end)
|> IO.inspect()
