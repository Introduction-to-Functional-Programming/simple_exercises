

<h1>Recursive Functions</h1>

Retorna o maior número de um list:
```elixir
def biggest_number([number | []]) do
	number
end
def biggest_number([number | tail]) do
	max(number, biggest_number(tail))
end
```
# Tail Call Optimization

Let's take a look at an example of a function that does not allow tail call optimization. This is a function that sums all the numbers in a list.

Here's the same function with tail call optimization.

```elixir
def sum_optimized(list) do
	sum_optimized(list, 0)
end

defp sum_optimized([head | tail], current_sum) do
	sum_optimized(tail, current_sum + head) 
end	
defp sum_optimized([], current_sum), do: current_sum
```
```elixir
def sum_unoptimized([]), do: 0
def sum_unoptimized([head | tail]) do
	head + sum_unoptimized(tail)
end	
```

https://inquisitivedeveloper.com/lwm-elixir-27
