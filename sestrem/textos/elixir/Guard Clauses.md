
A guard clause places further restrictions on the parameters in a function, such as the data type or allowed number range.

A guard clause begins with the `when` keyword and is placed between the function head and the `do` keyword.

```elixir
def divide(number1, number2) when number2 != 0 do
	number 1 / number 2
end
```
# Examples
```elixir
def min(num1, num2) when num1 <= num2 do
	num1
end
def min(num1, num2) when num1 > num2 do
	num2
end
```
```elixir
def get_type(data) when is_integer(data) do
	:integer
end
def get_type(data) when is_atom(data) do
	:atom
end
def get_type(data) when is_function(data) do
	:function
end
def get_type(_) do
	:other_type
end
```

https://inquisitivedeveloper.com/lwm-elixir-24
