
<h1>Default Values and Multiple Function Clauses</h1>

```elixir
def do_something(param1, param2 \\ 3)

def do_something(param1, param2) when is_integer(param1) do
	{param1 * 10, param2}
end

def do_something(param1, param2) when is_float(param1) do
	{param1 * 100, param2}
end
```

In order to specify a default value for these functions, we don't put the default value in both function clauses, but rather we create a new function clause that just has the default values and no function body. These default values will then apply to the other function clauses of the same arity. Elixir only wants one set of default values per function.

https://inquisitivedeveloper.com/lwm-elixir-25
