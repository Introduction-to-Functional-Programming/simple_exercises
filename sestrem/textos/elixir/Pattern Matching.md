
<h1>Pattern Matching</h1>

Pattern matching is a mechanism in Elixir that controls how data are bound to variables and it can provide a form of control flow.
```elixir
"Bob"
iex> {name, favorite_color} = {"Sam", "blue"}
{"Sam", "blue"}
iex> name
"Sam"
iex> favorite_color
"blue"
```
# Pin Operator

```elixir
iex> name = "Bob"
"Bob"
iex> {^name, favorite_color} = {"Sam", "blue"}
** (MatchError) no match of right hand side value: {"Sam", "blue"}
```
`{^name, favorite_color} = {"Sam", "blue"}` is the equivalent of `{"Bob", favorite_color} = {"Sam", "blue"}`, which clearly does not match.

```elixir
iex> name = "Sam"
"Sam"
iex> {^name, favorite_color} = {"Sam", "blue"}
{"Sam", "blue"}
iex> name
"Sam"
iex> favorite_color
"blue"
```
# Wildcards
```elixir
iex> {_, _, third_item} = {10, 12, 20}
{10, 12, 20}
iex> third_item
20
```
# List Pattern Matching
```elixir
iex> [head | tail] = [4, 5, 6]
[4, 5, 6]
iex> head
4
iex> tail
[5, 6]
```
```elixir
iex> [_, second | rest_of_list] = [4, 5, 6]
[4, 5, 6]
iex> second
5
iex> rest_of_list
[6]
```

https://inquisitivedeveloper.com/lwm-elixir-22/
