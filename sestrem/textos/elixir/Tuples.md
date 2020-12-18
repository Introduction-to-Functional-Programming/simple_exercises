
<h1>Tuples</h1>

_The Tuple module documentation warns us that modifying an existing tuple is an O(n) operation, since it causes the entire tuple to be copied. This is very inefficient._

Tuples in Elixir, like with other languages, are meant to serve as a container for grouping data. They are not meant to serve as an iterable collection like an array would.

A typical use for tuples is to return multiple items of data from a function. Many functions in Elixir return a tuple containing a status code and data. The status codes are always atoms and `:ok` and `:error` are common status codes.

Interesting fact: There aren't arrays in Elixir. Instead, you get to choose between types that are more suited for functional programming.

```elixir
iex> data_tuple = {"Dib", "Zim", 10, 3.14}
{"Dib", "Zim", 10, 3.14}
```
The  `elem/2`  function is used to access a data item in a tuple.

```elixir
iex> elem(data_tuple, 0)
"Dib"
iex> elem(data_tuple, 1)
"Zim"
iex> elem(data_tuple, 2)
10
iex> elem(data_tuple, 3)
3.14
iex> elem(data_tuple, 4)
** (ArgumentError) argument error
    :erlang.element(5, {"Dib", "Zim", 10, 3.14})

```

The length of a tuple can be found with the  `tuple_size/1`  function.
```elixir
iex> tuple_size({"Dib", "Zim", 10, 3.14})
4
iex> tuple_size({"Bob", 3})
2
iex> tuple_size({3})
1
iex> tuple_size({})
0
```

https://inquisitivedeveloper.com/lwm-elixir-13/
