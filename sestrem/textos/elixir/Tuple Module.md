<h1>The Tuple Module</h1>

The Tuple module documentation warns us that modifying an existing tuple is an O(n) operation, since it causes the entire tuple to be copied. This is very inefficient.

These tuple-related functions are located in the Kernel module.

-   `elem/2` - retrieves an element in a tuple by index
-   `put_elem/3` - updates a value in a tuple by index
-   `tuple_size/` - returns the number of elements in a tuple

# **Tuple.append/2**

Appends an element at the end of a tuple. This is an inefficient operation (O(n)), since it involves copying the entire tuple.

```elixir
iex> Tuple.append({:ok, "Bob", 4}, ["a", "b", "c"])
{:ok, "Bob", 4, ["a", "b", "c"]}
iex> Tuple.append({2}, "two")
{2, "two"}
iex> Tuple.append({1, 2, 3}, 4)
{1, 2, 3, 4}
```
## Tuple.delete_at/2
Deletes the element at a particular index in the tuple. If the index does not exist, an error is raised.

```elixir
iex> data_tuple = {"Bob", :ok, 6}
{"Bob", :ok, 6}
iex> Tuple.delete_at(data_tuple, 2)
{"Bob", :ok}
iex> Tuple.delete_at(data_tuple, 1)
{"Bob", 6}
iex> Tuple.delete_at(data_tuple, 0)
{:ok, 6}
iex> Tuple.delete_at(data_tuple, -1)
** (ArgumentError) argument error
    :erlang.delete_element(0, {"Bob", :ok, 6})
iex> Tuple.delete_at(data_tuple, 3)
** (ArgumentError) argument error
    :erlang.delete_element(4, {"Bob", :ok, 6})
```
# **Tuple.duplicate/2**

Creates a new tuple and initializes it with a single data item that's repeated in every index in the tuple. So `Tuple.duplicate("Doom", 8)` is the equivalent of `{"Doom", "Doom", "Doom", "Doom", "Doom", "Doom", "Doom", "Doom"}`. This function just provides an easy way to create a repetitive tuple, and is just as efficient as creating the tuple using the literal.
```elixir
iex> Tuple.duplicate(10, 1)
{10}
iex> Tuple.duplicate(10, 0)
{}
iex> Tuple.duplicate(10, -1)
** (ArgumentError) argument error
    :erlang.make_tuple(-1, 10)
iex> Tuple.duplicate(10, 3)
{10, 10, 10}
iex> Tuple.duplicate("Doom", 8)
{"Doom", "Doom", "Doom", "Doom", "Doom", "Doom", "Doom", "Doom"}
iex> Tuple.duplicate("gremlin", 30)
{"gremlin", "gremlin", "gremlin", "gremlin", "gremlin", "gremlin", "gremlin",
 "gremlin", "gremlin", "gremlin", "gremlin", "gremlin", "gremlin", "gremlin",
 "gremlin", "gremlin", "gremlin", "gremlin", "gremlin", "gremlin", "gremlin",
 "gremlin", "gremlin", "gremlin", "gremlin", "gremlin", "gremlin", "gremlin",
 "gremlin", "gremlin"}
```
# **Tuple.insert_at/3**

Inserts a value at a particular index in a tuple. This is an inefficient operation (O(n)), since it involves copying the entire tuple. Use sparingly.
```elixir
iex> data_tuple = {"Bob", :ok, 6}
{"Bob", :ok, 6}
iex> Tuple.insert_at(data_tuple, 0, "Zim")
{"Zim", "Bob", :ok, 6}
iex> Tuple.insert_at(data_tuple, 1, "Zim")
{"Bob", "Zim", :ok, 6}
iex> Tuple.insert_at(data_tuple, 2, "Zim")
{"Bob", :ok, "Zim", 6}
iex> Tuple.insert_at(data_tuple, 3, "Zim")
{"Bob", :ok, 6, "Zim"}
iex> Tuple.insert_at(data_tuple, 4, "Zim")
** (ArgumentError) argument error
    :erlang.insert_element(5, {"Bob", :ok, 6}, "Zim")
```
# **Tuple.to_list/1**

Copies the contents of a tuple into a list, which is more friendly for sequential access and certain types of modifications.

```elixir
iex> Tuple.to_list({"Zim", "Dib", "Gaz"})
["Zim", "Dib", "Gaz"]
iex> Tuple.to_list({:ok, [1, 2, 3]})
[:ok, [1, 2, 3]]
iex> Tuple.to_list({3})
[3]
iex> Tuple.to_list({})
[]
```

https://inquisitivedeveloper.com/lwm-elixir-26/
