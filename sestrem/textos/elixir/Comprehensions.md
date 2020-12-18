

<h1>Comprehensions</h1>
A comprehension is a type of statement that offers a shortcut for processing collections.

Comprehensions allow collections of data to be transformed or filtered, similar to Enum.map/2 and Enum.filter/2, but with different syntax. In some cases, the comprehension syntax is easier to read and maintain than the equivalent operation using Enum functions.

# Simple Data Transformation Example

We'll palindromize the items in the list of fruit by taking each fruit and concatenating it with a reversed version of the fruit.
```elixir
iex> available_fruit = ["apples", "guavas", "pinapples", "bananas"]
["apples", "guavas", "pinapples", "bananas"]
iex> for fruit <- available_fruit, do: fruit <> String.reverse(fruit)
["applesselppa", "guavassavaug", "pinapplesselppanip", "bananassananab"]
```
List comprehension is not the only way to do this. We can also do it with Enum.map/2.
```elixir
iex> Enum.map(available_fruit, fn fruit -> fruit <> String.reverse(fruit) end)
["applesselppa", "guavassavaug", "pinapplesselppanip", "bananassananab"]
```
# Data Combination Example

```elixir
iex> letters = ["a", "b", "c", "d"]
["a", "b", "c", "d"]
iex> numbers = [1, 2, 3, 4]
[1, 2, 3, 4]
iex> for letter <- letters, number <- numbers, do: {letter, number}
[
  {"a", 1},
  {"a", 2},
  {"a", 3},
  {"a", 4},
  {"b", 1},
  {"b", 2},
  {"b", 3},
  {"b", 4},
  {"c", 1},
  {"c", 2},
  {"c", 3},
  {"c", 4},
  {"d", 1},
  {"d", 2},
  {"d", 3},
  {"d", 4}
]		
```
```elixir
iex> Enum.flat_map(letters, fn letter -> Enum.map(numbers, fn number -> {letter, number} end) end)
[
  {"a", 1},
  {"a", 2},
  {"a", 3},
  {"a", 4},
  {"b", 1},
  {"b", 2},
  {"b", 3},
  {"b", 4},
  {"c", 1},
  {"c", 2},
  {"c", 3},
  {"c", 4},
  {"d", 1},
  {"d", 2},
  {"d", 3},
  {"d", 4}
]
```
https://inquisitivedeveloper.com/lwm-elixir-21/
