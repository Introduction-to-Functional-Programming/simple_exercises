
<h1>Sets</h1>
Sets are unordered collections of data where a data element can only occur once.

```elixir
iex> salad_ingredients = MapSet.new()
#MapSet<[]>
iex> MapSet.put(salad_ingredients, "lettuce")
#MapSet<["lettuce"]>
iex> MapSet.put(salad_ingredients, "cucumbers")
#MapSet<["cucumbers"]>
iex> salad_ingredients = MapSet.put(salad_ingredients, "lettuce")
#MapSet<["lettuce"]>
iex> salad_ingredients = MapSet.put(salad_ingredients, "cucumbers")
#MapSet<["cucumbers", "lettuce"]>
iex> salad_ingredients = MapSet.put(salad_ingredients, "tomatoes")
#MapSet<["cucumbers", "lettuce", "tomatoes"]>
iex> salad_ingredients = MapSet.put(salad_ingredients, "cucumbers")
#MapSet<["cucumbers", "lettuce", "tomatoes"]>
iex> MapSet.member?(salad_ingredients, "cucumbers")
true
iex> MapSet.member?(salad_ingredients, "carrots")
false
```
```elixir
iex> Enum.to_list(salad_ingredients)
["cucumbers", "lettuce", "tomatoes"]
```

https://inquisitivedeveloper.com/lwm-elixir-20/
