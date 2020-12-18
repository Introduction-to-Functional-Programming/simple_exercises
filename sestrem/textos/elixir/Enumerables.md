
<h1>Enumerables</h1>
An enumerable in Elixir is anything that can be iterated over.

Elixir defines an enumerable as any data structure that implements the Enumerable protocol.

Common enumerables are collections such as lists and maps and streams.

Many of the functions in the Enum module are higher-order functions, where we pass the data to be operated on and a function that describes how to operate on that data. This is a common pattern in functional languages, and is typically used in place of loops from imperative languages.

```elixir
iex> numbers = [1, 3, 5, 6, 7]
[1, 3, 5, 6, 7]
iex> 3 in numbers
true
iex> 2 in numbers
false
```
**Mapping**
`Enum.map/2`  transforms every element in an enumerable. The function that we pass to it determines  _how_  each element will be transformed.

```elixir
iex> numbers = [1, 2, 3, 4, 5]  
[1, 2, 3, 4, 5]
iex> doubled_numbers = Enum.map(numbers, fn x -> x * 2 end)        
[2, 4, 6, 8, 10]
iex> squared_numbers = Enum.map(numbers, fn x -> x * x end)
[1, 4, 9, 16, 25]
```
I used  `Enum.map/2`  and an anonymous function to double all the numbers in a collection and then I passed a different function to square the numbers.

I don't have to specify an anonymous function every time. I can also use named functions or reusable anonymous functions.

```elixir
iex> double = &(&1 * 2)
#Function<6.127694169/1 in :erl_eval.expr/5>
iex> square = &(&1 * &1)
#Function<6.127694169/1 in :erl_eval.expr/5>
iex> Enum.map(numbers, double)                   
[2, 4, 6, 8, 10]
iex> Enum.map(numbers, square)
[1, 4, 9, 16, 25]
```
Notice that I used the shortcut syntax to create the `double` and `square` functions.
**Filtering**
Another useful function is  `Enum.filter/2`, which filters out certain elements from a collection. The function that I pass to it determines which elements will be kept and which will be discarded. When the function returns true, the corresponding element will be kept; otherwise, it will be discarded.

```elixir
iex> is_even = fn number -> rem(number, 2) == 0 end 
#Function<6.127694169/1 in :erl_eval.expr/5>
iex> is_even.(2)                                    
true
iex> is_even.(7)
false
iex> is_odd = fn number -> rem(number, 2) == 1 end
#Function<6.127694169/1 in :erl_eval.expr/5>
iex> is_odd.(2)
false
iex> is_odd.(7)
true
iex> Enum.filter(numbers, is_even)
[2, 4]
iex> Enum.filter(numbers, is_odd)
[1, 3, 5]

```

First, I created two reusable functions,  `is_even`  and  `is_odd`. Then I tested them to verify that they did what I intended. I used  `is_even`  to filter out the even numbers and keep them. Then I used  `is_odd`  to filter out the odd numbers and keep them.

**Concatening**
`Enum.concat/2`  concatenates two enumerables into a list.

```elixir
ex> list = [1, "Bob", :ok, 3.14]
[1, "Bob", :ok, 3.14]
iex> Enum.concat(list, [4, 8, 15])
[1, "Bob", :ok, 3.14, 4, 8, 15]
iex> Enum.concat(list, %{name: "Dib", age: 10})
[1, "Bob", :ok, 3.14, {:age, 10}, {:name, "Dib"}]
```
https://inquisitivedeveloper.com/lwm-elixir-16
