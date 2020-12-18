

<h1>Enum Module</h1>

Contains functions for interacting with anything enumerable.
Enumerable: lists, maps, ranges.
Not enumerable: tuples

# **Enum.all?/2**

The `[Enum.all?/2]` function takes in an enumerable and a function as parameters. It calls the function for each element, and if the function evaluates to true for every element, `Enum.all?/2` returns true. If the function you pass in returns false at least once, the call to `Enum.all?/2` returns false.

This is useful to verify that all the elements in a collection meet some criteria, where the criteria are defined in the function that was passed in as a parameter.
```elixir
#Create some reusable functions
iex> is_even = fn x -> rem(x, 2) == 0 end
#Function<6.99386804/1 in :erl_eval.expr/5>
iex> is_odd = fn x -> rem(x, 2) == 1 end
#Function<6.99386804/1 in :erl_eval.expr/5>
#Check if all the numbers in a list are even (they are)
iex> Enum.all?([2, 4, 6, 8, 10], is_even)
true
#Check if all the numbers in a list are even (they are not)
iex> Enum.all?([2, 3, 4, 6, 8, 10], is_even)
false
#Check if all the numbers in an empty list are even (this returns true)
iex> Enum.all?([], is_even)
true
#Check if all the numbers in a list are odd (they are)
iex> Enum.all?([1, 3, 5, 7, 9, 11], is_odd)
true
#Check if all the numbers in a list are odd (they are not)
iex> Enum.all?([1, 3, 5, 7, 9, 10, 11], is_odd)
false
#Check if all the names in a list start with "B" (they do)
iex> Enum.all?(["Bob", "Bill", "Bambi", "Barf"], fn name -> String.starts_with?(name, "B") end)
true
#Check if all the names in a list start with "B" (they do not)
iex> Enum.all?(["Bob", "Bill", "Dilbert", "Bambi", "Barf"], fn name -> String.starts_with?(name, "B") end)
false
```
# **Enum.any?/2**

Whereas `Enum.all?/2` will return true only if the criteria are true for all the elements in a collection, `[Enum.any?/2]` returns true if _any_ of elements match the criteria. It will only return false if the function we passed in never returns true when it is applied to the elements in the list.

This is useful to verify that at least one of the elements in a collection meet some criteria, where the criteria are defined in the function that was passed in as a parameter.
```elixir
#Create some reusable functions
iex> is_even = fn x -> rem(x, 2) == 0 end
#Function<6.99386804/1 in :erl_eval.expr/5>
iex> is_odd = fn x -> rem(x, 2) == 1 end
#Function<6.99386804/1 in :erl_eval.expr/5>
#Check if there are any odd numbers in the list (there are)
iex> Enum.any?([3, 5, 7], is_odd)
true
#Check if there are any even numbers in the list (there are not)
iex> Enum.any?([3, 5, 7], is_even)
false
#Check if there are any even numbers in the list (there are)
iex> Enum.any?([3, 4, 5, 7], is_even)
true
#Check if there are any odd numbers in the same list (there are)
iex> Enum.any?([3, 4, 5, 7], is_odd)
true
```
# **Enum.at/3**
 
The `[Enum.at/3]` function retrieves the value at a particular index in an enumerable. Since the Enum module just uses functionality that is common to all enumerables, this means that the performance will be O(n) even if the data type has other functionality that allows a O(1) value lookup.
```elixir
#Access values that exist
iex> Enum.at([1, 2, 3, 4, 5], 0)
1
iex> Enum.at([1, 2, 3, 4, 5], 2)
3
iex> Enum.at([1, 2, 3, 4, 5], 4)
5
#Access values that don't exist
iex> Enum.at([1, 2, 3, 4, 5], 6)
nil
iex> Enum.at([1, 2, 3, 4, 5], 10)
nil
#Access values using negative indexes
iex> Enum.at([1, 2, 3, 4, 5], -1)
5
iex> Enum.at([1, 2, 3, 4, 5], -2)
4
iex> Enum.at([1, 2, 3, 4, 5], -4)
2
iex> Enum.at([1, 2, 3, 4, 5], -5)
1
#Access values that don't exist using negative indexes
iex> Enum.at([1, 2, 3, 4, 5], -6)
nil
iex> Enum.at([1, 2, 3, 4, 5], -10)
nil
#Override the default value parameter with an atom
iex> Enum.at([1, 2, 3, 4, 5], 2, :not_found)
3
iex> Enum.at([1, 2, 3, 4, 5], 7, :not_found)
:not_found
iex> Enum.at([1, 2, 3, 4, 5], -7, :not_found)
:not_found
```
# **Enum.concat/1**
It receives a list of enumerables and returns a list with the elements from all those enumerables concatenated together.

```elixir
iex> Enum.concat([[1, 2], ["a", "b"]])
[1, 2, "a", "b"]
iex> Enum.concat([["a", "b"], %{name: "Bob", species: :human}, 1..10])
["a", "b", {:name, "Bob"}, {:species, :human}, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```
# **Enum.count/1**

The `[Enum.count/1]` function just returns the number of elements in an enumerable.
```elixir
iex> numbers = [1, 2, 3, 4, 5]
[1, 2, 3, 4, 5]
iex> bob_map = %{name: "Bob", age: 32}
%{age: 32, name: "Bob"}
iex> Enum.count(numbers)
5
iex> Enum.count(bob_map)
2
iex> Enum.count([])
0
```
# **Enum.dedup/1**

Returns a list where all _consecutive_ duplicate values have been removed. Duplicate values that are not consecutive will not be removed. If you want to remove all duplicate values, no matter where they are, then shove the data into a `MapSet` instead, since sets can never contain duplicate values.
```elixir
#No numbers are eliminated because there are no duplicates
iex> Enum.dedup([1, 2, 3, 4, 5])
[1, 2, 3, 4, 5]
#The duplicate numbers are eliminated because they are all consecutive
iex> Enum.dedup([1, 1, 2, 2, 3, 4, 4, 4, 5])
[1, 2, 3, 4, 5]
#All the consecutive duplicates are eliminated, but the non-consecutive duplicates are not
iex> Enum.dedup([1, 1, 2, 2, 3, 4, 4, 4, 5, 4, 4, 3, 3, 2, 1])
[1, 2, 3, 4, 5, 4, 3, 2, 1]
```
# **Enum.drop/2**

Returns a list with elements removed from the beginning or end of the enumerable. The first parameter is the enumerable and the second parameter indicates how many elements are to be dropped. Whether the elements are dropped from the beginning or end is determined by whether the second parameter is positive or negative. A positive number means that elements are to be dropped from the beginning and a negative number means that elements are to be dropped from the end.

```elixir
iex> numbers = [1, 2, 3, 4, 5]
[1, 2, 3, 4, 5]
iex> Enum.drop(numbers, 2)
[3, 4, 5]
iex> Enum.drop(numbers, 4)
[5]
iex> Enum.drop(numbers, 5)
[]
iex> Enum.drop(numbers, 6)
[]
iex> Enum.drop(numbers, -1)
[1, 2, 3, 4]
iex> Enum.drop(numbers, -3)
[1, 2]
iex> Enum.drop(numbers, -7)
[]
iex> Enum.drop(numbers, 0)
[1, 2, 3, 4, 5]
```
# **Enum.drop_every/2**
Drops every Nth element from an enumerable starting with the first element, where N is an integer that is passed in as a parameter.
```elixir
iex> numbers = 1..20
1..20
iex> Enum.drop_every(numbers, 2)
[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
iex> Enum.drop_every(numbers, 3)
[2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20]
iex> Enum.drop_every(numbers, -2)
** (FunctionClauseError) no function clause matching in Enum.drop_every/2

    The following arguments were given to Enum.drop_every/2:

        # 1
        1..20

        # 2
        -2

    (elixir) lib/enum.ex:721: Enum.drop_every/2
```
# **Enum.drop_while/2**

Similar to `Enum.drop/2` except that accepts a function as the second parameter instead of an integer. `Enum.drop_while/2` will keeps dropping elements starting at the beginning of the enumerable as long as the parameter function returns a truthy value. Once the parameter function returns a falsey value, no more elements will be dropped, even if later elements would cause it to return a truthy value.
```elixir
iex> is_even = &(rem(&1, 2) == 0)
#Function<6.99386804/1 in :erl_eval.expr/5>
iex> is_odd = &(rem(&1, 2) == 1)
#Function<6.99386804/1 in :erl_eval.expr/5>
#Drop elements as long as the numbers are even. The even numbers after the odd number will not be dropped.
iex> Enum.drop_while([2, 2, 4, 10, 12, 13, 4, 2, 8], is_even)
[13, 4, 2, 8]
#Drop elements as long as the numbers are odd. The first element is an even number, 
#so no numbers will be dropped
iex> Enum.drop_while([2, 2, 4, 10, 12, 13, 4, 2, 8], is_odd)
[2, 2, 4, 10, 12, 13, 4, 2, 8]
#Drop elements as long as the numbers are odd.
#This time there are odd numbers at the start of the list
iex> Enum.drop_while([1, 3, 5, 10, 12, 13, 4, 2, 8], is_odd)
[10, 12, 13, 4, 2, 8]
```
# **Enum.empty?/1**
Determines if an enumerable is empty. An empty enumerable is one that has no elements.
```elixir
iex> Enum.empty?([1, 2, 3])
false
iex> Enum.empty?([1])
false
iex> Enum.empty?([])
true
iex> Enum.empty?(%{})
true
```
# **Enum.fetch!/2**

Similar to the `Enum.at/3` function in that it retrieves the value found at a particular index in an enumerable. Unlike `Enum.at/3`, which returns a `nil` when a value is not found at that index, `**Enum.fetch/2` throws an error**. That's suggested by the presence of "!" in the function name, which is an Elixir naming convention that indicates that the function throws an error.

```elixir
iex> Enum.fetch!([1, 6, 10, 12], 2)
10
iex> Enum.fetch!([1, 6, 10, 12], 0)
1
iex> Enum.fetch!([1, 6, 10, 12], 3)
12
iex> Enum.fetch!([1, 6, 10, 12], 4)
** (Enum.OutOfBoundsError) out of bounds error
    (elixir) lib/enum.ex:863: Enum.fetch!/2
iex> Enum.fetch!([1, 6, 10, 12], -1)
12
iex> Enum.fetch!([1, 6, 10, 12], -3)
6
iex> Enum.fetch!([1, 6, 10, 12], -4)
1
iex> Enum.fetch!([1, 6, 10, 12], -5)
** (Enum.OutOfBoundsError) out of bounds error
    (elixir) lib/enum.ex:863: Enum.fetch!/2
```
# **Enum.fetch/2**

Same as `Enum.fetch!/2`, but it does not throw an error. I can deduce that from the function name, which is almost the same, but does not have a "!" character in it. By Elixir convention, that means that it does not throw an error.

When `Enum.fetch/2` finds an index, it will always return a tuple, with the first value being `:ok` and the second value being the value that it found at that index. When `Enum.fetch/2` sees that an index does not exist, it returns the atom `:error`. We can use pattern matching to provide different logic depending on whether the function was able to retrieve a value or not.

```elixir
iex> Enum.fetch([1, 6, 10, 12], 2)
{:ok, 10}
iex> Enum.fetch([1, 6, 10, 12], 0)
{:ok, 1}
iex> Enum.fetch([1, 6, 10, 12], 3)
{:ok, 12}
iex> Enum.fetch([1, 6, 10, 12], 4)
:error
iex> Enum.fetch([1, 6, 10, 12], -1)
{:ok, 12}
iex> Enum.fetch([1, 6, 10, 12], -3)
{:ok, 6}
iex> Enum.fetch([1, 6, 10, 12], -4)
{:ok, 1}
iex> Enum.fetch([1, 6, 10, 12], -5)
:error
```
# **Enum.filter/2**

The `[Enum.filter/2](<https://hexdocs.pm/elixir/Enum.html#filter/2>)` function is a higher-order function that receives an enumerable and a function, filters the elements in the enumerable, and returns a list containing the elements that met the criteria specified in the parameter function.

This allows us to extract data from an enumerable that meets the criteria we provide.
```elixir
iex> is_even = &(rem(&1, 2) == 0)
#Function<6.99386804/1 in :erl_eval.expr/5>
iex> is_odd = &(rem(&1, 2) == 1)
#Function<6.99386804/1 in :erl_eval.expr/5>
iex> Enum.filter([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], is_even)
[2, 4, 6, 8, 10]
iex> Enum.filter([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], is_odd)
[1, 3, 5, 7, 9]
iex> Enum.filter([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], &(&1 <= 5))
[1, 2, 3, 4, 5]
iex> Enum.filter([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], &(&1 * &1 <= 10))
[1, 2, 3]
```
# **Enum.find/3**

Higher-order function that returns the first element in an enumerable that meets the criteria, where the criteria are defined using a function that is passed in as a parameter.

This function is useful when you just want to find just the first item that meets some criteria.

# **Enum.find_index/2**

Similar to `Enum.find/3`. It's also a higher-order function that finds the first element that meets the criteria. The difference is that `Enum.find_index/2` returns the index instead of the value and there is no default value parameter.

If no items could be found that match the criteria, the function returns `nil`.

# **Enum.map/2**

It's a higher-order order function that does a one to one transformation of every item in an enumerable, where each item in the enumerable is run though a transformation function that was passed in as a parameter, and the result is added to the list that will be returned. So the number of elements in the result is the same as the number of elements that were passed in.

```elixir
#Map each number to its string equivalent
iex> Enum.map(1..10, &(Integer.to_string(&1)))
["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
#Double the numbers in the range
iex> Enum.map(1..10, &(&1 * 2))
[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
#Square the numbers in the range
iex> Enum.map(1..10, &(&1 * &1))
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
#Do no transformation at all, just passing back the number unaltered.
#It's useless, but it can be done.
iex> Enum.map(1..10, &(&1))
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```
# **Enum.map_every/3**

The `Enum.map_every/3` function closely resembles `Enum.map/2`, except that the mapping function is applied to every Nth element instead of every element. The value N is controlled by the second parameter.
```elixir
iex> numbers = 1..20
1..20
#Map every 3rd element to a string starting with the first element
iex> Enum.map_every(numbers, 3, &(Integer.to_string(&1)))
["1", 2, 3, "4", 5, 6, "7", 8, 9, "10", 11, 12, "13", 14, 15, "16", 17, 18,
 "19", 20]
#Map every 5th element to a string starting with the first element
iex> Enum.map_every(numbers, 5, &(Integer.to_string(&1)))
["1", 2, 3, 4, 5, "6", 7, 8, 9, 10, "11", 12, 13, 14, 15, "16", 17, 18, 19, 20]
#Map every 2nd element to a string starting with the first element
iex> Enum.map_every(numbers, 2, &(Integer.to_string(&1)))
["1", 2, "3", 4, "5", 6, "7", 8, "9", 10, "11", 12, "13", 14, "15", 16, "17",
 18, "19", 20]
#Map every element to a string starting with the first element
iex> Enum.map_every(numbers, 1, &(Integer.to_string(&1)))
["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14",
 "15", "16", "17", "18", "19", "20"]
#Passing a 0 as the second parameter means that no items are transformed
iex> Enum.map_every(numbers, 0, &(Integer.to_string(&1)))
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
#No negative numbers!
iex> Enum.map_every(numbers, -1, &(Integer.to_string(&1)))
** (FunctionClauseError) no function clause matching in Enum.map_every/3

    The following arguments were given to Enum.map_every/3:

        # 1
        1..20

        # 2
        -1

        # 3
        &:erlang.integer_to_binary/1

    (elixir) lib/enum.ex:1352: Enum.map_every/3
```
# **Enum.flat_map/2**

The `[Enum.flat_map/2](<https://hexdocs.pm/elixir/Enum.html#flat_map/2>)` function works just like `Enum.map/2` in that it transforms each element to another element. The difference between `map` and `flat_map` is that it will **"flatten" the results into a single list.** It unpacks any enumerables within the main enumerable and places all the elements in the main enumerable. **So if the results are a list of lists, `flat_map` will take the items from all those sub-lists and put them in a single list.**
```elixir
iex> double = &(&1 * 2)
#Function<6.99386804/1 in :erl_eval.expr/5>
iex> data = [1..10, 1..5, 1..10, [44]]
[1..10, 1..5, 1..10, ',']
#Map the data, doubling all the elements in the sub-lists
iex> Enum.map(data, &(Enum.map(&1, double)))
[
  [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
  [2, 4, 6, 8, 10],
  [2, 4, 6, 8, 10, 12, 14, 16, 18, 20],
  'X'
]
#Do the same thing with flat_map. The results are the same, but
#they are "flattened" into a single list
iex> Enum.flat_map(data, &(Enum.map(&1, double)))
[2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 2, 4, 6, 8, 10, 2, 4, 6, 8, 10, 12, 14, 16,
 18, 20, 88]
#Map the data without transforming it
iex> Enum.map(data, &(&1))
[1..10, 1..5, 1..10, ',']
#Flat map the data without transforming it. The data is the same, but it's
#been enumerated and placed in a single list
iex> Enum.flat_map(data, &(&1))
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
 44]
```
# **Enum.join/2**

The `[Enum.join/2](<https://hexdocs.pm/elixir/Enum.html#join/2>)` function will convert each item in an enumerable to a binary (this is usually a string) and then join the items together using a separator in between each pair of items, which is also a binary. The enumeration is passed in as the first parameter and the separator is passed in as an optional parameter. If no separator is specified, then the items are just joined together without a separator.

**This is the equivalent of a string join function that is commonly found in other languages, and this function is clearly targeted at joining stuff together to form a string.**
```elixir
#Create a comma-separated string containing all the numbers
iex> Enum.join(1..10, ", ")
"1, 2, 3, 4, 5, 6, 7, 8, 9, 10"
#Create a string where the separator is an "x"
iex> Enum.join(1..10, "x")
"1x2x3x4x5x6x7x8x9x10"
#Join the items together to create a string where we didn't specify a separator
iex> Enum.join(1..10)
"12345678910"
#Join the numbers with a single-byte binary separator
iex> Enum.join(1..10, <<0x01>>)
<<49, 1, 50, 1, 51, 1, 52, 1, 53, 1, 54, 1, 55, 1, 56, 1, 57, 1, 49, 48>>
#Join the numbers with a single-byte binary separator that corresponds to a string character.
iex> Enum.join(1..10, <<0x56>>)
"1V2V3V4V5V6V7V8V9V10"
```
**This function is useful for converting collections of data into strings.**

# **Enum.map_join/3**

Combines the functionality of `Enum.map/2` and `Enum.join/2` to create a function that maps and joins in a single pass through the enumerable. It first applies the mapping function to transform the item and then converts that item to a binary and joins it to the other items using a separator. It's the equivalent of calling `Enum.map/2` and then feeding the results to `Enum.join/2`, except that it's more efficient by doing that in a single pass instead of two separate passes.
```elixir
#Double the numbers and then join them as a comma-separated list
iex> double = &(&1 * 2)
#Function<6.99386804/1 in :erl_eval.expr/5>
iex> Enum.map_join(1..10, ", ", double)
"2, 4, 6, 8, 10, 12, 14, 16, 18, 20"
#Subtract 5 from the numbers and join them like a multiplication expression
iex> Enum.map_join(1..10, " * ", &(&1 - 5))
"-4 * -3 * -2 * -1 * 0 * 1 * 2 * 3 * 4 * 5"
#Subtract 5 from the numbers and don't provide a separator.
iex> Enum.map_join(1..10, &(&1 - 5))
"-4-3-2-1012345
```
# **Enum.reduce/3**

The `[Enum.reduce/3](<https://hexdocs.pm/elixir/Enum.html#reduce/3>)` function implements a reduce operation. A reduce operation (sometimes called "fold" or "aggregate") iterates over a collection, applies a function to each item in the collection, and keeps a stateful object called an accumulator that is returned by one application of the function and is passed to the next. This allows the reduce operation to build up some sort of data in the accumulator, which is finally returned as the result.

A reduce function is more generic than other functions such as `Enum.map/2` or `Enum.filter/2` as it allows you to decide what you do with the data that is derived for each item in the collection. In fact, you could implement a lot of more specific functions with reduce such as counting, summing, mapping, filtering, etc.

The `Enum.reduce/3` function has three parameters.

-   The enumerable
-   The initial accumulator to be passed into the parameter function when it is called on the first item
-   The function to be called for every item in the enumerable. The first parameter is the current item and the second parameter is the accumulator that was returned from the previous call to this function.

So the reduce operation will work like this.

1.  The function that is passed in will be called, being passed the first item and the initial accumulator that was passed in as a parameter.
2.  The function will return an accumulator value that will be passed to itself when the function is called for the next item
3.  The function is called with the next item and the accumulator from the previous function call.
4.  This will continue until there are no more elements in the enumerable.
5.  The accumulator that was returned from the final function call will be returned from the reduce function.

```elixir
iex> iex> Enum.reduce(1..10, 0, fn (number, sum) -> sum + number end)
55
```
Finding the max value in an enumerable:
```elixir
iex> Enum.reduce(1..10, 0, fn
...>    (number, max) when number <= max -> max
...>    (number, max) when number > max -> number
...> end
...> )
10
iex> Enum.reduce([3, -4, 12, 8, 0, -1, 10], 0, fn
...>    (number, max) when number <= max -> max
...>    (number, max) when number > max -> number
...> end)
12
```
# **Enum.reduce_while/3**

The `[Enum.reduce_while/3](<https://hexdocs.pm/elixir/Enum.html#reduce_while/3>)` function is like `Enum.reduce/3`, except that it is interruptable. You can stop the reduce operation somewhere in the middle of iterating over the enumerable if the reduce function returns a particular value. The value of the accumulator at that point becomes the final result.

When the reduce function wants the reduce operation to continue, it must return `{:cont, acc}`, where `acc` is the accumulator. When the reduce function wants to stop the reduce operation, it must return `{:halt, acc}`.

# **Enum.map_reduce/3**

The `[Enum.map_reduce/3](<https://hexdocs.pm/elixir/Enum.html#map_reduce/3>)` function combines the functionality of map with the functionality of reduce. It works just like `Enum.reduce/3` except that the parameter function returns two values in a tuple: a mapped value and an accumulator. The accumulator is passed to the next call to the function as normal, and the mapped value is stored in a list that is returned at the very end.

This allows us to do map and reduce operations with a single pass over the enumerable.

Let's look at two examples that I created for the reduce examples: a sum function and a max function, except that I've altered them to also return a mapped value.

Here's the example of creating a sum using Enum.reduce/3:
```elixir
iex> Enum.reduce(1..10, 0, fn (number, sum) -> sum + number end)
55
```
Here's the same thing using Enum.map_reduce/3. In addition to computing the sum, I also build a mapped list of doubled values from the original list.

```elixir
iex> Enum.map_reduce(1..10, 0, fn (number, sum) -> {number * 2, sum + number} end)
{[2, 4, 6, 8, 10, 12, 14, 16, 18, 20], 55}
```
# **Enum.group_by/3**

The [`Enum.group_by/3`] function groups data in an enumerable into a map where each group is a key-value pair. The key is whatever value the items in that group have in common, and the value is a list of the items in that group.

`Enum.group_by/3` takes three parameters:

-   An enumerable
-   A function that determines the group key of an item. The key determines which group the item is placed in.
-   An optional function that transforms an item before it is added to a group

The best way to understand this function is through example. Here's an example of grouping a range of numbers by whether they are even or odd.

# **Enum.flat_map_reduce/3**

Works just like `Enum.map_reduce/3` except that it flattens the results, like `Enum.flat_map/2` does.



https://inquisitivedeveloper.com/lwm-elixir-28
https://inquisitivedeveloper.com/lwm-elixir-31/
