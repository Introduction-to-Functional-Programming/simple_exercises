
<h1>Lists</h1>
Lists in Elixir are actually linked lists, which are particularly suitable for functional programming because they go well together with recursion. Since a list is a linked list, modification and sharing is cheap, but random access is expensive, since it is an O(n) operation.

```elixir
iex> list = ["Gaz", :name, 3.14, 2]
["Gaz", :name, 3.14, 2]
```
Lists are made up of a head and a tail. The head is the first item in the list and the tail is the rest of the list.

![](https://inquisitivedeveloper.com/content/images/2018/12/image-2.png)

That tail also consists of a head (the first element) and a tail (the rest of the list).

![](https://inquisitivedeveloper.com/content/images/2018/12/image-3.png)

```elixir
iex> [1, 2, 3]
[1, 2, 3]
iex> [1 | [2, 3]]
[1, 2, 3]
iex> [1 | [2 | [3]]]
[1, 2, 3]
iex> [1 | [2 | [3 | []]]]
[1, 2, 3]
```
You can also think of the internals of the list as looking like this, a classic linked-list representation.

![](https://inquisitivedeveloper.com/content/images/2018/12/image-4.png)

## Prepending

We can prepend to a list using the  `|`  operator as well. It's the same basic concept: adding a head element to an existing list, which becomes a tail of the new list.

```elixir
iex> list = [3 | []]
[3]
iex> list = [2 | list]
[2, 3]
iex> list = [1 | list]
[1, 2, 3]
iex> list = [4 | list]
[4, 1, 2, 3]

```
**Prepending**

At this point you might be thinking "But I want to add items to the end of my list, otherwise I'll have a list in reverse order!". That's not a problem. The way Elixir code solves that problem is to prepend elements to a list and then reverse it all using the  `Enum.reverse/1`  function. The  `Enum.reverse/1`  function is highly optimized, and doing this is way more efficient that constructing the same list by appending.

```elixir
iex> list = [4 | [3, 2, 1]]
[4, 3, 2, 1]
iex> Enum.reverse(list)
[1, 2, 3, 4]
```
**Appending**
```elixir
iex> [1, 2, 3] ++ [4]
[1, 2, 3, 4]
iex> [1, 2, 3] ++ 4
[1, 2, 3 | 4]  #Improper list
iex> [1, 2] ++ [3, 4]
[1, 2, 3, 4]
```
You can test if an element is in a list using the `in` operator.
```elixir
iex> 10 in [5, 10, 2, 6]
true
iex> 1 in [5, 10, 2, 6]
false

```

You can find the length of a list using the  `length/1`  function.

```elixir
iex> length([1, 2, 3])
3
iex> length(["Dib"])
1
iex> length([])
0
```
You can retrieve an element from the list with the  `Enum.at/2`  function.

```elixir
iex> Enum.at([1, 2, 3], 0)
1
iex> Enum.at([1, 2, 3], 2)
3
iex> Enum.at([1, 2, 3], 5)
nil
```
Keep in mind that unlike a tuple where it's an O(1) operation, accessing a specific element in a list is a O(n) operation.

(https://inquisitivedeveloper.com/lwm-elixir-14/)
