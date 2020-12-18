
<h1>Structs</h1>

A struct is a data structure that stores properties and values.

A struct is actually a specialized map with predefined keys and default values, which functions as a data type. Unlike a map, where any key can be added, a struct can only contain the keys that were predefined at compile time. Any attempt to add a new property results in an error. This results in a data structure that is fixed at compile time.

## Defining Structs

```elixir
defmodule Person do
    defstruct name: "", age: 0, stage: :baby
end
```
```elixir
defmodule Person do
    defstruct [
		name: "", 
		age: 0, 
		stage: :baby
	]
end
```
# Instantiating Structs

We create an instance of the struct using the map notation (since a struct is just a specialized map), but we put the name of the struct between the "%" and "{" characters.
```elixir
iex> unknown_person = %Person{}
%Person{age: 0, name: "", stage: :baby}
iex> bob = %Person{name: "Bob"}
%Person{age: 0, name: "Bob", stage: :baby}
iex> gaz = %Person{name: "Gaz", age: 10, stage: :child}
%Person{age: 10, name: "Gaz", stage: :child}
```
# Accessing Structs

The struct properties (actually keys in a map) can be accessed using the dot notation, just like a map.
```elixir
iex> bob = %Person{name: "Bob"}
%Person{age: 0, name: "Bob", stage: :baby}
iex> gaz = %Person{name: "Gaz", age: 10, stage: :child}
%Person{age: 10, name: "Gaz", stage: :child}
iex> bob.name
"Bob"
iex> bob.age
0
iex> gaz.stage
:child
iex> gaz.favorite_color
** (KeyError) key :favorite_color not found in: %Person{age: 10, name: "Gaz", stage: :child}

```
# **Updating Structs**

Structs can be updated using the map pipe syntax. The struct to be updated is put before the pipe character and the keys and values that are to be updated are put after the pipe character.
```elixir
iex> bob = %Person{name: "Bob"}
%Person{age: 0, name: "Bob", stage: :baby}
iex> old_bob = %Person{bob | age: 95, stage: :elderly}
%Person{age: 95, name: "Bob", stage: :elderly}
```
# Packaging Struct Data and Functions
```elixir
defmodule Person do	
    defstruct name: "", age: 0, stage: :baby
	
	def new() do
		%Person{}
	end
	
	def increment_age(person) do
		%Person{person | age: person.age + 1}
	end
	
	def babify(person) do
		%Person{person | age: 1, stage: :baby}
	end
	
	def can_retire?(person, retirement_age) do
		person.age >= retirement_age
	end	
end
```
Let's load that module into IEx and start using the functions.

```elixir
iex> bob = Person.new("Bob")
%Person{age: 0, name: "Bob", stage: :baby}
iex> bob = Person.increment_age(bob)
%Person{age: 1, name: "Bob", stage: :baby}
iex> bob = Person.increment_age(bob)
%Person{age: 2, name: "Bob", stage: :baby}
iex> Person.can_retire?(bob, 60)
false
iex> bob = %Person{bob | stage: :retired}
%Person{age: 2, name: "Bob", stage: :retired}
iex> bob = Person.babify(bob)
%Person{age: 1, name: "Bob", stage: :baby}
iex> bob = %Person{bob | age: 62, stage: :retired}
%Person{age: 62, name: "Bob", stage: :retired}
iex> Person.can_retire?(bob, 60)
true
```
[https://inquisitivedeveloper.com/lwm-elixir-18]
