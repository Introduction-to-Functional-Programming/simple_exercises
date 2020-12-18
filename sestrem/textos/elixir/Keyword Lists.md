<h1>Keyword Lists</h1>

They look a lot like maps and they have a map-like interface, but they are actually implemented as lists of tuples.

The primary benefit of keyword lists is that you can specify multiple key-value pairs for the same key. Also, the key-value pairs are ordered. Maps are unordered, but lists are ordered.

Are typically used to specify options that are passed to a function.
```elixir
iex> option_list = [size: 12, color: "red", color: "blue", style: :bold, style: :italic]
[size: 12, color: "red", color: "blue", style: :bold, style: :italic]
```
```elixir
defmodule KeywordListExample do
	@default_options [color: "blue", size: "tiny", speed: "low"]
	
	def create_vehicle(registration_number, options \\ []) do
		#Merge the default options with the options that were passed
		options = Keyword.merge(@default_options, options)
		
		%{id: registration_number, color: options[:color], size: options[:size], speed: options[:speed]}
	end
end
```
```elixir
iex> KeywordListExample.create_vehicle(4324324)
%{color: "blue", id: 4324324, size: "tiny", speed: "low"}
iex> KeywordListExample.create_vehicle(4324324, [color: "red"])
%{color: "red", id: 4324324, size: "tiny", speed: "low"}
iex> KeywordListExample.create_vehicle(4324324, [color: "red", color: "blue"])
%{color: "red", id: 4324324, size: "tiny", speed: "low"}
iex> KeywordListExample.create_vehicle(4324324, [color: "red", size: "medium", speed: "andante"])
%{color: "red", id: 4324324, size: "medium", speed: "andante"}
```

https://inquisitivedeveloper.com/lwm-elixir-19/
