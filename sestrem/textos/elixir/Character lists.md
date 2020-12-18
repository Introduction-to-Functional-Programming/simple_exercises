<h1>Character lists</h1>

A character list literal uses single quotes instead of double quotes.
```elixir
iex> string = "Speedy Taco"
"Speedy Taco"
iex> char_list = 'Speedy Taco'
'Speedy Taco'
```
```elixir
iex> String.to_charlist("Hello World")
'Hello World'
iex> List.to_string('Hello World')
"Hello World"
iex> List.to_string([3, 353, 1024])
<<3, 197, 161, 208, 128>>
```
Character list literals can also be specified by using a sigil, which begins with a tilde (~) character. So 'Hello World' can be written as ~c(Hello World), where ~c() indicates a character list. I'm not sure why a sigil would be used instead of '' notation, but it is available. Perhaps it makes it easier to put single quote characters in the character list.

```
iex> char_list = ~c(Hello World)
'Hello World'
iex> char_list = ~c(A 'character list' with 'single quotes')
'A \'character list\' with \'single quotes\''
```
Elixir allows you to retrieve a character's code point value by using the  `?`  before a character, like so:  `?c`  gives you the code point number that corresponds to the letter "c", and  `?T`  results in the code point value for "T".

```
iex> ?c
99
iex> ?T
84
iex> [?T, ?c]
'Tc'
```
[https://inquisitivedeveloper.com/lwm-elixir-15/](https://inquisitivedeveloper.com/lwm-elixir-15/)
