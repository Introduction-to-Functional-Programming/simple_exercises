
<h1>Functions and Pattern Matching</h1>

```elixir
def generic_specific([]), do: "specific"
def generic_specific(_), do: "generic"
```
```elixir
iex> MatchingExamples.generic_specific([])
"specific"
iex> MatchingExamples.generic_specific(3)
"generic"
iex> MatchingExamples.generic_specific(3, 4)
** (UndefinedFunctionError) function MatchingExamples.generic_specific/2 is undefined or private. Did you mean one of:

      * generic_specific/1

    MatchingExamples.generic_specific(3, 4)
```
```elixir
def print_name(name), do: print_name(name, :english)
def print_name(%{first: first_name, last: last_name}, :english) do
	IO.puts("#{first_name} #{last_name}")
end		
def print_name(%{first: first_name, last: last_name}, :hungarian) do
	IO.puts("#{last_name} #{first_name}")
end	
```
```elixir
iex> name = %{first: "Bob", last: "Johansson", age: 32}
%{age: 32, first: "Bob", last: "Johansson"}
iex> MatchingExamples.print_name(name)
Bob Johansson
:ok
iex> MatchingExamples.print_name(name, :english)
Bob Johansson
:ok
iex> MatchingExamples.print_name(name, :hungarian)
Johansson Bob
:ok
iex> name = %{first: "Bob", age: 32}
%{age: 32, first: "Bob"}
iex> MatchingExamples.print_name(name)
** (FunctionClauseError) no function clause matching in MatchingExamples.print_name/2

    The following arguments were given to MatchingExamples.print_name/2:

        # 1
        %{age: 32, first: "Bob"}

        # 2
        :english

    examples/lwm 23/matching_examples.exs:16: MatchingExamples.print_name/2
```

https://inquisitivedeveloper.com/lwm-elixir-23
