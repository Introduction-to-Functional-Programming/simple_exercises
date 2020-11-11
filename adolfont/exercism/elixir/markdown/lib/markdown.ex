defmodule Markdown do
  @doc """
    Parses a given string with Markdown syntax and returns the associated HTML for that string.

    ## Examples

    iex> Markdown.parse("This is a paragraph")
    "<p>This is a paragraph</p>"

    iex> Markdown.parse("#Header!\n* __Bold Item__\n* _Italic Item_")
    "<h1>Header!</h1><ul><li><em>Bold Item</em></li><li><i>Italic Item</i></li></ul>"
  """
  @spec parse(String.t()) :: String.t()
  def parse(m) do
    # Here I introduced a sequence of pipes |>
    m
    |> String.split("\n")
    |> Enum.map(fn t -> process(t) end)
    |> Enum.join()
    |> introduce_html_unordered_list_tag()
  end

  def process("#" <> _ = t) do
    process_header_text(t)
  end

  def process("*" <> _ = t) do
    parse_list_md_level(t)
  end

  def process(t) do
    enclose_with_paragraph_tag(String.split(t))
  end

  defp process_header_text(t) do
    t |> String.split() |> parse_header_md_level() |> enclose_with_header_tag()
  end

  defp parse_header_md_level([h | t]) do
    {to_string(String.length(h)), Enum.join(t, " ")}
  end

  defp parse_list_md_level(l) do
    l
    |> do_parse_list_md_level()
    |> join_words_with_tags()
    |> enclose_with("li")
  end

  defp enclose_with(string, tag) do
    open(tag) <> string <> close(tag)
  end

  defp open(tag_name) do
    "<#{tag_name}>"
  end

  defp close(tag_name) do
    "</#{tag_name}>"
  end

  defp do_parse_list_md_level(l) do
    # Introducing pipes
    l
    |> String.trim_leading("* ")
    |> String.split()
  end

  defp enclose_with_header_tag({hl, htl}) do
    enclose_with(htl, "h#{hl}")
  end

  defp enclose_with_paragraph_tag(t) do
    join_words_with_tags(t)
    |> enclose_with("p")
  end

  defp join_words_with_tags(t) do
    # Introducing pipes
    t
    |> Enum.map(fn w -> replace_md_with_tag(w) end)
    |> Enum.join(" ")
  end

  defp replace_md_with_tag(w) do
    # Introducing pipes
    w
    |> replace_prefix_md()
    |> replace_suffix_md()
  end

  defp replace_prefix_md(w) do
    # introduced auxiliary functions

    cond do
      contains_opening_bold?(w) -> add_bold(w)
      contains_opening_italic?(w) -> add_italic(w)
      true -> w
    end
  end

  # auxiliary functions

  defp contains_opening_bold?(word) do
    word =~ ~r/^#{"__"}{1}/
  end

  defp contains_opening_italic?(word) do
    word =~ ~r/^[#{"_"}{1}][^#{"_"}+]/
  end

  defp add_bold(word) do
    String.replace(word, ~r/^#{"__"}{1}/, "<strong>", global: false)
  end

  defp add_italic(word) do
    String.replace(word, ~r/_/, "<em>", global: false)
  end

  defp replace_suffix_md(w) do
    # introduced auxiliary functions
    cond do
      contains_closing_bold?(w) -> add_closing_bold(w)
      contains_closing_italic?(w) -> add_closing_italic(w)
      true -> w
    end
  end

  # auxiliary functions
  defp contains_closing_bold?(word) do
    word =~ ~r/#{"__"}{1}$/
  end

  defp contains_closing_italic?(word) do
    word =~ ~r/[^#{"_"}{1}]/
  end

  defp add_closing_bold(w) do
    String.replace(w, ~r/#{"__"}{1}$/, "</strong>")
  end

  defp add_closing_italic(w) do
    String.replace(w, ~r/_/, "</em>")
  end

  defp introduce_html_unordered_list_tag(l) do
    # introduced auxiliary functions
    # Renamed function and introduced pipes
    l
    |> add_initial_ul_tag()
    |> add_closing_ul_tag()
  end

  # auxiliary functions
  defp add_initial_ul_tag(line) do
    String.replace(line, "<li>", "<ul><li>", global: false)
  end

  defp add_closing_ul_tag(line) do
    String.replace_suffix(
      line,
      "</li>",
      "</li></ul>"
    )
  end
end
