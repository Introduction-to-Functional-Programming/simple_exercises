defmodule HarryPotterBookstore do
  def books_bought(shopping_cart) do
    Enum.map(shopping_cart, fn {livro, _quantidade} -> livro end)
  end

  def full_price(shopping_cart, price) do
    (Enum.map(shopping_cart, fn {_livro, quantidade} -> quantidade end) |> Enum.sum()) * price
  end

  def full_price_second_task(shopping_cart, price) do
    full_price = (Enum.map(shopping_cart, fn {_livro, quantidade} -> quantidade end) |> Enum.sum()) * price

    livros = Enum.map(shopping_cart, fn {livro, _quantidade} -> livro end)
    qtde_livros_unicos = Enum.uniq(livros) |> Enum.count()

    if qtde_livros_unicos == 2 do
      full_price * 0.95
    else
      full_price
    end
  end

  def _desconto(1) do 1 end
  def _desconto(2) do 0.95 end
  def _desconto(3) do 0.9 end
  def _desconto(4) do 0.8 end
  def _desconto(5) do 0.75 end

  def full_price_second_task_v2(shopping_cart, price) do
    full_price = (Enum.map(shopping_cart, fn {_livro, quantidade} -> quantidade end) |> Enum.sum()) * price

    livros = Enum.map(shopping_cart, fn {livro, quantidade} ->%{:livro => livro, :quantidade => quantidade} end)
    IO.inspect livros
    #livros_unicos = Enum.uniq(livros.livro)

    #livros_unicos = Enum.uniq_by([livros], fn {x, _} -> x end)
    livros_unicos = Enum.into(livros, MapSet.new())
    IO.puts "livros únicos"
    IO.inspect livros_unicos
    #IO.inspect livros_unicos




    #qtde_livros_unicos = Enum.uniq(livros) |> Enum.count()

    #full_price * _desconto(qtde_livros_unicos)
  end

  @spec full_price_third_task(any, number) :: number
  def full_price_third_task(shopping_cart, price) do
    full_price = (Enum.map(shopping_cart, fn {_livro, quantidade} -> quantidade end) |> Enum.sum()) * price

    livros = Enum.map(shopping_cart, fn {livro, _quantidade} -> livro end)
    qtde_livros_unicos = Enum.uniq(livros) |> Enum.count()

    if qtde_livros_unicos ==3  do
      full_price * 0.9
    else
      full_price
    end
  end


  def full_price_fourth_task(shopping_cart, price) do
    full_price = (Enum.map(shopping_cart, fn {_livro, quantidade} -> quantidade end) |> Enum.sum()) * price

    livros = Enum.map(shopping_cart, fn {livro, _quantidade} -> livro end)
    qtde_livros_unicos = Enum.uniq(livros) |> Enum.count()

    if qtde_livros_unicos == 4  do
      full_price * 0.8
    else
      full_price
    end
  end

  def full_price_fifth_task(shopping_cart, price) do
    full_price = (Enum.map(shopping_cart, fn {_livro, quantidade} -> quantidade end) |> Enum.sum()) * price

    livros = Enum.map(shopping_cart, fn {livro, _quantidade} -> livro end)
    qtde_livros_unicos = Enum.uniq(livros) |> Enum.count()

    if qtde_livros_unicos == 5  do
      full_price * 0.75
    else
      full_price
    end
  end

  def full_price_all_rules(shopping_cart, price) do
    full_price = (Enum.map(shopping_cart, fn {_livro, quantidade} -> quantidade end) |> Enum.sum()) * price

    livros = Enum.map(shopping_cart, fn {livro, _quantidade} -> livro end)
    qtde_livros_unicos = Enum.uniq(livros) |> Enum.count()

    case qtde_livros_unicos do
      2 -> full_price * 0.95
      3 -> full_price * 0.9
      4 -> full_price * 0.8
      5 -> full_price * 0.75
      _ -> full_price
    end
  end


end
