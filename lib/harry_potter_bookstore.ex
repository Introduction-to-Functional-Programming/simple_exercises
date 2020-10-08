defmodule HarryPotterBookstore do

    def books_bought(shopping_cart) do
      Enum.map(shopping_cart, fn {livro,_quantidade} -> livro end)
    end

end
