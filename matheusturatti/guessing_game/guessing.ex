defmodule GuessGame do
    def guess(min, max) do 
        IO.puts("hmmm meu chute eh:\n#{meio(min,max)}")
        resposta = IO.gets("")
        case String.trim(resposta) do
            "maior" -> guess(cima(min, max), max)
            "menor" -> guess(min, baixo(min,max))
            "sim" -> IO.puts("sabia que iria acertar")
            _ -> IO.puts(~s{digite "maior", "menor" ou "sim"})
                 guess(min,max)
        end
    end
        

    def meio(a, b) when a > b do
        meio(b,a)
    end
    
    def meio(baixo,alto) do
        div(baixo+alto, 2)
    end

    def cima(baixo, alto) do
        max(meio(baixo,alto) -1 , baixo)
    end

    def baixo(baixo, alto)  do
        min(meio(baixo,alto)+1, alto)
    end

end