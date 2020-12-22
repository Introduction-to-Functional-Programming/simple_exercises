## Gleam, PureScript, Miranda, Clojure, Haskell, Elm: Aula Final de Introdução à Programação Funcional

Vídeo em https://youtu.be/1CVtezl4UNU


## Gleam

Criada em 2019 e foi desenvolvida em Rust. Construída em cima da BEAM.
Inicialmente havia sido criado em Erlang. Voltada para o mercado profissional.
Apesar de não ser uma linguagem puramente funcional, todas as estruturas de dados de linguagem.
É estaticamente tipada. "Se compila, irá funcionar". Os 4 pilares são: segurança, amigável,
performance e compila para Erlang. Herda toda as otimizações da máquina virtual do Erlang.
O código-fonte em Gleam parece mais com Rust do que Elixir ou Erlang.
Suporta nativamente tipos opacos e curry, diferentemente do Elixir.
Não implementa pattern matching.

## Miranda

Linguagem puramente funcional desenvolvida em 1985. Não está mais resolvendo novas atualizações.
Antes do Miranda, o seu criador David Turner já havia criado as linguagens SASL e KRC.
Não possui características imperativas. Ela é lazy evaluation (da mesma forma que o Stream do Elixir).
Influenciou a criação das linguagens Clean e Haskell.
O seu REPL chama-se "mira", mas para criar funções é necessário utilizar scripts.
O operador de concatenação de strings e lista é o ++.
Possui os recursos de pattern matching e list comprehensions.

## Clojure
Dialeto do LISP feito para ser utilizado na JVM. Originalmente era mantido
pela empresa Cognitect que foi comprada em 2020 pela Nubank.
Todo comando é considerado uma lista: uma sequência de elementos passados entre parênteses.
A estrutura informada entre parênteses é a função a ser executada seguida dos seus parâmetros.
É possível utilizar bibliotecas do Java. Suporta funções anônimas.
Possui um recurso (função "recur") para otimizar chamadas recursivas.
Na definição de maps, tem um recurso similar ao atom do Elixir.

## Haskell
Criada em 1990 da necessidade acadêmica de uma linguagem puramente funcional open-source.
Foi inspirada pela linguagem Miranda. Influenciou a criação do Python, Rust, Elm e PureScript.
É mantida por um comitê com participantes substituídos a cada 3 anos.
A sua última versão estável foi lançada em 2010. Estava previsto para 2020 o lançamento de uma nova versão.
Possui operador inflix (ex: 5 `mod` 4). Não possui arrays, apenas listas.
Implementa lazy evaluation. É possível concatenar listas utlizando ++ ou :
Possui pattern matching, list comprehensions e guard statements.
Foi utilizado pelo Facebook e GitHub para a criação de ferramentas.

## Elm
Foi criado como um projeto de conclusão de curso em Harvard em 2012 com objetivo de criar interfaces gráficas.
Não gera exceções em tempo de execução, é fortemente tipada. É uma linguagem puramente funcional que transpila para Javascript.
A versão atual é a 0.19.1. Possui interoperabilidade com HTML, CSS e JavaScript.
Um programa em Elm sempre possui 3 partes (Elm architecture): model, view e update.
A sintaxe para criação de funções é semelhante ao PureScript, bem como a notação de tipos .
Possui funções anônimas e pipes (|>) como o Elixir. Não possui pattern matching, para simular esta funcionalidade,
Deve-se utilizar "case". Possui a ferramenta "Try Elm", onde é possível testar código-fonte Elm de forma online.

