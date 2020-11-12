
# Enum x Stream

## Collections
Elixir possui vários tipos de dados que são classificados como *collections* (lists, maps, files e mesmo funções).

Os tipos *collections* possuem implementações diferentes mas tem uma característica em comum: os seus itens podem ser percorridos e implementam o protocolo [Enumerable](https://hexdocs.pm/elixir/Enumerable.html).

O Elixir possui 2 módulos com funções para iteração de itens. O módulo [Enum](https://hexdocs.pm/elixir/Enum.html) é o mais utilizado. As suas funções são ***eager***. Isto significa que elas irão percorrer o enumerável assim que forem executadas. Desta forma, quando múltiplas operações forem executadas, cada operação irá criar uma lista intermediária. 

As funções do módulo *Stream*, da mesma forma que do *Enum*, executam operação em enumeráveis em tempo linear. Porém, ao invés de retornar um enumerável ele irá retornar um stream que deve ser utilizado em uma função *Enum*. A operação será executada apenas quando for invocada uma função do módulo *Enum*. Por isso, um stream é um enumerável "***lazy***".  
  
O objetivo de um stream e esta funcionalidade "*lazyness*" é tratar uma quantidade muito grande de dados, possivelmente infinita. De forma geral, o Stream é menos utilizado que o Enum, mas quando ele é utilizado, a diferença de performance é gritante.

## Diferença na utilização de Enum e Stream

Para exemplificar a diferença da utilização de Enum e Stream, vamos criar um map com 10 milhões de registros, aplicar nele uma função que adiciona 1 em cada elemento e então retorna os 5 primeiros elementos.

#### Utilizando Enum:
    Enum.map(1..10_000_000, &(&1+1)) |> Enum.take(5)

#### Utilizando Stream:
    Stream.map(1..10_000_000, &(&1+1)) |> Enum.take(5)

 Vamos criar uma função utilizando Enum e Stream e então utilizar a função [timer.tc](https://erlang.org/doc/man/timer.html#tc-1) para cronometrar o tempo de execução de cada função:

    stream_10M = fn() -> Stream.map(1..10_000_000, &(&1+1)) |> Enum.take(5) end
    enum_10M = fn() -> Enum.map(1..10_000_000, &(&1+1)) |> Enum.take(5) end
    
| Execução | Resultado |
|--|--|
| :timer.tc(stream_10M) | {**0**, [2, 3, 4, 5, 6]} |
| :timer.tc(enum_10M) | {**15141000**, [2, 3, 4, 5, 6]} |

A execução utilizando *Enum* levou +- 15 segundos enquanto que a com *Stream* foi praticamente instantânea. Isto ocorreu por que o *Enum* criou o map com os 10 milhões de itens para então passá-lo como parâmetro para o *Enum.take(5)*. Diferentemente, o *Stream* criou um map com apenas 5 elementos, o suficiente para a execução de *Enum.take(5)*.

## Resumo e exemplos das funções destes módulos
Durante os meus estudos eu havia feito um resumo das funções para enumeráveis. Segue abaixo:
[1a parte](https://www.notion.so/28-The-Enum-Module-Part-1-1071b44eadc64a3787097684137115b9)
[2a parte](https://www.notion.so/31-Enum-Module-Part-2-aab600c611be455bbcbde56b11c36a8a)


