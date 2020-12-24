##Resumo aula dia 17, Matheus Turatti - Haskell

##Gleam

É uma linguagem recente, criada em 2019, que utiliza o Erlang BEAM, criada pelo britânico Louis Pilfold, a linguagem atualmente encontra-se na sua versão beta.
Gleam é uma linguagem funcional, estaticamente tipada, todas as estruturas de dados são imutáveis, porém a linguagem não é puramente funcional. 
Louis criou a linguagem em cima de quatro pilares: segurança, amigável, performática e Erlang.
Gleam une a segurança de um robusto sistema de tipos estáticos com a confiabilidade da BEAM. 
O compilador da Gleam tem como objetivo oferecer feedbacks claros e úteis sobre qualquer tipo de problema encontrado. 
Gleam foi construída em cima da máquina virtual do Erlang e traz consigo toda a otimização oferecida por ela e ao mesmo tempo busca tornar o uso de códigos escritos em outras linguagens BEAM o mais fácil possível.

##Purescript

É uma linguagem transpilada para Javascript, no qual um programa escrito em Purescript é primeiramente transformado em Javascript para então ser compilado JIT como tal.
Foi criado em 2013 por Phil Freeman, atualmente a linguagem é open source e contém 138 contrubintes, deles 3 são empresas.
A linguagem é definida como puramente funcial, ela é eager language, fortemente tipada, inspirada em Haskell, é case sensitive e identation sensitive.
Como foi inspirada em Haskell, a linguagem dispensa parenteses para definições e chamadas de funções, possui Structs disponíveis para uso.
Purescript possui um compilador iterativo chamado Spago repl, similar a ferramenta iex de Elixir, contudo é necessário importar Prelude.
Prelude é um módulo que contém as operações mais básicas aritméticas, sem ele não é possível sequer realizar operações simples de cálculo.

##Miranda

Uma linguagem puramente funcional, declarativa de avaliação preguiçosa de tipagem forte e estática.
Criada em 1985, foi criada por David Turner e desenvolvida pela empresa Research Software Ltd.
Atualmente a lingua não é mais desenvolvida e parece ter um foco voltado aos fundamentos básicos de um paradigma funcional.
Miranda funciona através de scripts, onde a ordem das funções não é relevante.
Possui como estruturas internas listas encadeadas e tuplas, em que não é permitido misturar tipos diferentes numa mesma lista e tupla permite mais que dois elementos de tipos diferentes.
Através do comando mira é possível ativar o shell interativo do Miranda.
Através de arquivos de script é possível concatenar variáveis e funções e criar um novo programa desordenado.

##Clojure

Uma linguagem feita em dialeto de Lisp, criado para funcionar na JVM, o ambiente de bytecode de Java.
Criada por Rich Hickey, atualmente é mantida por Cognitect, sua versão atual é 1.10.0.
Sua estrutura dialético Lisp, é feita em bases de listas, composta por uma invocação com uso de parenteses, uma função e seus argumentos.
Clojure não é puramente funcional, pois chamadas de funções podem resular em efeitos colaterais em suas invocações.
Como linguagem funcional, tudo é uma expressão e em sua maioria evita efeitos colaterais para poder melhor usufruir de cálculos simultanêos.
Possui uma boa interoperabilidade com Java, pois foi criada em cima da JVM.
Seu compilador interativo é chamado REPL, cuja funcionalidade é semelhante aos iex do Elixir.

##Elm

Uma linguagem de tipos inferidos que busca eliminar erros que ocorrem em tempo de execução.
Elm é uma linguagem funcional que compila para JavaScript, útil ao considerar que boa parte da web utiliza JavaScript atualmente.
Criada por Evan Czaplicki em 2012, foi consequência de uma tese de conclusão de curso para unir paradigma funcional no escopo de interfaces gráficas.
Possui um repositório aberto no github e atualmente contém 5 colaboradores, responsáveis por evoluir a linguagem.
Entre suas principais funcionalidades se encontra estruturas de dados persistentes e interoperabilidade HTML, CSS e JavaScript.
Também usufrui de uma arquitetura própria, em que um programa Elm é dividido em três partes: Model, View e Update
Model é o estado da aplicação, View é uma função que transforma o Model em HTML e Update são funções que atualizam o Model com base em mensagens passadas em tempo de execução.
Possui tipagem estática e tipos inferidos, contudo possui sistemas de anotação para ajudar na legibilidade de código.
