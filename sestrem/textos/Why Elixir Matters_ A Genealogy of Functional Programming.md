
# Why Elixir Matters: A Genealogy of Functional Programming

`Palestrante: Osayame Gaius-Obaseki`

`Evento: ElixirDaze 2018`

`Trabalha na Mailchimp (plataforma para email marketing)`

Ele começa criticando o fato de, historicamente, as linguagens funcionais não terem conseguido se tornar populares por falhar em se comunicar adequadamente com a comunidade de desenvolvedores de quais os problemas do mundo real que podem ser resolvidos com a sua utilização. Exemplifica este fato mostrando a popularidade das linguagens (TIOBE index) e a quantidade de vagas de emprego em 2017/2018. Nenhuma linguagem funcional aparece nestes rankings.

## Genealogia

Entender pq uma coisa se torna mais popular que outra. Por exemplo: pq a VM do Java se tornou mais popular que a VM do Erlang?

## História da linguagem funcional

até final da década 1950: Lambda Calculus 
1958: LISP (com o objetivo de ser mais fácil de usar que o Lambda Calculus). Considerada a 1a linguagem funcional 
1970: Scheme (objetivo inicialmente acadêmico, mas houveram esforços para aplicar conceitos existentes do Lambda e LISP em problemas cotidianos) 
1973: ML (pattern matching) 
1986: Erlang (projeto da Ericsson) (concurrent + functional programming) 
1988: Miranda: descendente do ML 
1997: Haskell (com conceitos do Miranda) Início do período com aumento da tentativa de utilizar em comunidades industriais (ele faz uma analogia com o "renascimento" da Idade Média) 
2004: Scala (para utilização na VM do Java) 
2005: F# (para utilização no .NET) 
2007: Clojure (para utilização na VM do Java) 
2009: Akka. 
2011: Elixir (com o objetivo de simplificar a utilização dos conceitos criados no Erlang) 
2012: Elm (possibilidade de usar programação funcional em frontends) (fim do "renascimento")

## Why Elixir Matters

> O aumento da popularidade da Internet e a crescente necessidade de
> serviços com alta disponibilidade aumentou a quantidade de problemas
> que o Erlang consegue resolver

## Erlang x Elixir 
### Syntax
A sintaxe do Erlang não foi desenvolvida para utilização de propósito geral. Isto prejudicou a sua adoção. Elixir tenta resolver este problema (tal qual LISP fez com o Lambda Calculus)

### The Web
A linguagem perdeu a chance de ser utilizada para criação de web services em geral, apesar de que o Erlang poderia ter sido usado para a criação destas ferramentas. Esta falha fez com que outras linguagens se popularizassem: Java, Javascript e até mesmo Ruby. Todas as linguagens foram criadas após o Erlang. Elixir foi criado já com o objetivo de ser utilizado na Web.

### Evangelism
 Em todas as linguagens populares, houve um esforço inicial para que isso acontecesse e fosse criada uma comunidade ao seu redor. A Ericsson não fez isso com o Erlang. Originalmente, a Netscape se baseou no Scheme, para a criação do Javascript.

### Como a comunidade para aumentar a adoção do Elixir

 - priorizar a criação de bibliotecas para resolver problemas populares ao invés de implementar conceitos obscuros de linguagens funcionais. Ter a solução para um problema existente, facilita o convencimento para se utilizar determinada tecnologia.
 -   evangelização

 -   marketing: como "vender" a linguagem para outros programadores
 -   consultoria para resolver problemas existentes
 -   participar em meetups em conferências que não sejam de Elixir e mostrar a possibilidade de utilizá-lo na resolução de problemas.
> Written with [StackEdit](https://stackedit.io/).
