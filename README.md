# Golden Gate - Corretora de Valores

Programa desenvolvido com a linguagem funcional Haskell para o projeto da cadeira Paradigmas de Linguagem de Programação do curso de Ciência da Computação da Universidade Federal de Campina Grande - UFCG.

Um jogo em terminal que simula uma interface de uma corretora de valores de maneira simples e intuitiva, com Home Broker, carteira, empresas fictícias e opção de compra e venda de ações.


## Demonstração
Assista a este [vídeo](https://www.youtube.com/watch?v=G0oL7a8rMmM) para entender o funcionamento do programa.

## Instalação
Você precisa da versão mais atualizada de [Haskell](https://www.haskell.org/ghcup/install/) instalada.


|Bibliotecas necessárias|Requirido para|
|-----------------------|--------------|
| Aeson                 |Manipulação do banco de dados com `json`|
| Random                |Funcionamento da lógica de variações randômicas dos preços das ações|


Instale a biblioteca `aeson`

    cabal install --lib aeson

Instale a biblioteca `random`

    cabal install --lib random

Após isso, execute o programa no seu diretório

    runhaskell Main.hs


## Parte Lógica

O projeto é divido em duas etapas, clique [aqui](https://github.com/ProjetoPLP/ProjetoPLPProlog) para acessar o repositório da parte lógica.