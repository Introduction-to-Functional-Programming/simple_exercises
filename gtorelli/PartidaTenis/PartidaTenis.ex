defmodule PartidaTenis do
  @moduledoc """
  Documentation for `PartidaTenis`.
  Neste problema você deverá implementar as regras de um jogo de tênis simples (apenas dois jogadores).

    As regras de um jogo de tênis tem diversos detalhes, mas para simplificar o problema, você deve implementar apenas as regras de um game:

    Em uma game cada jogador pode ter a seguinte pontuação: 0, 15, 30, ou 40;
    Os jogadores sempre começam com 0 pontos;
    Se o jogador possui 40 pontos e ganha a disputa, ele vence o game;
    Se ambos jogadores atingem 40 pontos, ocorre um empate (deuce);
    Estando em empate, o jogador que ganhar a bola seguinte está em vantagem (advantage);
    Se um jogador em vantagem ganha novamente a bola, ele vence o game;
    Se um jogador estiver em vantagem e o outro ganhar a bola, volta a ocorrer o empta (deuce).

    OBS: DIFERENCA DE 1 SET = VANTAGEM
         DIFERENCA DE 2 SETS = VITORIA
  """

  def resultado({pontos_j1, pontos_j2})
      when pontos_j1 == pontos_j2 do
    "Empate (deuce)"
  end

  def resultado({pontos_j1, pontos_j2})
      when pontos_j1 >= 4 and pontos_j2 == pontos_j1 - 1 do
    "Vantagem para jogador 1"
  end

  def resultado({pontos_j1, pontos_j2})
      when pontos_j2 >= 4 and pontos_j1 == pontos_j2 - 1 do
    "Vantagem para jogador 2"
  end

  def resultado({pontos_j1, pontos_j2})
      when pontos_j1 >= 4 and pontos_j2 == pontos_j1 - 2 do
    "Vitoria do jogador 1"
  end

  def resultado({pontos_j1, pontos_j2})
      when pontos_j2 >= 4 and pontos_j1 == pontos_j2 - 2 do
    "Vitoria do jogador 2"
  end
end
