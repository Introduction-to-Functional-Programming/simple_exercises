defmodule PartidaTenisTest do
  use ExUnit.Case
  doctest PartidaTenis

  test "Empate" do
    assert PartidaTenis.resultado({4, 4}) == "Empate (deuce)"
  end

  test "Vantagem para jogador 1" do
    assert PartidaTenis.resultado({5, 4}) == "Vantagem para jogador 1"
  end

  test "Vantagem para jogador 2" do
    assert PartidaTenis.resultado({4, 5}) == "Vantagem para jogador 2"
  end

  test "Vitoria do jogador 1" do
    assert PartidaTenis.resultado({6, 4}) == "Vitoria do jogador 1"
  end

  test "Vitoria do jogador 2" do
    assert PartidaTenis.resultado({4, 6}) == "Vitoria do jogador 2"
  end
end
