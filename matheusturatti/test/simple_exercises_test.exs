defmodule SimpleExercisesTest do
  use ExUnit.Case
  doctest SimpleExercises

  test "greets the world" do
    assert SimpleExercises.hello() == :world
  end
end
