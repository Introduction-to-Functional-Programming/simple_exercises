defmodule Day03Test do
  use ExUnit.Case
  doctest Day03

  test "Task 1 - basic test" do
    input = """
    ..##.........##.........##.........##.........##.........##.......
    #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
    .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
    ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
    .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
    ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....
    .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
    .#........#.#........#.#........#.#........#.#........#.#........#
    #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
    #...##....##...##....##...##....##...##....##...##....##...##....#
    .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#
    """

    assert Day03.task_1(input, 1, 1) == 2
    assert Day03.task_1(input, 3, 1) == 7
    assert Day03.task_1(input, 5, 1) == 3
    assert Day03.task_1(input, 7, 1) == 4
    assert Day03.task_1(input, 1, 2) == 2
  end

  test "Task 1 - test with input" do
    input = Day03.process_file("my_input.txt")

    assert Day03.task_1(input, 3, 1) == 237
  end

  test "Task 2 - test with input" do
    input = Day03.process_file("my_input.txt")

    assert Day03.task_1(input, 1, 1) == 65
    assert Day03.task_1(input, 5, 1) == 59
    assert Day03.task_1(input, 7, 1) == 61
    assert Day03.task_1(input, 1, 2) == 38
  end

  test "Task 2 - final multiplication" do
    input = Day03.process_file("my_input.txt")

    result =
      [{3, 1}, {1, 1}, {5, 1}, {7, 1}, {1, 2}]
      |> Enum.map(fn {right, down} -> Day03.task_1(input, right, down) end)
      |> Enum.reduce(1, fn x, y -> x * y end)

    assert result == 2_106_818_610
  end
end
