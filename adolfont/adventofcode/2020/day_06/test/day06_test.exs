defmodule Day06Test do
  use ExUnit.Case

  test "Open the input file and gets the groups" do
    assert length(Day06.get_groups_for(File.read!("input.txt"))) == 498
  end

  test "Gets the answers from a group" do
    a_group =
      "rypdeiqkbgacnxwumhtozfjvs\nmhrqdwtxcfjuseknozipayvbg\ngunjdtebovsyihraczkmqxfpw\npqcnduafgkbzjhvirxtwmesoy"

    assert Day06.get_answers(a_group) ==
             [
               "rypdeiqkbgacnxwumhtozfjvs",
               "mhrqdwtxcfjuseknozipayvbg",
               "gunjdtebovsyihraczkmqxfpw",
               "pqcnduafgkbzjhvirxtwmesoy"
             ]
  end

  test "Count yes answers in a plane" do
    a_plane = """
    abc

    a
    b
    c

    ab
    ac

    a
    a
    a
    a

    b
    """

    assert Day06.get_yes_answers(a_plane) == 11
  end

  test "Count yes answers in the plane from my input file" do
    a_plane = File.read!("input.txt")

    assert Day06.get_yes_answers(a_plane) == 7128
  end

  test "Count yes answers in a plane for everyone in a group (Task 2)" do
    a_plane = """
    abc

    a
    b
    c

    ab
    ac

    a
    a
    a
    a

    b
    """

    assert Day06.get_yes_answers_for_everyone_in_a_group(a_plane) == 6
  end

  test "Count yes answers in a plane for everyone in  my input file (Task 2)" do
    a_plane = File.read!("input.txt")

    assert Day06.get_yes_answers_for_everyone_in_a_group(a_plane) == 3640
  end
end
