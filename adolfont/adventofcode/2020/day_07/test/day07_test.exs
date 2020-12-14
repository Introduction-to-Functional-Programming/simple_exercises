defmodule Day07Test do
  import Day07
  use ExUnit.Case

  test "parse rules" do
    rule_1 = "faded blue bags contain no other bags."

    assert parse_rule(rule_1) == [{"", 0, "faded blue"}]
    rule_2 = "bright white bags contain 1 shiny gold bag."
    assert parse_rule(rule_2) == [{"shiny gold", 1, "bright white"}]
    rule_3 = "light red bags contain 1 bright white bag, 2 muted yellow bags."

    assert parse_rule(rule_3) == [
             {"bright white", 1, "light red"},
             {"muted yellow", 2, "light red"}
           ]

    rule_4 =
      "bright bronze bags contain 4 striped purple bags, 1 dull crimson bag, 4 dotted plum bags, 1 vibrant silver bag."

    assert parse_rule(rule_4) ==
             [
               {"striped purple", 4, "bright bronze"},
               {"dull crimson", 1, "bright bronze"},
               {"dotted plum", 4, "bright bronze"},
               {"vibrant silver", 1, "bright bronze"}
             ]
  end

  test "Day 7 first test" do
    rules = """
    light red bags contain 1 bright white bag, 2 muted yellow bags.
    dark orange bags contain 3 bright white bags, 4 muted yellow bags.
    bright white bags contain 1 shiny gold bag.
    muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
    shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
    dark olive bags contain 3 faded blue bags, 4 dotted black bags.
    vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
    faded blue bags contain no other bags.
    dotted black bags contain no other bags.
    """

    #  Each line contains a rule
    # Each rule:
    # <color as two words> contain
    # [no other bags.
    # |
    # comma-separated list of "<number> <color as two words> bags" with
    # dot in the end.
    # ]
    # create:
    # {color_1, n} -> color_2 or {color_1, n, color_2}
    # meaning that
    # n color_1 bags can be in a coloe_2 bag

    # than from color_x, find transitively the other colors

    assert Day07.can_contain("shiny gold") == 4
  end
end
