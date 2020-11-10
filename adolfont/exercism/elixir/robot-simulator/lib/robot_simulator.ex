defmodule RobotSimulator do
  @moduledoc """
  Robot Simulator on Exercism
  """
  defstruct [:direction, :position]

  @directions [:north, :west, :south, :east]
  @non_advance_movements ["L", "R"]
  @movements ["A" | @non_advance_movements]
  @invalid_instruction_error {:error, "invalid instruction"}

  @spec is_valid_direction(any) ::
          {:__block__ | {:., [], [:erlang | :not, ...]}, [],
           [{:= | {any, any, any}, [], [...]}, ...]}
  defguard is_valid_direction(direction) when not (direction in @directions)

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction :: atom, position :: {integer, integer}) :: any
  def create() do
    %__MODULE__{direction: :north, position: {0, 0}}
  end

  def create(direction, _) when is_valid_direction(direction) do
    {:error, "invalid direction"}
  end

  def create(direction, {x, y}) when is_integer(x) and is_integer(y) do
    %__MODULE__{direction: direction, position: {x, y}}
  end

  def create(_, _) do
    {:error, "invalid position"}
  end

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot :: any, instructions :: String.t()) :: any
  def simulate(robot, instructions) when is_binary(instructions) do
    instruction_list = String.graphemes(instructions)

    if valid_instructions?(instruction_list) do
      instruction_list
      |> Enum.reduce(robot, fn instruction, acc -> simulate_step(acc, instruction) end)
    else
      @invalid_instruction_error
    end
  end

  defp valid_instructions?(instruction_list) do
    Enum.all?(instruction_list, fn x -> x in @movements end)
  end

  def simulate_step(robot, one_movement) when one_movement in @non_advance_movements do
    %__MODULE__{robot | direction: new_direction(robot.direction, one_movement)}
  end

  def simulate_step(robot, "A") do
    %__MODULE__{robot | position: new_position(robot.direction, robot.position)}
  end

  defp new_direction(:north, "L"), do: :west
  defp new_direction(:west, "L"), do: :south
  defp new_direction(:south, "L"), do: :east
  defp new_direction(:east, "L"), do: :north

  defp new_direction(:north, "R"), do: :east
  defp new_direction(:west, "R"), do: :north
  defp new_direction(:south, "R"), do: :west
  defp new_direction(:east, "R"), do: :south

  defp new_position(:north, {x, y}), do: {x, y + 1}
  defp new_position(:west, {x, y}), do: {x - 1, y}
  defp new_position(:south, {x, y}), do: {x, y - 1}
  defp new_position(:east, {x, y}), do: {x + 1, y}

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot :: any) :: atom
  def direction(robot) do
    robot.direction
  end

  @doc """
  Return the robot's position.
  """
  @spec position(robot :: any) :: {integer, integer}
  def position(robot) do
    robot.position
  end
end
