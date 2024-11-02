defmodule Mafba.Display.Registry do

  def child_spec(), do: {
    Registry,
    [keys: :unique, name: __MODULE__]
  }

  def via(id), do: {:via, Registry, {__MODULE__, id}}

end
