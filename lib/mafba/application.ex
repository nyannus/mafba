defmodule Mafba.Application do
  use Application

  def start(_type, _args) do
    children = [
      Mafba.Display.Registry.child_spec()
    ]

    opts = [strategy: :one_for_one, name: Mafba.Application]
    Supervisor.start_link(children, opts)
  end
end
