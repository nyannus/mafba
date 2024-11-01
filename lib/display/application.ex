defmodule Display.Application do
  use Application

  def start(_type, _args) do
    children = [
      Display.Registry.child_spec()
    ]

    opts = [strategy: :one_for_one, name: Display.Application]
    Supervisor.start_link(children, opts)
  end
end
