ExUnit.start()

if Code.ensure_loaded?(Mox.Server) do
  Mox.Server.start_link([])
end

Mox.defmock(Termbox2NIFMock, for: Termbox2.NIFBehaviour)
Application.put_env(:termbox2_elixir, :nif_module, Termbox2NIFMock)
