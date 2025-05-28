defmodule Termbox2Test do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Bitwise
  import Mox
  use Termbox2
  doctest Termbox2

  setup :verify_on_exit!

  describe "init/0 and shutdown/0" do
    test "delegates to NIF and maps return values" do
      Termbox2NIFMock
      |> expect(:tb_init, fn -> 0 end)
      assert Termbox2.init() == :ok

      Termbox2NIFMock
      |> expect(:tb_init, fn -> -1 end)
      assert Termbox2.init() == {:error, :failed}

      Termbox2NIFMock
      |> expect(:tb_shutdown, fn -> 0 end)
      assert Termbox2.shutdown() == :ok
    end
  end

  describe "set_input_mode/1 argument conversion" do
    test "accepts atoms and converts to NIF int" do
      Termbox2NIFMock
      |> expect(:tb_set_input_mode, fn 0 -> 1 end)
      assert Termbox2.set_input_mode(:esc) == :alt

      Termbox2NIFMock
      |> expect(:tb_set_input_mode, fn 2 -> 0 end)
      assert Termbox2.set_input_mode(:normal) == :esc
    end
    test "returns error for invalid atom" do
      assert Termbox2.set_input_mode(:foo) == {:error, :badarg}
    end
  end

  describe "set_cell/5 color/attr atom conversion" do
    test "converts color/attr atoms to NIF ints" do
      Termbox2NIFMock
      |> expect(:tb_set_cell, fn 1, 2, ?A, 2, 0 -> 0 end)
      assert Termbox2.set_cell(1, 2, ?A, :red, :default) == :ok

      Termbox2NIFMock
      |> expect(:tb_set_cell, fn 1, 2, ?A, 258, 0 -> 0 end)
      assert Termbox2.set_cell(1, 2, ?A, [:red, :bold], :default) == :ok
    end
    test "returns error for invalid ch" do
      assert Termbox2.set_cell(1, 2, nil, :red, :default) == {:error, :badarg}
    end
  end

  describe "peek_event/1 and poll_event/0 event struct mapping" do
    test "maps NIF tuple to %Termbox2.Event{}" do
      Termbox2NIFMock
      |> expect(:tb_peek_event, fn 100 -> {:ok, 0, 1, 0x1B} end)
      assert Termbox2.peek_event(100) == {:ok, %Termbox2.Event{type: :key, mod: [:alt], key: :esc, ch: 0x1B, w: nil, h: nil, x: nil, y: nil}}
    end
    test "returns error for timeout" do
      Termbox2NIFMock
      |> expect(:tb_peek_event, fn 100 -> -1 end)
      assert Termbox2.peek_event(100) == {:error, :timeout}
    end
  end

  describe "property-based tests for set_cell/5" do
    property "accepts any valid integer coordinates and char" do
      check all x <- StreamData.integer(0..100),
                y <- StreamData.integer(0..100),
                ch <- StreamData.integer(32..126) do
        Termbox2NIFMock
        |> expect(:tb_set_cell, fn ^x, ^y, ^ch, _, _ -> 0 end)
        assert Termbox2.set_cell(x, y, ch, :default, :default) == :ok
      end
    end
  end

  test "greets the world" do
    assert Termbox2.hello() == :world
  end

  test "delegates to :termbox2_nif.tb_width/0" do
    Termbox2NIFMock
    |> expect(:tb_width, fn -> 80 end)
    assert Termbox2.width() == {:ok, 80}
  end

  test "delegates to :termbox2_nif.tb_height/0" do
    Termbox2NIFMock
    |> expect(:tb_height, fn -> 24 end)
    assert Termbox2.height() == {:ok, 24}
  end

  test "delegates to :termbox2_nif.tb_clear/0" do
    Termbox2NIFMock
    |> expect(:tb_clear, fn -> 0 end)
    assert Termbox2.clear() == :ok
  end

  test "delegates to :termbox2_nif.tb_present/0" do
    Termbox2NIFMock
    |> expect(:tb_present, fn -> 0 end)
    assert Termbox2.present() == :ok
  end

  test "delegates to :termbox2_nif.tb_set_cursor/2" do
    Termbox2NIFMock
    |> expect(:tb_set_cursor, fn 1, 2 -> 0 end)
    assert Termbox2.set_cursor(1, 2) == :ok
  end

  test "delegates to :termbox2_nif.tb_hide_cursor/0" do
    Termbox2NIFMock
    |> expect(:tb_hide_cursor, fn -> 0 end)
    assert Termbox2.hide_cursor() == :ok
  end

  test "delegates to :termbox2_nif.tb_print/5" do
    Termbox2NIFMock
    |> expect(:tb_print, fn 1, 2, 1, 0, "Hello" -> 0 end)
    assert Termbox2.print(1, 2, 1, 0, "Hello") == :ok
  end

  test "delegates to :termbox2_nif.tb_set_output_mode/1" do
    Termbox2NIFMock
    |> expect(:tb_set_output_mode, fn 0 -> 1 end)
    assert Termbox2.set_output_mode(:normal) == :"256"
  end

  test "delegates to :termbox2_nif.tb_set_clear_attrs/2" do
    Termbox2NIFMock
    |> expect(:tb_set_clear_attrs, fn 1, 0 -> 0 end)
    assert Termbox2.set_clear_attrs(1, 0) == :ok
  end
end
