defmodule Mafba.Display do
  require Logger
  import Bitwise
  alias Circuits.UART

  # OP Codes
  @cmd_set_all        0b0001
  @cmd_set_home       0b0010
  @cmd_reset          0b0011
  @cmd_read_status    0b0100
  @cmd_read_code      0b0101
  @cmd_lock           0b0110
  @cmd_unlock         0b0111
  @cmd_set_code       0b1000
  @cmd_start_calbr2   0b1001
  @cmd_start_calbr1   0b1010
  @cmd_calstop        0b1011
  @cmd_get_calvalues  0b1100
  @cmd_set_table      0b1101
  @cmd_delete_table   0b1110

  # Error codes
  @err_comm_error     0b1000
  @err_start_missing  0b1001
  @err_unknown_char   0b1010
  @err_external_rot   0b1011
  @err_rot_timeout    0b1100
  @err_fbm_missing    0b1101
  @err_rotating       0b1111

  @type display()  :: binary()
  @type address()  :: non_neg_integer()
  @type code()     :: non_neg_integer()
  @type position() :: non_neg_integer()

  @type command_error()   :: {:error, atom() | File.posix()}
  @type command_noreply() :: {:ok, :noreply}
  @type command_reply()   :: {:ok, :reply, binary} | {:ok, :error, atom()}

  @type command() :: :set_all | :set_home
    | :reset
    | :status
    | :lock | :unlock
    | :set_code
    | :calbr1 | :calbr2 | :calstop | :calvals
    | :tbl_set | :tbl_del

  # Address expansion bit
  defp flag_ba(a) when a > 127, do: 1
  defp flag_ba(_), do: 0

  # Code expansion bit
  defp flag_bcp(c, p \\ 0)
  defp flag_bcp(c, p) when c > 127 or p > 127, do: 1
  defp flag_bcp(_, _), do: 0

  # Build the command header as aligned bytes # | 1 |    BA Flag   |     BCP Flag    |  1  |         OP Code       |     Address   |      Code      |    Position   |
  @spec build_command(command(), address(), code(), position()) :: binary()
  def build_command(:set_all,  _, _, _), do: <<1::1,          0::1,             0::1, 1::1, @cmd_set_all::4                                                           >>
  def build_command(:set_home, _, _, _), do: <<1::1,          0::1,             0::1, 1::1, @cmd_set_home::4                                                          >>
  def build_command(:reset,    _, _, _), do: <<1::1,          0::1,             0::1, 1::1, @cmd_reset::4                                                             >>
  def build_command(:status,   a, _, _), do: <<1::1, flag_ba(a)::1,             0::1, 1::1, @cmd_read_status::4,   (a &&& 0x7f)::8                                    >>
  def build_command(:code,     a, _, _), do: <<1::1, flag_ba(a)::1,             0::1, 1::1, @cmd_read_code::4,     (a &&& 0x7f)::8                                    >>
  def build_command(:lock,     a, _, _), do: <<1::1, flag_ba(a)::1,             0::1, 1::1, @cmd_lock::4,          (a &&& 0x7f)::8                                    >>
  def build_command(:unlock,   a, _, _), do: <<1::1, flag_ba(a)::1,             0::1, 1::1, @cmd_unlock::4,        (a &&& 0x7f)::8                                    >>
  def build_command(:set_code, a, c, _), do: <<1::1, flag_ba(a)::1,   flag_bcp(c)::1, 1::1, @cmd_set_code::4,      (a &&& 0x7f)::8, (c &&& 0x7f)::8                   >>
  def build_command(:calbr1,   _, _, _), do: <<1::1,          0::1,             0::1, 1::1, @cmd_start_calbr1::4                                                      >>
  def build_command(:calbr2,   _, _, _), do: <<1::1,          0::1,             0::1, 1::1, @cmd_start_calbr2::4                                                      >>
  def build_command(:calstop,  _, _, _), do: <<1::1,          0::1,             0::1, 1::1, @cmd_calstop::4                                                           >>
  def build_command(:calvals,  a, _, _), do: <<1::1, flag_ba(a)::1,             0::1, 1::1, @cmd_get_calvalues::4, (a &&& 0x7f)::8                                    >>
  def build_command(:tbl_set,  a, c, p), do: <<1::1, flag_ba(a)::1, flag_bcp(c,p)::1, 1::1, @cmd_set_table::4,     (a &&& 0x7f)::8, (c &&& 0x7f)::8, (p &&& 0x7f)::8  >>
  def build_command(:tbl_del,  a, _, _), do: <<1::1, flag_ba(a)::1,             0::1, 1::1, @cmd_delete_table::4,  (a &&& 0x7f)::8                                    >>
  def build_command(_,_,_,_), do: <<>>

  def send_command(device, command, expected_resp_size \\ 0, addr \\ nil, code \\ nil, pos \\ nil) do
    payload = build_command(command, addr, code, pos)
    display = Display.Registry.via(device)
    Logger.debug("Device: #{device} TX >>> #{inspect(payload, base: :hex)}")
    with :ok <- UART.write(display, payload) do
      if expected_resp_size > 0 do
        case UART.read(display) do
          {:ok, <<>>}      ->
            Logger.debug("Device: #{device} RX <<< timeout")
            {:ok, :reply, :timeout}
          {:ok, response}  ->
            Logger.debug("Device: #{device} RX <<< #{inspect(response, base: :hex)}")
            {:ok, :reply, response}
          {:error, :ebadf} -> {:ok, :error, :closed}
        end
      else
        {:ok, :noreply}
      end
    else
      {:error, :ebadf} -> {:error, :closed}
      {:error, error}  -> {:error, error}
    end
  end

  def status_code(code) when (code &&& 0x08) != 0 do
    case code &&& 0x0f do
      @err_comm_error     -> [:comm_error]
      @err_start_missing  -> [:start_missing]
      @err_unknown_char   -> [:unknown_char]
      @err_external_rot   -> [:external_rotation]
      @err_rot_timeout    -> [:rotation_timeout]
      @err_fbm_missing    -> [:fbm_missing]
      @err_rotating       -> [:rotating]
    end
  end

  def status_code(code) do
    <<0::1, no_ac::1, no_fi::1, no_hi::1>> = <<code::4>>
    []
    ++ (if no_ac == 1, do: [:no_ac],        else: [])
    ++ (if no_fi == 1, do: [:no_flap_imps], else: [])
    ++ (if no_hi == 1, do: [:no_home_imp],  else: [])
  end

  @spec sigil_t(binary(), maybe_improper_list()) :: map()
  def sigil_t(str, _offset) do
    # parseoffset = fn str ->
    #   case :string.to_integer(str) do
    #     {:error, _} -> raise "Invalid table offset"
    #     {offset, _} -> offset
    #   end
    # end
    # offsetfn = case offset do
    #   [] -> fn x -> x end
    #   o  -> fn x ->
    #     index = String.graphemes(str) |> Enum.find_index(fn y -> y == o end)
    #     if index == nil, do: throw "Invalid offset anchor"

    #   end
    # end
    # offsetfn = case offset do
    #   []       -> fn x -> x end
    #   [?p | o] -> fn x -> rem(x - parseoffset.(o), String.length(str)) end
    #   [?n | o] -> fn x -> rem(x + parseoffset.(o), String.length(str)) end
    #   _ -> raise "Invalid table offset"
    # end
    (0..String.length(str) - 1)
    |> Enum.map(fn index -> {String.at(str, index), index + 0x20} end)
    |> Map.new()
  end

  @spec __using__(any()) :: Macro.t()
  defmacro __using__(opts) do

    device = Keyword.fetch! opts, :device

    quote do
      require Logger
      import unquote(__MODULE__)
      use GenServer

      # Accumulate the state of all defined segemnts
      Module.register_attribute __MODULE__, :segment, accumulate: true

      @spec start_link() :: GenServer.on_start()
      def start_link() do
        GenServer.start_link(__MODULE__, [])
      end

      # Starts a UART connection to the specified device and saves the connection as the state
      #* (yes a genserver is a bit overkill here)
      def init(_) do
        {:ok, uart} = Circuits.UART.start_link(name: {:via, Registry, {Display.Registry, unquote(device)}})
        case Circuits.UART.open(uart, unquote(device), parity: :even, rx_framing_timeout: 2000, active: false, speed: 4800) do
          :ok           -> {:ok, uart}
          {:error, err} -> {:stop, {:uart_error, err}}
        end
      end

      ## Callbacks

      @callback table(addr :: non_neg_integer()) :: map()
      @callback address(pos :: non_neg_integer()) :: non_neg_integer()

      ## Client functions bound to current device

      def sync(),                     do: send_command(unquote(device), :set_all)
      def home(),                     do: send_command(unquote(device), :set_home)
      def reset(),                    do: send_command(unquote(device), :reset)
      def status(addr),               do: send_command(unquote(device), :status,    1, addr) |> status_code()
      def code(addr),                 do: send_command(unquote(device), :code,      1, addr)
      def set_code(addr, code),       do: send_command(unquote(device), :set_code,  0, addr, code)
      def lock(addr),                 do: send_command(unquote(device), :lock,      0, addr)
      def unlock(addr),               do: send_command(unquote(device), :unlock,    0, addr)
      def calibrate(:br1),            do: send_command(unquote(device), :calbr1)
      def calibrate(:br2),            do: send_command(unquote(device), :calbr2)
      def calibrate(:stop),           do: send_command(unquote(device), :calstop)
      def cal_values(addr),           do: send_command(unquote(device), :calvals,   1, addr)
      def table_set(addr, code, pos), do: send_command(unquote(device), :tbl_set,   0, addr, code, pos)
      def table_del(addr),            do: send_command(unquote(device), :tbl_del,   0, addr)

      ## Extended functions

      # Translates a text to the individual instructions for each segment and sends them commands
      def text(txt) do
        for {c, index} <- String.graphemes(txt) |> Enum.with_index() do
          try do Map.fetch!(table(index), c) rescue _ -> raise "Unsupported character #{c} for position #{index}" end
        end
        |> Enum.zip(for pos <- 0..String.length(txt) - 1 do
          try do address(pos) rescue _ -> raise "Oversized string for display. Position #{pos} is not supported" end
        end)
        |> Enum.map(fn {code, addr} -> set_code(addr, code) end)
        sync()
      end

      # Gets the amount of defined segments
      def size() do
        Module.get_attribute(__MODULE__, :segment, []) |> length()
      end

      # Fills the entire display with a given character
      def fill(char) do
        (for _ <- 1..size(), do: char) |> Enum.join() |> text()
      end



      # Utility Module for calibration of the individual modules
      # defmodule Calibration do
      #   import unquote(__MODULE__)

      #   # Delegates to instance module definitions
      #   def calibrate(mode), do: send_command(unquote(device), :calbr1)


        # Calibrates the count of the flaps in the segment
        def calcount(), do: calibrate(:br1)

        @home_read_interval 1500
        @home_stop_interval 3000

        # Calibrates the homepoint of the segment
        def calhomeread(ri \\ @home_read_interval, si \\ @home_stop_interval) do
          calibrate(:br2)
          :timer.sleep(ri)
          read = code(0)
          :timer.sleep(si)
          calibrate(:stop)
          stop = code(0)
          Logger.warning "Home Calibration finished: #{inspect read} => #{inspect stop}"
        end

        # Calibrates the homepoint of the segment
        def calhome(interval) do
          calibrate(:br2)
          :timer.sleep(interval)
          calibrate(:stop)
        end

      # end

    end
  end

  @spec defsegment(keyword()) :: Macro.t()
  defmacro defsegment(opts) do
    pos = Keyword.fetch! opts, :position
    adr = Keyword.fetch! opts, :address
    tbl = Keyword.fetch! opts, :table
    quote do
      @segment {unquote(pos), unquote(adr), unquote(tbl)}
      def table(unquote(pos)), do: unquote(tbl)
      def address(unquote(pos)), do: unquote(adr)
    end
  end

end
