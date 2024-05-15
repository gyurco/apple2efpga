--
-- mist_top.vhd.vhd
--
-- Apple II+ toplevel for the MiST board
-- https://github.com/wsoltys/mist_apple2
--
-- Copyright (c) 2014 W. Soltys <wsoltys@gmail.com>
--
-- This source file is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This source file is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library mist;
use mist.mist.all;

entity apple2e_mist is
  generic
  (
    VGA_BITS   : integer := 6;
    BIG_OSD : boolean := false;
    HDMI : boolean := false;
    USE_AUDIO_IN : boolean := false;
    BUILD_DATE : string :=""
  );
  port (
    -- Clocks

    CLOCK_27    : in std_logic; -- 27 MHz


    -- SDRAM
    SDRAM_nCS : out std_logic; -- Chip Select
    SDRAM_DQ : inout std_logic_vector(15 downto 0); -- SDRAM Data bus 16 Bits
    SDRAM_A : out std_logic_vector(12 downto 0); -- SDRAM Address bus 13 Bits
    SDRAM_DQMH : out std_logic; -- SDRAM High Data Mask
    SDRAM_DQML : out std_logic; -- SDRAM Low-byte Data Mask
    SDRAM_nWE : out std_logic; -- SDRAM Write Enable
    SDRAM_nCAS : out std_logic; -- SDRAM Column Address Strobe
    SDRAM_nRAS : out std_logic; -- SDRAM Row Address Strobe
    SDRAM_BA : out std_logic_vector(1 downto 0); -- SDRAM Bank Address
    SDRAM_CLK : out std_logic; -- SDRAM Clock
    SDRAM_CKE: out std_logic; -- SDRAM Clock Enable
    
    -- SPI
    SPI_SCK : in std_logic;
    SPI_DI : in std_logic;
    SPI_DO : out std_logic;
    SPI_SS2 : in std_logic;
    SPI_SS3 : in std_logic;
    CONF_DATA0 : in std_logic;

    -- VGA output
    VGA_HS,                                             -- H_SYNC
    VGA_VS : out std_logic;                             -- V_SYNC
    VGA_R,                                              -- Red[x:0]
    VGA_G,                                              -- Green[x:0]
    VGA_B : out std_logic_vector(VGA_BITS-1 downto 0);  -- Blue[x:0]

    -- HDMI
    HDMI_R     : out   std_logic_vector(7 downto 0) := (others => '0');
    HDMI_G     : out   std_logic_vector(7 downto 0) := (others => '0');
    HDMI_B     : out   std_logic_vector(7 downto 0) := (others => '0');
    HDMI_HS    : out   std_logic := '0';
    HDMI_VS    : out   std_logic := '0';
    HDMI_DE    : out   std_logic := '0';
    HDMI_PCLK  : out   std_logic := '0';
    HDMI_SCL   : inout std_logic;
    HDMI_SDA   : inout std_logic;

    -- Audio
    AUDIO_L,
    AUDIO_R : out std_logic;
    I2S_BCK    : out   std_logic;
    I2S_LRCK   : out   std_logic;
    I2S_DATA   : out   std_logic;
    SPDIF_O    : out   std_logic;

    AUDIO_IN : in std_logic;

    -- UART

    UART_RX : in std_logic;
    UART_TX : out std_logic;

    -- LEDG
    LED : out std_logic

    );

end apple2e_mist;

architecture datapath of apple2e_mist is

  function SEP return string is
  begin
	  if BIG_OSD then return "-;"; else return ""; end if;
  end function;

  function USER_IO_FEAT return std_logic_vector is
  variable feat: std_logic_vector(31 downto 0);
  begin
    feat := x"00000000";
	  if BIG_OSD then feat := feat or x"00002000"; end if;
	  if HDMI    then feat := feat or x"00004000"; end if;
	  return feat;
  end function;

  function to_slv(s: string) return std_logic_vector is 
    constant ss: string(1 to s'length) := s; 
    variable rval: std_logic_vector(1 to 8 * s'length); 
    variable p: integer; 
    variable c: integer; 
  
  begin 
    for i in ss'range loop
      p := 8 * i;
      c := character'pos(ss(i));
      rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8)); 
    end loop; 
    return rval; 
  end function; 

  constant CONF_STR : string :=
   "AppleII;;"&
   "S0U,NIB,Load Disk 0;"&
   "S1U,NIB,Load Disk 1;"&
   SEP&
   "O89,Write Protect,None,Disk 0,Disk 1, Disk 0&1;"&
   "O1,CPU Type,6502,65C02;"&
   "O23,Monitor,Color,B&W,Green,Amber;"&
   "ODE,Color palette,//e,IIgs,AppleWin,apple2fpga;"&
   "O4,Machine Type,NTSC,PAL;"&
   "OBC,Scanlines,Off,25%,50%,75%;"&
   "O5,Joysticks,Normal,Swapped;"&
   "O6,Mockingboard S4,off,on;"&
   SEP&
   "T7,Cold reset;";

  component mist_sd_card
    port (
            sd_lba         : out std_logic_vector(31 downto 0);
            sd_rd          : out std_logic;
            sd_wr          : out std_logic;
            sd_ack         : in  std_logic;

            sd_buff_addr   : in  std_logic_vector(8 downto 0);
            sd_buff_dout   : in  std_logic_vector(7 downto 0);
            sd_buff_din    : out std_logic_vector(7 downto 0);
            sd_buff_wr     : in  std_logic;

            ram_addr       : in  unsigned(12 downto 0);
            ram_di         : in  unsigned( 7 downto 0);
            ram_do         : out unsigned( 7 downto 0);
            ram_we         : in  std_logic;

            change         : in  std_logic;                     -- Force reload as disk may have changed
            mount          : in  std_logic;                     -- umount(0)/mount(1)
            track          : in  std_logic_vector(5 downto 0);  -- Track number (0-34)
            busy           : out std_logic;
            ready          : out std_logic;
            active         : in  std_logic;

            clk            : in  std_logic;     -- System clock
            reset          : in  std_logic
        );
  end component mist_sd_card;

  component sdram is
    port( sd_data : inout std_logic_vector(15 downto 0);
          sd_addr : out std_logic_vector(12 downto 0);
          sd_dqm : out std_logic_vector(1 downto 0);
          sd_ba : out std_logic_vector(1 downto 0);
          sd_cs : out std_logic;
          sd_we : out std_logic;
          sd_ras : out std_logic;
          sd_cas : out std_logic;
          init_n : in std_logic;
          clk : in std_logic;
          clkref : in std_logic;
          din : in std_logic_vector(7 downto 0);
          dout : out std_logic_vector(15 downto 0);
          aux : in std_logic;
          addr : in std_logic_vector(24 downto 0);
          we : in std_logic
    );
  end component;

  component i2s
  generic (
    I2S_Freq   : integer := 48000;
    AUDIO_DW   : integer := 16
  );
  port
  (
    clk        : in    std_logic;
    reset      : in    std_logic;
    clk_rate   : in    integer;
    sclk       : out   std_logic;
    lrclk      : out   std_logic;
    sdata      : out   std_logic;
    left_chan  : in    std_logic_vector(AUDIO_DW-1 downto 0);
    right_chan : in    std_logic_vector(AUDIO_DW-1 downto 0)
  );
  end component i2s;

  component spdif port
  (
    clk_i      : in    std_logic;
    rst_i      : in    std_logic;
    clk_rate_i : in    integer;
    spdif_o    : out   std_logic;
    sample_i   : in    std_logic_vector(31 downto 0)
  );
  end component spdif;

  signal CLK_28M, CLK_14M, CLK_2M, CLK_2M_D, PHASE_ZERO, PHASE_ZERO_R, PHASE_ZERO_F : std_logic;
  signal clk_div : unsigned(1 downto 0);
  signal IO_SELECT, DEVICE_SELECT : std_logic_vector(7 downto 0);
  signal ADDR : unsigned(15 downto 0);
  signal D, PD: unsigned(7 downto 0);
  signal DISK_DO, PSG_DO : unsigned(7 downto 0);
  signal DO : std_logic_vector(15 downto 0);
  signal aux : std_logic;
  signal cpu_we : std_logic;
  signal psg_irq_n, psg_nmi_n : std_logic;

  signal we_ram : std_logic;
  signal VIDEO, HBL, VBL : std_logic;
  signal COLOR_LINE : std_logic;
  signal COLOR_LINE_CONTROL : std_logic;
  signal SCREEN_MODE : std_logic_vector(1 downto 0);
  signal COLOR_PALETTE : std_logic_vector(1 downto 0);
  signal GAMEPORT : std_logic_vector(7 downto 0);
  signal scandoubler_disable : std_logic;
  signal ypbpr : std_logic;
  signal no_csync : std_logic;

  signal K : unsigned(7 downto 0);
  signal read_key : std_logic;
  signal akd : std_logic;

  signal flash_clk : unsigned(22 downto 0) := (others => '0');
  signal power_on_reset : std_logic := '1';
  signal reset : std_logic;

  signal D1_ACTIVE, D2_ACTIVE : std_logic;
  signal TRACK1_RAM_BUSY : std_logic;
  signal TRACK1_RAM_ADDR : unsigned(12 downto 0);
  signal TRACK1_RAM_DI : unsigned(7 downto 0);
  signal TRACK1_RAM_DO : unsigned(7 downto 0);
  signal TRACK1_RAM_WE : std_logic;
  signal TRACK1 : unsigned(5 downto 0);
  signal TRACK2_RAM_BUSY : std_logic;
  signal TRACK2_RAM_ADDR : unsigned(12 downto 0);
  signal TRACK2_RAM_DI : unsigned(7 downto 0);
  signal TRACK2_RAM_DO : unsigned(7 downto 0);
  signal TRACK2_RAM_WE : std_logic;
  signal TRACK2 : unsigned(5 downto 0);
  signal DISK_READY : std_logic_vector(1 downto 0);
  signal disk_change : std_logic_vector(1 downto 0);
  signal disk_size : std_logic_vector(63 downto 0);
  signal disk_mount : std_logic;

  signal downl : std_logic := '0';
  signal io_index : std_logic_vector(4 downto 0);
  signal size : std_logic_vector(24 downto 0) := (others=>'0');
  signal a_ram: unsigned(15 downto 0);
  signal r : unsigned(7 downto 0);
  signal g : unsigned(7 downto 0);
  signal b : unsigned(7 downto 0);
  signal blank : std_logic;
  signal hsync : std_logic;
  signal vsync : std_logic;
  signal sd_we : std_logic;
  signal sd_oe : std_logic;
  signal sd_addr : std_logic_vector(18 downto 0);
  signal sd_di : std_logic_vector(7 downto 0);
  signal sd_do : std_logic_vector(7 downto 0);
  signal io_we : std_logic;
  signal io_addr : std_logic_vector(24 downto 0);
  signal io_do : std_logic_vector(7 downto 0);
  signal io_ram_we : std_logic;
  signal io_ram_d : std_logic_vector(7 downto 0);
  signal io_ram_addr : std_logic_vector(18 downto 0);
  signal ram_we : std_logic;
  signal ram_di : std_logic_vector(7 downto 0);
  signal ram_addr : std_logic_vector(24 downto 0);

  signal i2c_start : std_logic;
  signal i2c_read : std_logic;
  signal i2c_addr : std_logic_vector(6 downto 0);
  signal i2c_subaddr : std_logic_vector(7 downto 0);
  signal i2c_wdata : std_logic_vector(7 downto 0);
  signal i2c_rdata : std_logic_vector(7 downto 0);
  signal i2c_end : std_logic;
  signal i2c_ack : std_logic;
  
  signal switches   : std_logic_vector(1 downto 0);
  signal buttons    : std_logic_vector(1 downto 0);
  signal joy        : std_logic_vector(5 downto 0);
  signal joy0       : std_logic_vector(31 downto 0);
  signal joy1       : std_logic_vector(31 downto 0);
  signal joy_an0    : std_logic_vector(31 downto 0);
  signal joy_an1    : std_logic_vector(31 downto 0);
  signal joy_an     : std_logic_vector(15 downto 0);
  signal status     : std_logic_vector(63 downto 0);
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;

  signal st_wp      : std_logic_vector( 1 downto 0);
  
  signal psg_audio_l : unsigned(9 downto 0);
  signal psg_audio_r : unsigned(9 downto 0);
  signal audio       : std_logic;

  -- signals to connect sd card emulation with io controller
  signal sd_lba:  std_logic_vector(31 downto 0);
  signal sd_rd:   std_logic_vector(1 downto 0) := (others => '0');
  signal sd_wr:   std_logic_vector(1 downto 0) := (others => '0');
  signal sd_ack:  std_logic_vector(1 downto 0);

  signal SD_LBA1:  std_logic_vector(31 downto 0);
  signal SD_LBA2:  std_logic_vector(31 downto 0);
  
  -- data from io controller to sd card emulation
  signal sd_data_in: std_logic_vector(7 downto 0);
  signal sd_data_out: std_logic_vector(7 downto 0);
  signal sd_data_out_strobe:  std_logic;
  signal sd_buff_addr: std_logic_vector(8 downto 0);

  signal SD_DATA_IN1: std_logic_vector(7 downto 0);
  signal SD_DATA_IN2: std_logic_vector(7 downto 0);
  
  -- sd card emulation
  signal sd_cs:	std_logic;
  signal sd_sck:	std_logic;
  signal sd_sdi:	std_logic;
  signal sd_sdo:	std_logic;
  
  signal pll_locked : std_logic;
  signal sdram_dqm: std_logic_vector(1 downto 0);
  signal joyx       : std_logic;
  signal joyy       : std_logic;
  signal pdl_strobe : std_logic;
  signal open_apple : std_logic;
  signal closed_apple : std_logic;

  signal ear_in : std_logic;

begin

  st_wp <= status(9 downto 8);

  -- In the Apple ][, this was a 555 timer
  power_on : process(CLK_14M)
  begin
    if rising_edge(CLK_14M) then
      reset <= status(0) or power_on_reset;

      if buttons(1)='1' or status(7) = '1' then
        power_on_reset <= '1';
        flash_clk <= (others=>'0');
      else
		  if flash_clk(22) = '1' then
          power_on_reset <= '0';
			end if;
			 
        flash_clk <= flash_clk + 1;
      end if;
    end if;
  end process;
  
  SDRAM_CLK <= CLK_28M;
  
  pll : entity work.mist_clk 
  port map (
    areset => '0',
    inclk0 => CLOCK_27,
    c0     => CLK_28M,
    c1     => CLK_14M,
    locked => pll_locked
    );

 
  -- Paddle buttons
  -- GAMEPORT input bits:
  --  7    6    5    4    3   2   1    0
  -- pdl3 pdl2 pdl1 pdl0 pb3 pb2 pb1 casette
	ear_in <= AUDIO_IN when USE_AUDIO_IN else UART_RX;
  GAMEPORT <=  "00" & joyy & joyx & "0" & (joy(5) or closed_apple) & (joy(4) or open_apple) & ear_in;
  
  joy_an <= joy_an0(15 downto 0) when status(5)='0' else joy_an1(15 downto 0);
  joy <= joy0(5 downto 0) when status(5)='0' else joy1(5 downto 0);
  
  process(CLK_14M, pdl_strobe)
    variable cx, cy : integer range -100 to 5800 := 0;
  begin
    if rising_edge(CLK_14M) then
     CLK_2M_D <= CLK_2M;
     if CLK_2M_D = '0' and CLK_2M = '1' then
      if cx > 0 then
        cx := cx -1;
        joyx <= '1';
      else
        joyx <= '0';
      end if;
      if cy > 0 then
        cy := cy -1;
        joyy <= '1';
      else
        joyy <= '0';
      end if;
      if pdl_strobe = '1' then
        cx := 2800+(22*to_integer(signed(joy_an(15 downto 8))));
        cy := 2800+(22*to_integer(signed(joy_an(7 downto 0)))); -- max 5650
        if cx < 0 then
          cx := 0;
        elsif cx >= 5590 then
          cx := 5650;
        end if;
        if cy < 0 then
          cy := 0;
        elsif cy >= 5590 then
          cy := 5650;
        end if;
      end if;
     end if;
    end if;
  end process;

  COLOR_LINE_CONTROL <= COLOR_LINE and not (status(2) or status(3));  -- Color or B&W mode
  SCREEN_MODE <= status(3 downto 2); -- 00: Color, 01: B&W, 10:Green, 11: Amber
  COLOR_PALETTE <= status(14 downto 13);
  
  -- sdram interface
  SDRAM_CKE <= '1';
  SDRAM_DQMH <= sdram_dqm(1);
  SDRAM_DQML <= sdram_dqm(0);

  sdram_inst : sdram
    port map( sd_data => SDRAM_DQ,
              sd_addr => SDRAM_A,
              sd_dqm => sdram_dqm,
              sd_cs => SDRAM_nCS,
              sd_ba => SDRAM_BA,
              sd_we => SDRAM_nWE,
              sd_ras => SDRAM_nRAS,
              sd_cas => SDRAM_nCAS,
              clk => CLK_28M,
              clkref => CLK_2M,
              init_n => pll_locked,
              din => ram_di,
              addr => ram_addr,
              we => ram_we,
              dout => DO,
              aux => aux
    );
  
  -- Simulate power up on cold reset to go to the disk boot routine
  ram_we   <= we_ram when status(7) = '0' else '1';
  ram_addr <= "000000000" & std_logic_vector(a_ram) when status(7) = '0' else std_logic_vector(to_unsigned(1012,ram_addr'length)); -- $3F4
  ram_di   <= std_logic_vector(D) when status(7) = '0' else "00000000";

  PD <= PSG_DO when IO_SELECT(4) = '1' else DISK_DO;

  core : entity work.apple2 port map (
    CLK_14M        => CLK_14M,
    PALMODE        => status(4),
    CLK_2M         => CLK_2M,
    PHASE_ZERO     => PHASE_ZERO,
    PHASE_ZERO_R   => PHASE_ZERO_R,
    PHASE_ZERO_F   => PHASE_ZERO_F,
    FLASH_CLK      => flash_clk(22),
    reset          => reset,
    cpu            => status(1),
    ADDR           => ADDR,
    ram_addr       => a_ram,
    D              => D,
    ram_do         => unsigned(DO),
    aux            => aux,
    PD             => PD,
    CPU_WE         => cpu_we,
    IRQ_N          => psg_irq_n,
    NMI_N          => psg_nmi_n,
    ram_we         => we_ram,
    VIDEO          => VIDEO,
    COLOR_LINE     => COLOR_LINE,
    HBL            => HBL,
    VBL            => VBL,
    K              => K,
    KEYSTROBE      => read_key,
    AKD            => akd,
    AN             => open,
    GAMEPORT       => GAMEPORT,
    PDL_strobe     => pdl_strobe,
    IO_SELECT      => IO_SELECT,
    DEVICE_SELECT  => DEVICE_SELECT,
    speaker        => audio
    );

  tv : entity work.tv_controller port map (
    CLK_14M    => CLK_14M,
    VIDEO      => VIDEO,
    COLOR_LINE => COLOR_LINE_CONTROL,
    SCREEN_MODE => SCREEN_MODE,
    COLOR_PALETTE => COLOR_PALETTE,
    HBL        => HBL,
    VBL        => VBL,
    VGA_CLK    => open,
    VGA_HS     => hsync,
    VGA_VS     => vsync,
    VGA_BLANK  => blank,
    VGA_R      => r,
    VGA_G      => g,
    VGA_B      => b
    );

  keyboard : entity work.keyboard port map (
    PS2_Clk  => ps2Clk,
    PS2_Data => ps2Data,
    CLK_14M  => CLK_14M,
    reset    => reset,
    reads    => read_key,
    K        => K,
    akd      => akd,
    open_apple => open_apple,
    closed_apple => closed_apple
    );

  disk : entity work.disk_ii port map (
    CLK_14M        => CLK_14M,
    CLK_2M         => CLK_2M,
    PHASE_ZERO     => PHASE_ZERO,
    IO_SELECT      => IO_SELECT(6),
    DEVICE_SELECT  => DEVICE_SELECT(6),
    RESET          => reset,
    DISK_READY     => DISK_READY,
    A              => ADDR,
    D_IN           => D,
    D_OUT          => DISK_DO,
    D1_ACTIVE      => D1_ACTIVE,
    D2_ACTIVE      => D2_ACTIVE,
    WP             => st_wp,
    -- track buffer interface for disk 1
    TRACK1         => TRACK1,
    TRACK1_ADDR    => TRACK1_RAM_ADDR,
    TRACK1_DO      => TRACK1_RAM_DO,
    TRACK1_DI      => TRACK1_RAM_DI,
    TRACK1_WE      => TRACK1_RAM_WE,
    TRACK1_BUSY    => TRACK1_RAM_BUSY,
    -- track buffer interface for disk 2
    TRACK2         => TRACK2,
    TRACK2_ADDR    => TRACK2_RAM_ADDR,
    TRACK2_DO      => TRACK2_RAM_DO,
    TRACK2_DI      => TRACK2_RAM_DI,
    TRACK2_WE      => TRACK2_RAM_WE,
    TRACK2_BUSY    => TRACK2_RAM_BUSY
    );

  disk_mount <= '0' when disk_size = x"0000000000000000" else '1';
  sd_lba <= SD_LBA2 when sd_rd(1) = '1' or sd_wr(1) = '1' else SD_LBA1;
  sd_data_in <= SD_DATA_IN2 when sd_ack(1) = '1' else SD_DATA_IN1;
  
  sdcard_interface1: mist_sd_card port map (
    clk          => CLK_14M,
    reset        => reset,

    ram_addr     => TRACK1_RAM_ADDR, -- in unsigned(12 downto 0);
    ram_di       => TRACK1_RAM_DI,   -- in unsigned(7 downto 0);
    ram_do       => TRACK1_RAM_DO,   -- out unsigned(7 downto 0);
    ram_we       => TRACK1_RAM_WE,

    track        => std_logic_vector(TRACK1),
    busy         => TRACK1_RAM_BUSY,
    change       => DISK_CHANGE(0),
    mount        => disk_mount,
    ready        => DISK_READY(0),
    active       => D1_ACTIVE,

    sd_buff_addr => sd_buff_addr,
    sd_buff_dout => sd_data_out,
    sd_buff_din  => SD_DATA_IN1,
    sd_buff_wr   => sd_data_out_strobe,

    sd_lba       => SD_LBA1,
    sd_rd        => sd_rd(0),
    sd_wr        => sd_wr(0),
    sd_ack       => sd_ack(0)
  );

  sdcard_interface2: mist_sd_card port map (
    clk          => CLK_14M,
    reset        => reset,

    ram_addr     => TRACK2_RAM_ADDR, -- in unsigned(12 downto 0);
    ram_di       => TRACK2_RAM_DI,   -- in unsigned(7 downto 0);
    ram_do       => TRACK2_RAM_DO,   -- out unsigned(7 downto 0);
    ram_we       => TRACK2_RAM_WE,

    track        => std_logic_vector(TRACK2),
    busy         => TRACK2_RAM_BUSY,
    change       => DISK_CHANGE(1),
    mount        => disk_mount,
    ready        => DISK_READY(1),
    active       => D2_ACTIVE,

    sd_buff_addr => sd_buff_addr,
    sd_buff_dout => sd_data_out,
    sd_buff_din  => SD_DATA_IN2,
    sd_buff_wr   => sd_data_out_strobe,

    sd_lba       => SD_LBA2,
    sd_rd        => sd_rd(1),
    sd_wr        => sd_wr(1),
    sd_ack       => sd_ack(1)
  );

  LED <= not (D1_ACTIVE or D2_ACTIVE);

  mb : work.mockingboard
    port map (
      CLK_14M    => CLK_14M,
      PHASE_ZERO => PHASE_ZERO,
      PHASE_ZERO_R => PHASE_ZERO_R,
      PHASE_ZERO_F => PHASE_ZERO_F,
      I_RESET_L => not reset,
      I_ENA_H   => status(6),

      I_ADDR    => std_logic_vector(ADDR)(7 downto 0),
      I_DATA    => std_logic_vector(D),
      unsigned(O_DATA)    => PSG_DO,
      I_RW_L    => not cpu_we,
      I_IOSEL_L => not IO_SELECT(4),
      O_IRQ_L   => psg_irq_n,
      O_NMI_L   => psg_nmi_n,
      unsigned(O_AUDIO_L) => psg_audio_l,
      unsigned(O_AUDIO_R) => psg_audio_r
      );

  dac_l : mist.dac
    generic map(10)
    port map (
      clk_i		=> CLK_14M,
      res_n_i	=> not reset,
      dac_i 	=> std_logic_vector(psg_audio_l + (audio & "0000000")),
      dac_o 	=> AUDIO_L
      );

  dac_r : mist.dac
    generic map(10)
    port map (
      clk_i		=> CLK_14M,
      res_n_i	=> not reset,
      dac_i 	=> std_logic_vector(psg_audio_r + (audio & "0000000")),
      dac_o 	=> AUDIO_R
      );

  my_i2s : i2s
  port map (
    clk => CLK_28M,
    reset => '0',
    clk_rate => 28_600_000,
    sclk => I2S_BCK,
    lrclk => I2S_LRCK,
    sdata => I2S_DATA,
    left_chan  => '0'&std_logic_vector(psg_audio_l + (audio & "0000000"))&"00000",
    right_chan => '0'&std_logic_vector(psg_audio_r + (audio & "0000000"))&"00000"
  );

  my_spdif : spdif
  port map (
    rst_i => '0',
    clk_i => CLK_28M,
    clk_rate_i => 28_600_000,
    spdif_o => SPDIF_O,
    sample_i => '0'&std_logic_vector(psg_audio_r + (audio & "0000000"))&"00000" & '0'&std_logic_vector(psg_audio_l + (audio & "0000000"))&"00000"
  );

  user_io_inst : user_io
    generic map (
      STRLEN => CONF_STR'length,
      FEATURES => USER_IO_FEAT
    )
    port map (
      clk_sys => CLK_14M,
      clk_sd => CLK_14M,
      SPI_CLK => SPI_SCK,
      SPI_SS_IO => CONF_DATA0,    
      SPI_MISO => SPI_DO,    
      SPI_MOSI => SPI_DI,       
      conf_str => to_slv(CONF_STR),
      status => status,   
      joystick_0 => joy0,   
      joystick_1 => joy1,
      joystick_analog_0 => joy_an0,
      joystick_analog_1 => joy_an1,
      SWITCHES => switches,
      BUTTONS => buttons,
      scandoubler_disable => scandoubler_disable,
      ypbpr => ypbpr,
      no_csync => no_csync,

      i2c_start => i2c_start,
      i2c_read => i2c_read,
      i2c_addr => i2c_addr,
      i2c_subaddr => i2c_subaddr,
      i2c_dout => i2c_wdata,
      i2c_din => i2c_rdata,
      i2c_end => i2c_end,
      i2c_ack => i2c_ack,

      -- connection to io controller
      sd_lba  => sd_lba,
      sd_rd   => sd_rd,
      sd_wr   => sd_wr,
      sd_ack_x => sd_ack,
      sd_ack_conf => open,
      sd_sdhc => '1',
      sd_conf => '0',
      sd_dout => sd_data_out,
      sd_dout_strobe => sd_data_out_strobe,
      sd_din => sd_data_in,
      sd_buff_addr => sd_buff_addr,
      img_mounted => disk_change,
      img_size => disk_size,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );

 vga_video : mist_video
    generic map(
      COLOR_DEPTH => 8,
      SD_HCNT_WIDTH => 10,
      OUT_COLOR_DEPTH => VGA_BITS,
      BIG_OSD => BIG_OSD
    )
    port map (
      clk_sys => CLK_28M,
      scanlines   => status(12 downto 11),
      ce_divider => "001",
      scandoubler_disable => scandoubler_disable,
      ypbpr => ypbpr,
      no_csync => no_csync,
      rotate => "00",

      SPI_DI => SPI_DI,
      SPI_SCK => SPI_SCK,
      SPI_SS3 => SPI_SS3,

      R => std_logic_vector(r),
      G => std_logic_vector(g),
      B => std_logic_vector(b),
      HSync => hsync,
      VSync => vsync,
      VGA_HS => VGA_HS,
      VGA_VS => VGA_VS,
      VGA_R  => VGA_R,
      VGA_G  => VGA_G,
      VGA_B  => VGA_B
    );

hdmi_block : if HDMI generate

  i2c_master_d : i2c_master
  generic map (
    CLK_Freq => 28000000
  )
  port map (
    CLK => CLK_28M,
    I2C_START => i2c_start,
    I2C_READ => i2c_read,
    I2C_ADDR => i2c_addr,
    I2C_SUBADDR => i2c_subaddr,
    I2C_WDATA => i2c_wdata,
    I2C_RDATA => i2c_rdata,
    I2C_END => i2c_end,
    I2C_ACK => i2c_ack,
    I2C_SCL => HDMI_SCL,
    I2C_SDA => HDMI_SDA
  );

  hdmi_video : mist_video
  generic map (
    SD_HCNT_WIDTH => 10,
    COLOR_DEPTH => 8,
    OSD_COLOR => "011",
    USE_BLANKS => true,
    OUT_COLOR_DEPTH => 8,
    BIG_OSD => BIG_OSD,
    VIDEO_CLEANER => true
  )
  port map (
    clk_sys => CLK_28M,
    scanlines   => status(12 downto 11),
    ce_divider => "001",
    scandoubler_disable => '0',
    ypbpr => '0',
    no_csync => '1',
    rotate => "00",

    SPI_DI => SPI_DI,
    SPI_SCK => SPI_SCK,
    SPI_SS3 => SPI_SS3,

    R => std_logic_vector(r),
    G => std_logic_vector(g),
    B => std_logic_vector(b),
    HBlank => blank,
    VBlank => not vsync,
    HSync => hsync,
    VSync => vsync,
    VGA_HS => HDMI_HS,
    VGA_VS => HDMI_VS,
    VGA_R  => HDMI_R,
    VGA_G  => HDMI_G,
    VGA_B  => HDMI_B,
    VGA_DE => HDMI_DE
  );

  HDMI_PCLK <= CLK_28M;
end generate;

end datapath;