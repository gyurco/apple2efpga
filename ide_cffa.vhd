-------------------------------------------------------------------------------
--
-- CFFA compatible IDE interface
--
-- (c)2025 Gyorgy Szombathelyi
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ide_cffa is
  port (
    CLK_28M        : in  std_logic;
    PHASE_ZERO     : in  std_logic;
    IO_SELECT      : in  std_logic;             -- e.g., C700 - C7FF ROM
    IO_STROBE      : in  std_logic;             -- e.g., C800 - CFFF I/O locations
    DEVICE_SELECT  : in  std_logic;
    RESET          : in  std_logic;
    A              : in  unsigned(15 downto 0);
    D_IN           : in  unsigned( 7 downto 0); -- From 6502
    D_OUT          : out unsigned( 7 downto 0); -- To 6502
    RNW            : in  std_logic;
    OE             : out std_logic;
    -- IDE interface
    IDE_CS         : out std_logic;
    IDE_ADDR       : out std_logic_vector( 2 downto 0);
    IDE_DOUT       : in  std_logic_vector(15 downto 0);
    IDE_DIN        : out std_logic_vector(15 downto 0);

    -- EEPROM save/load
    SAVE_A         : in  unsigned(10 downto 0);
    SAVE_D         : in  unsigned( 7 downto 0);
    SAVE_WE        : in  std_logic;
    SAVE_Q         : out unsigned( 7 downto 0)
    );
end ide_cffa;

architecture rtl of ide_cffa is

  signal rom_dout : unsigned(7 downto 0);
  signal rom_active : std_logic;
  signal eeprom_we : std_logic;
  signal wprotect: std_logic;

  signal internal_ide_dout : std_logic_vector(7 downto 0);
  signal data_in_latch : std_logic_vector(7 downto 0);
  signal data_out_latch : std_logic_vector(7 downto 0);

begin

  process(CLK_28M) begin
    if rising_edge(CLK_28M) then
      if RESET = '1' then
        rom_active <= '0';
      else
        if IO_STROBE = '1' and A(11 downto 0) = x"FFF" then
          rom_active <= '0';
        elsif IO_SELECT = '1' then
          rom_active <= '1';
        end if;
      end if;
    end if;
  end process;

  process(CLK_28M) begin
    if rising_edge(CLK_28M) then
      if RESET = '1' then
        wprotect <= '1';
      else
        -- W_HOST  = !/DSEL & !(A3 # A2 # A1 # A0) & !/RW;
        if DEVICE_SELECT = '1' and A(3 downto 0) = "0000" and RNW = '0' then
          data_out_latch <= std_logic_vector(D_IN);
        end if;
        -- R_ATA   = !/DSEL & (A3 # (A2 & A1)) & /RW;
        if DEVICE_SELECT = '1' and (A(3) = '1' or (A(2 downto 1) = "11")) and RNW = '1' then
          data_in_latch <= IDE_DOUT(7 downto 0);
        end if;
        if DEVICE_SELECT = '1' and A(3 downto 0) = "0011" and RNW = '1' then
          wprotect <= '0';
        end if;
        if DEVICE_SELECT = '1' and A(3 downto 0) = "0100" and RNW = '1' then
          wprotect <= '1';
        end if;
      end if;
    end if;
  end process;

  IDE_ADDR <= std_logic_vector(A(2 downto 0)) when A(3) = '1' else "111";
  IDE_CS <= '1' when DEVICE_SELECT = '1' and (A(3) = '1' or A(2 downto 1) = "11") else '0';
  IDE_DIN <= std_logic_vector(D_IN) & data_out_latch;

  internal_ide_dout <= data_in_latch when A(3 downto 0) = "0000" else IDE_DOUT(15 downto 8);

  D_OUT <= unsigned(internal_ide_dout) when DEVICE_SELECT = '1' else rom_dout;
  OE <= (IO_STROBE and rom_active) or IO_SELECT or DEVICE_SELECT;
  eeprom_we <= '1' when RNW = '0' and IO_STROBE = '1' and rom_active = '1' and A(10 downto 5) = 0 and wprotect = '0' else '0';

  -- ROM
  rom : entity work.ide_cffa_rom port map (
    addr => A(11 downto 0),
    clk  => CLK_28M,
    data => rom_dout,
    we => eeprom_we,
    w_d => D_IN,

    addr_b => '1'&SAVE_A,
    data_b => SAVE_Q,
    we_b => SAVE_WE,
    w_d_b => SAVE_D);

end rtl;
