--
-- Mockingboard clone for the Apple II
-- Model A: two AY-3-8913 chips for six audio channels
--
-- Top file by W. Soltys <wsoltys@gmail.com>
-- 
-- loosely based on:
-- http://www.downloads.reactivemicro.com/Public/Apple%20II%20Items/Hardware/Mockingboard_v1/Mockingboard-v1a-Docs.pdf
-- http://www.applelogic.org/CarteBlancheIIProj6.html
--

library ieee ;
  use ieee.std_logic_1164.all ;
--  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;
  
  
entity MOCKINGBOARD is
  port (
    CLK_14M           : in std_logic;
    PHASE_ZERO        : in std_logic;
    PHASE_ZERO_R      : in std_logic;
    PHASE_ZERO_F      : in std_logic;
    I_ADDR            : in std_logic_vector(7 downto 0);
    I_DATA            : in std_logic_vector(7 downto 0);
    O_DATA            : out std_logic_vector(7 downto 0);
    
    I_RW_L            : in std_logic;
    O_IRQ_L           : out std_logic;
    O_NMI_L           : out std_logic;
    I_IOSEL_L         : in std_logic;
    I_RESET_L         : in std_logic;
    I_ENA_H           : in std_logic;     
    
    O_AUDIO_L         : out std_logic_vector(9 downto 0);
    O_AUDIO_R         : out std_logic_vector(9 downto 0)
    );
 end;
 
 
 architecture RTL of MOCKINGBOARD is
 
  signal o_pb_l           : std_logic_vector(7 downto 0);
  signal o_pb_r           : std_logic_vector(7 downto 0);
  
  signal i_psg_r          : std_logic_vector(7 downto 0);
  signal o_psg_r          : std_logic_vector(7 downto 0);
  signal i_psg_l          : std_logic_vector(7 downto 0);
  signal o_psg_l          : std_logic_vector(7 downto 0);

  signal o_data_l          : std_logic_vector(7 downto 0);
  signal o_data_r          : std_logic_vector(7 downto 0);
  
  signal lirq             : std_logic;
  signal rirq             : std_logic;
  
  signal PSG_EN   : std_logic;
  signal VIA_CE_F, VIA_CE_R : std_logic;

begin

  O_DATA <= o_data_l when I_ADDR(7) = '0' else o_data_r;
	-- Both VIAs generate IRQ, not NMI!
  O_IRQ_L <= (not lirq and not rirq) or not I_ENA_H;
  O_NMI_L <= '1';

  PSG_EN <= PHASE_ZERO_F;
  VIA_CE_R <= PHASE_ZERO_F;
  VIA_CE_F <= PHASE_ZERO_R;

-- Left Channel Combo
  m6522_left : work.via6522
    port map (
      clock       => clk_14M,
      rising      => VIA_CE_R,
      falling     => VIA_CE_F,
      reset       => not I_RESET_L,

      addr        => I_ADDR(3 downto 0),
      wen         => not I_RW_L and not I_ADDR(7) and not I_IOSEL_L and I_ENA_H,
      ren         => I_RW_L and not I_ADDR(7) and not I_IOSEL_L and I_ENA_H,
      data_in     => I_DATA,
      data_out    => o_data_l,

      phi2_ref    => open,

      port_a_o    => i_psg_l,
      port_a_t    => open,
      port_a_i    => o_psg_l,

      port_b_o    => o_pb_l,
      port_b_t    => open,
      port_b_i    => (others => '1'),

      ca1_i       => '1',
      ca2_o       => open,
      ca2_i       => '1',
      cb1_o       => open,
      cb1_i       => '1',
      cb1_t       => open,
      cb2_o       => open,
      cb2_i       => '1',
      cb2_t       => open,
      irq         => lirq
      );

  psg_left: entity work.YM2149
    port map(
      -- data bus
      I_DA       => i_psg_l,            -- in  std_logic_vector(7 downto 0); -- pin 37 to 30
      O_DA       => o_psg_l,            -- out std_logic_vector(7 downto 0); -- pin 37 to 30
      O_DA_OE_L  => open,               -- out std_logic;
      -- control
      I_A9_L     => '0',                -- in  std_logic; -- pin 24
      I_A8       => '1',                -- in  std_logic; -- pin 25
      I_BDIR     => o_pb_l(1),          -- in  std_logic; -- pin 27
      I_BC2      => '1',                -- in  std_logic; -- pin 28
      I_BC1      => o_pb_l(0),          -- in  std_logic; -- pin 29
      I_SEL_L    => '1',                -- in  std_logic;

      O_AUDIO_L  => O_AUDIO_L,          -- out std_logic_vector(7 downto 0);

      -- port a
      I_IOA      => (others => '0'),    -- in  std_logic_vector(7 downto 0); -- pin 21 to 14
      O_IOA      => open,               -- out std_logic_vector(7 downto 0); -- pin 21 to 14
      O_IOA_OE_L => open,               -- out std_logic;
      -- port b
      I_IOB      => (others => '0'),    -- in  std_logic_vector(7 downto 0); -- pin 13 to 6
      O_IOB      => open,               -- out std_logic_vector(7 downto 0); -- pin 13 to 6
      O_IOB_OE_L => open,               -- out std_logic;

      ENA        => PSG_EN and I_ENA_H, -- in  std_logic; -- clock enable for higher speed operation
      RESET_L    => o_pb_l(2),          -- in  std_logic;
      CLK        => CLK_14M             -- in  std_logic
    );

-- Right Channel Combo
  m6522_right : work.via6522
    port map (
      clock       => clk_14M,
      rising      => VIA_CE_R,
      falling     => VIA_CE_F,
      reset       => not I_RESET_L,

      addr        => I_ADDR(3 downto 0),
      wen         => not I_RW_L and I_ADDR(7) and not I_IOSEL_L and I_ENA_H,
      ren         => I_RW_L and I_ADDR(7) and not I_IOSEL_L and I_ENA_H,
      data_in     => I_DATA,
      data_out    => o_data_r,

      phi2_ref    => open,

      port_a_o    => i_psg_r,
      port_a_t    => open,
      port_a_i    => o_psg_r,

      port_b_o    => o_pb_r,
      port_b_t    => open,
      port_b_i    => (others => '1'),

      ca1_i       => '1',
      ca2_o       => open,
      ca2_i       => '1',
      cb1_o       => open,
      cb1_i       => '1',
      cb1_t       => open,
      cb2_o       => open,
      cb2_i       => '1',
      cb2_t       => open,
      irq         => rirq
      );

  psg_right: entity work.YM2149
    port map(
      -- data bus
      I_DA       => i_psg_r,            -- in  std_logic_vector(7 downto 0); -- pin 37 to 30
      O_DA       => o_psg_r,            -- out std_logic_vector(7 downto 0); -- pin 37 to 30
      O_DA_OE_L  => open,               -- out std_logic;
      -- control
      I_A9_L     => '0',                -- in  std_logic; -- pin 24
      I_A8       => '1',                -- in  std_logic; -- pin 25
      I_BDIR     => o_pb_r(1),          -- in  std_logic; -- pin 27
      I_BC2      => '1',                -- in  std_logic; -- pin 28
      I_BC1      => o_pb_r(0),          -- in  std_logic; -- pin 29
      I_SEL_L    => '1',                -- in  std_logic;

      O_AUDIO_L  => O_AUDIO_R,          -- out std_logic_vector(7 downto 0);

      -- port a
      I_IOA      => (others => '0'),    -- in  std_logic_vector(7 downto 0); -- pin 21 to 14
      O_IOA      => open,               -- out std_logic_vector(7 downto 0); -- pin 21 to 14
      O_IOA_OE_L => open,               -- out std_logic;
      -- port b
      I_IOB      => (others => '0'),    -- in  std_logic_vector(7 downto 0); -- pin 13 to 6
      O_IOB      => open,               -- out std_logic_vector(7 downto 0); -- pin 13 to 6
      O_IOB_OE_L => open,               -- out std_logic;

      ENA        => PSG_EN and I_ENA_H, -- in  std_logic; -- clock enable for higher speed operation
      RESET_L    => o_pb_r(2),          -- in  std_logic;
      CLK        => CLK_14M             -- in  std_logic
    );
end architecture RTL;