-------------------------------------------------------------------------------
--
-- 15 kHz TV output for Apple //e
-- Szombathelyi György
--
-- based on vga_controller by
-- Stephen A. Edwards, sedwards@cs.columbia.edu
--
--
-- FIXME: This is all wrong
--
-- The Apple ][ uses a 14.31818 MHz master clock.  It outputs a new
-- horizontal line every 65 * 14 + 2 = 912 14M cycles.  The extra two
-- are from the "extended cycle" used to keep the 3.579545 MHz
-- colorburst signal in sync.  Of these, 40 * 14 = 560 are active video.
--
-- In graphics mode, the Apple effectively generates 140 four-bit pixels
-- output serially (i.e., with 3.579545 MHz pixel clock).  In text mode,
-- it generates 280 one-bit pixels (i.e., with a 7.15909 MHz pixel clock).
--
-- We capture 140 four-bit nibbles for each line and interpret them in
-- one of the two modes.  In graphics mode, each is displayed as a
-- single pixel of one of 16 colors.  In text mode, each is displayed
-- as two black or white pixels.
-- 
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tv_controller is
  
  port (
    clk_14m    : in  std_logic;	     -- 14.31818 MHz master clock

    VIDEO      : in std_logic;         -- from the Apple video generator
    COLOR_LINE : in std_logic;
    SCREEN_MODE: in std_logic_vector(1 downto 0); -- 00: Color, 01: B&W, 10: Green, 11: Amber
    HBL        : in std_logic;
    VBL        : in std_logic;

    VGA_CLK    : out std_logic;
    VGA_HS     : out std_logic;             -- Active low
    VGA_VS     : out std_logic;             -- Active low
    VGA_BLANK  : out std_logic;
    VGA_R      : out unsigned(7 downto 0);
    VGA_G      : out unsigned(7 downto 0);
    VGA_B      : out unsigned(7 downto 0)
    );
  
end tv_controller;

architecture rtl of tv_controller is

  -- RGB values from Linards Ticmanis,
  -- http://newsgroups.derkeiler.com/Archive/Comp/comp.sys.apple2/2005-09/msg00534.html

  type basis_color is array(0 to 3) of unsigned(7 downto 0);
  constant basis_r : basis_color := ( X"88", X"38", X"07", X"38" );
  constant basis_g : basis_color := ( X"22", X"24", X"67", X"52" );
  constant basis_b : basis_color := ( X"2C", X"A0", X"2C", X"07" );

  signal shift_reg : unsigned(5 downto 0);  -- Last six pixels

  signal last_hbl : std_logic;
  signal last_vbl : std_logic;
  signal hcount : unsigned(10 downto 0);
  signal vcount : unsigned(8 downto 0);
  signal hactive : std_logic;
  signal vactive : std_logic;

  constant VGA_SCANLINE : integer := 456*2; -- Must be 456*2 (set by the Apple)
  
  constant VGA_HSYNC : integer := 64;
  constant VGA_BACK_PORCH : integer := 158;
  constant VGA_ACTIVE : integer := 282 * 2;
  constant VGA_FRONT_PORCH : integer := 130;

  -- VGA_HSYNC + VGA_BACK_PORCH + VGA_ACTIVE + VGA_FRONT_PORCH = VGA_SCANLINE

  constant VBL_TO_VSYNC : integer := 33;
  constant VGA_VSYNC_LINES : integer := 4;

  signal VGA_VS_I, VGA_HS_I : std_logic;

  signal video_active : std_logic;
  signal color_line_delayed : std_logic;

begin

  hcount_vcount_control : process (clk_14m)
  begin
    if rising_edge(clk_14m) then
      if HBL = '0' and last_hbl = '1' then  -- Falling edge
        last_vbl <= VBL;
        color_line_delayed <= COLOR_LINE;
        hcount <= (others => '0');
        vcount <= vcount + 1;
        if last_VBL = '0' and VBL = '1' then
          vcount <= (others => '0');
        end if;
      else
        hcount <= hcount + 1;
      end if;
      last_hbl <= HBL;
    end if;
  end process hcount_vcount_control;

  hsync_gen : process (clk_14m)
  begin
    if rising_edge(clk_14m) then

      if hcount = VGA_ACTIVE + VGA_FRONT_PORCH then
        VGA_HS_I <= '0';
      elsif hcount = VGA_ACTIVE + VGA_FRONT_PORCH + VGA_HSYNC then
        VGA_HS_I <= '1';
      end if;

      if hcount = 8 then
        hactive <= '1';
        vactive <= not VBL;
      elsif hcount = VGA_ACTIVE + 10 then
        hactive <= '0';
        vactive <= not VBL;
      end if;
    end if;
  end process hsync_gen;

  VGA_HS <= VGA_HS_I;

  vsync_gen : process (clk_14m)
  begin
    if rising_edge(clk_14m) then
      if vcount = VBL_TO_VSYNC and hcount = VGA_ACTIVE + VGA_FRONT_PORCH + VGA_HSYNC then
        VGA_VS_I <= '0';
      elsif vcount = VBL_TO_VSYNC + VGA_VSYNC_LINES and hcount = VGA_ACTIVE + VGA_FRONT_PORCH then
        VGA_VS_I <= '1';
      end if;
    end if;
  end process vsync_gen;

  VGA_VS <= VGA_VS_I;

  shifter: process (clk_14m)
  begin
    if rising_edge(clk_14m) then
      shift_reg <= VIDEO & shift_reg(5 downto 1);
    end if;
  end process;
  
  video_active <= hactive and vactive;

  pixel_generator: process (clk_14m)
    variable r, g, b : unsigned(7 downto 0); 
  begin
    if rising_edge(clk_14m) then
	 
      r := X"00";
      g := X"00"; 
      b := X"00"; 
		
		-- alternate background for monochrome modes
		case SCREEN_MODE is 
			when "00" =>
				r := X"00"; g := X"00"; b := X"00"; -- color mode background
			when "01" => 
				r := X"00"; g := X"00"; b := X"00"; -- B&W mode background
			when "10" =>
				r := X"00"; g := X"0F"; b := X"01"; -- green mode background color
			when "11" =>
				r := X"20"; g := X"08"; b := X"01"; -- amber mode background color
		end case;
		
      if video_active = '1' then
        
        if color_line_delayed = '0' then  -- Monochrome mode
          
			 if shift_reg(2) = '1' then
				-- handle green/amber color modes
				case SCREEN_MODE is 
					when "00" => 
						r := X"FF"; g := X"FF"; b := X"FF"; -- white (color mode)
					when "01" => 
						r := X"FF"; g := X"FF"; b := X"FF"; -- white (B&W mode)
					when "10" =>
						r := X"00"; g := X"C0"; b := X"01"; -- green
					when "11" =>
						r := X"FF"; g := X"80"; b := X"01"; -- amber 
				end case;
			 end if;
          
        elsif shift_reg(0) = shift_reg(4) and shift_reg(5) = shift_reg(1) then
          
          -- Tint of adjacent pixels is consistent : display the color
          
          if shift_reg(3) = '1' then
            r := r + basis_r(to_integer(hcount + 1));
            g := g + basis_g(to_integer(hcount + 1));
            b := b + basis_b(to_integer(hcount + 1));
          end if;
          if shift_reg(4) = '1' then
            r := r + basis_r(to_integer(hcount + 2));
            g := g + basis_g(to_integer(hcount + 2));
            b := b + basis_b(to_integer(hcount + 2));
          end if;
          if shift_reg(1) = '1' then
            r := r + basis_r(to_integer(hcount + 3));
            g := g + basis_g(to_integer(hcount + 3));
            b := b + basis_b(to_integer(hcount + 3));
          end if;
          if shift_reg(2) = '1' then
            r := r + basis_r(to_integer(hcount));
            g := g + basis_g(to_integer(hcount));
            b := b + basis_b(to_integer(hcount));
          end if;
        else
          
          -- Tint is changing: display only black, gray, or white
          
          case shift_reg(3 downto 2) is
            when "11"        => r := X"FF"; g := X"FF"; b := X"FF";
            when "01" | "10" => r := X"80"; g := X"80"; b := X"80";
            when others      => r := X"00"; g := X"00"; b := X"00";
          end case;
        end if;
        
      end if;
      
      VGA_R <= r;
      VGA_G <= g;
      VGA_B <= b;
      
    end if;
  end process pixel_generator;

  VGA_CLK <= clk_14m;

  VGA_BLANK <= video_active;

end rtl;
