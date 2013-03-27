--* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
--* * * * * * * * * * * * * * * * VHDL Source Code  * * * * * * * * * * * * * *
--* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
--* Title           :  TESTCHIP_CORE
--* Filename & Ext  :  testchip_core.vhdl
--* Author          :  David W. Bishop
--* Created         :  6/6/96
--* Version         :  1.1
--* Revision Date   :  97/12/03
--* SCCSid          :  1.1 12/03/97 testchip_core.vhdl
--* WORK Library    :  chiptest
--* Mod History     :  
--* Description     :  This is a test chip core, designed to test several
--*                 :  functions in Synthesis and simulation
--* Known Bugs      :  
--* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

library ieee;
use ieee.std_logic_1164.all;

entity testchip_core is
  port ( clk        : in  std_ulogic;
         reset      : in  std_ulogic;
         dclk       : in  std_ulogic;
         dclk_i     : out std_ulogic;
         slow_count : out std_logic_vector ( 4 downto 0 );
         enable     : in  std_ulogic;
         load       : in  std_ulogic;
         input_data : in  std_logic_vector ( 7 downto 0 );
         shout      : out std_ulogic;
         control    : out std_logic_vector ( 1 downto 0 );
         count      : out std_logic_vector ( 7 downto 0 );
         sum        : out std_logic_vector ( 8 downto 0 );
         mulout     : out std_logic_vector ( 14 downto 0 );
         parity     : out std_ulogic );
end testchip_core;

architecture rtl of testchip_core is

--------------------------------------------------------------------------------- Start with the declarations for the sub-blocks
-------------------------------------------------------------------------------
  component Test_ClkGen
    port ( clk   : in  std_ulogic;
           reset : in  std_ulogic;
           dclk  : out std_ulogic );
  end component;

  component test_counter
    generic ( width : integer := 17 );
    port ( clk    : in  std_ulogic;
           reset  : in  std_ulogic;
           enable : in  std_ulogic;
           count  : out std_logic_vector ( width - 1 downto 0) );
  end component;

  component test_add
    generic ( width : integer := 17 );
    port ( clk    : in  std_ulogic;
           reset  : in  std_ulogic;
           enable : in  std_ulogic;
           inp1   : in  std_logic_vector ( width downto 0);
           inp2   : in  std_logic_vector ( width downto 0);
           sum    : out std_logic_vector ( (width + 1) downto 0) );
  end component;

  component test_reg
    generic ( width : integer := 17 );
    port ( clk    : in  std_ulogic;
           reset  : in  std_ulogic;
           enable : in  std_ulogic;
           sel    : in  std_ulogic;
           inp1   : in  std_logic_vector ( width downto 0);
           inp2   : in  std_logic_vector ( width downto 0);
           outpt  : out std_logic_vector ( width downto 0) );
  end component;

  component test_shift
    generic ( width : integer := 17 );
    port ( clk   : in  std_ulogic;
           reset : in  std_ulogic;
           load  : in  std_ulogic;
           en    : in  std_ulogic;
           inp   : in  std_logic_vector ( width downto 0 );
           outp  : out std_ulogic );
  end component;

  component test_state
    port ( clk              : in  std_ulogic;
           reset            : in  std_ulogic;
           con1, con2, con3 : in  std_ulogic;
           out1, out2       : out std_ulogic );
  end component;

  component test_multpipe
    generic ( width : integer := 7 );
    port ( clk        : in  std_ulogic;
           reset      : in  std_ulogic;
           enable     : in  std_ulogic;
           inp1, inp2 : in  std_logic_vector ( width downto 0);
           sum        : out std_logic_vector ( (width * 2 ) downto 0) );
  end component;

  component test_parity
    generic ( WIDTH : integer := 16);
    port ( BUSX   : in  std_logic_vector ( WIDTH downto 0 );
           Parity : out std_ulogic );
  end component;

-- Signal declarations
  signal internal_data : std_logic_vector ( input_data'high downto 0 );
  signal local_count   : std_logic_vector ( input_data'high downto 0 );
  signal local_mulout  : std_logic_vector ( mulout'high downto 0 );
  signal VCC, GND      : std_ulogic;

begin  --  rtl 

  VCC    <= '1';
  GND    <= '0';
  mulout <= local_mulout;

  U1 : test_clkgen
    port map (
      clk   => clk,
      reset => reset,
      dclk  => dclk_i );

  U2 : test_counter
    generic map ( width => 5 )
    port map (
      clk    => dclk,
      reset  => reset,
      enable => VCC,
      count  => slow_count
      );

  U3 : test_reg
    generic map ( width => internal_data'high )
    port map (
      clk    => clk,
      reset  => reset,
      enable => VCC,
      sel    => VCC,
      inp1   => input_data,
      inp2   => local_count,
      outpt  => internal_data
      );

  U4 : test_counter
    generic map ( width => internal_data'high + 1 )
    port map (
      clk    => clk,
      reset  => reset,
      enable => enable,
      count  => local_count
      );

  U7 : test_add
    generic map ( width => internal_data'high )
    port map (
      clk    => clk,
      reset  => reset,
      enable => enable,
      inp1   => local_count,
      inp2   => internal_data,
      sum    => sum
      );

  U5 : test_shift
    generic map ( width => internal_data'high )
    port map (
      clk   => clk,
      reset => reset,
      en    => enable,
      load  => load,
      inp   => internal_data,
      outp  => shout
      );

  U9 : test_state
    port map (
      clk   => clk,
      reset => reset,
      con1  => local_count ( 0 ),
      con2  => local_count ( 1 ),
      con3  => local_count ( 2 ),
      out1  => control ( 0 ),
      out2  => control ( 1 )
      );

  U8 : test_multpipe
    generic map ( width => internal_data'high )
    port map (
      clk    => clk,
      reset  => reset,
      enable => enable,
      inp1   => local_count,
      inp2   => internal_data,
      sum    => local_mulout );

  U6 : Test_Parity
    generic map ( Width => internal_data'high )
    port map (
      BUSX   => internal_data,
      Parity => parity );

  U10 : test_reg
    generic map ( width => count'high )
    port map (
      clk    => clk,
      reset  => reset,
      enable => VCC,
      sel    => GND,
      inp1   => input_data,
      inp2   => local_count,
      outpt  => count
      );


end rtl;
