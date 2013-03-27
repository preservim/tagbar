--* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
--* * * * * * * * * * * * * * * * VHDL Source Code  * * * * * * * * * * * * * *
--* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
--* Title           :  Test_MultPipe
--* Filename & Ext  :  test_multpipe.vhdl
--* Author          :  David Bishop  <dbishop@vhdl.org> X-XXXXX
--* Created         :  1999/03/12
--* Last modified   :  $Date: 1999-03-12 16:41:40-05 $
--* WORK Library    :  testchip_lib
--* Description     :  A pipline multiplier with lots of redundant logic
--* Known Bugs      :
--*                 :  
--* RCS Summary     :  $Id: test_multpipe.vhdl,v 1.1 1999-03-12 16:41:40-05 bishop Exp bishop $
--*                 :
--* Mod History     :  $Log: test_multpipe.vhdl,v $
--* Mod History     :  Revision 1.1  1999-03-12 16:41:40-05  bishop
--* Mod History     :  Initial revision
--* Mod History     :
--*                 :
--* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_multpipe is
  generic ( width : integer := 7 );
  port ( clk        : in  std_ulogic;
         reset      : in  std_ulogic;
         enable     : in  std_ulogic;
         inp1, inp2 : in  std_logic_vector ( width downto 0);
         sum        : out std_logic_vector ( (width * 2 ) downto 0) );
end test_multpipe;

architecture rtl of test_multpipe is
  subtype vectors is std_logic_vector (( width * 2 ) downto 0 );
  subtype unsigneds is unsigned (( width * 2 ) downto 0 );
  type    stage_array is array ( width downto 0 ) of vectors;
  type    adder_array is array ( width downto 0 ) of unsigneds;
  signal  stage1, stage2 : stage_array;
  signal  adder_stage    : adder_array;
  signal  reg_stage      : adder_array;
begin  --  rtl 

  -- Fill the two arrays
  build_stage1 : process ( inp1 )
  begin
    stage_loop1 : for i in 0 to width loop
      stage1 ( i )                        <= ( others => '0' );
      stage1 ( i ) ( width + i downto i ) <= inp1;
    end loop stage_loop1;
  end process build_stage1;

  build_stage2 : process ( inp2 )
  begin
    stage_loop2 : for i in 0 to width loop
      stage2 ( i ) <= ( others => inp2 ( i ) );
    end loop stage_loop2;
  end process build_stage2;

-- and the two arrays together, now you have a matrix which
-- you can add up.
  and_stages : process ( stage1, stage2 )
  begin
    and_loop : for i in 0 to width loop
      adder_stage ( i ) <= unsigned ( stage1 ( i ) and stage2 ( i ) );
    end loop and_loop;
  end process and_stages;

  register_stages : process ( reset, clk )
    variable local_sum : unsigned ( sum'high downto 0 );
  begin
    if reset = '0' then
      reset_loop : for i in 0 to width loop
        reg_stage ( i ) <= ( others => '0' );
      end loop reset_loop;
    elsif rising_edge ( clk ) then
      if enable = '1' then
        adder_loop : for i in 0 to ( width + 1 ) / 2 loop
          reg_stage ( i ) <= adder_stage ( i ) + adder_stage ( width - i );
        end loop adder_loop;
        reg_stage ( 4 ) <= reg_stage ( 0 ) + reg_stage ( 3 );
        reg_stage ( 5 ) <= reg_stage ( 1 ) + reg_stage ( 2 );
        reg_stage ( 6 ) <= reg_stage ( 4 ) + reg_stage ( 5 );
      end if;
    end if;
  end process register_stages;

  sum <= std_logic_vector (reg_stage ( 6 ));

end rtl;
