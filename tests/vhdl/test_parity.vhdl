--* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
--* * * * * * * * * * * * * * * * VHDL Source Code  * * * * * * * * * * * * * *
--* * * Copyright (C) 1997 - Eastman Kodak Company - All Rights Reserved  * * *
--* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
--* Title           :  TEST_PARITY
--* Filename & Ext  :  test_parity.vhdl
--* Author          :  David Bishop X-66788
--* Created         :  3/18/97
--* Version         :  1.2
--* Revision Date   :  97/04/15
--* SCCSid          :  1.2 04/15/97 test_parity.vhdl
--* WORK Library    :  testchip
--* Mod History     :  
--* Description     :  This is a parity generator which is written recursively
--*                 :  It is designed to test the ability of Simulation and
--*                 :  Synthesis tools to check this capability.
--* Known Bugs      :  
--* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- Parity is done in a recursively called function.
-- Package definition
package Parity_Pack is
  function Recursive_Parity ( BUSX : std_logic_vector )
       return std_ulogic;
end Parity_Pack;

-- Package body.
package body Parity_Pack is
  function Recursive_Parity ( BUSX : std_logic_vector )
       return std_ulogic is
    variable Upper, Lower : std_ulogic;
    variable Half : integer;
    variable BUS_int : std_logic_vector ( BUSX'length - 1 downto 0 );
    variable Result : std_logic;
  begin
    BUS_int := BUSX;
    if ( BUS_int'length = 1 ) then
      Result := BUS_int ( BUS_int'left );
    elsif ( BUS_int'length = 2 ) then
      Result := BUS_int ( BUS_int'right ) xor BUS_int ( BUS_int'left );
    else
      Half := ( BUS_int'length + 1 ) / 2 + BUS_int'right;
      Upper := Recursive_Parity ( BUS_int ( BUS_int'left downto Half ));
      Lower := Recursive_Parity ( BUS_int ( Half - 1 downto BUS_int'right ));
      Result := Upper xor Lower;
    end if;
    return Result;
  end;
end Parity_Pack;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use work.Parity_Pack.all;

entity Test_Parity is
  generic ( WIDTH : integer := 16);
  port ( BUSX : in std_logic_vector ( WIDTH downto 0 );
         Parity : out std_ulogic );
end Test_Parity;

architecture RTL of Test_Parity is
begin
  Parity <= Recursive_Parity ( BUSX );
end RTL;
