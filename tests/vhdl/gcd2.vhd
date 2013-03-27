----------------------------------------------------------------------
-- GCD CALCULATOR (ESD book figure 2.11)			
-- Weijun Zhang, 04/2001
--
-- we can put all the components in one document(gcd2.vhd)
-- or put them in separate files 
-- this is the example of RT level modeling (FSM + DataPath)
-- the code is synthesized by Synopsys design compiler
----------------------------------------------------------------------

-- Component: MULTIPLEXOR --------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity mux is 
    port(	rst, sLine: in std_logic;
		load, result: in std_logic_vector( 3 downto 0 );
		output: out std_logic_vector( 3 downto 0 )
    );
end mux;

architecture mux_arc of mux is
begin
    process( rst, sLine, load, result )
    begin
	if( rst = '1' ) then 
	    output <= "0000";		-- do nothing
	elsif sLine = '0' then 
	    output <= load;		-- load inputs
	else 
	    output <= result;		-- load results
	end if;
    end process;
end mux_arc;
	
-- Component: COMPARATOR ---------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity comparator is
    port(	rst: in std_logic;
		x, y: in std_logic_vector( 3 downto 0 );
		output: out std_logic_vector( 1 downto 0 )
    );
end comparator;

architecture comparator_arc of comparator is
begin
    process( x, y, rst )
    begin
	if( rst = '1' ) then 
	    output <= "00";		-- do nothing
	elsif( x > y ) then 
	    output <= "10";		-- if x greater
	elsif( x < y ) then 
	    output <= "01";		-- if y greater
	else 
	    output <= "11";  	        -- if equivalance. 	
	end if;
    end process;
end comparator_arc;

-- Component: SUBTRACTOR ----------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity subtractor is
    port(	rst: in std_logic;
		cmd: in std_logic_vector( 1 downto 0 );
		x, y: in std_logic_vector( 3 downto 0 );
		xout, yout: out std_logic_vector( 3 downto 0 )
    );
end subtractor;

architecture subtractor_arc of subtractor is
begin
    process( rst, cmd, x, y )
    begin
	if( rst = '1' or cmd = "00" ) then 	-- not active.
	    xout <= "0000";	
	    yout <= "0000";
	elsif( cmd = "10" ) then 		-- x is greater
	    xout <= ( x - y );
	    yout <= y;
	elsif( cmd = "01" ) then 		-- y is greater
	    xout <= x;	
	    yout <= ( y - x );
	else 
	    xout <= x;  			-- x and y are equal
	    yout <= y; 		
	end if;
    end process;
end subtractor_arc;

-- Component: REGISTER ---------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity regis is
    port(	rst, clk, load: in std_logic;
	        input: in std_logic_vector( 3 downto 0 );
		output: out std_logic_vector( 3 downto 0 )
    );
end regis;

architecture regis_arc of regis is
begin
    process( rst, clk, load, input )
    begin
	if( rst = '1' ) then 
	    output <= "0000";
	elsif( clk'event and clk = '1') then
	    if( load = '1' ) then	
	        output <= input;
	    end if;
        end if;
    end process;
end regis_arc;

-- component: FSM controller --------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity fsm is
    port(	rst, clk, proceed: in std_logic;
		comparison: in std_logic_vector( 1 downto 0 );
		enable, xsel, ysel, xld, yld: out std_logic
    );
end fsm;

architecture fsm_arc of fsm is

    type states is ( init, s0, s1, s2, s3, s4, s5 );
    signal nState, cState: states;

begin
    process( rst, clk )
    begin
	if( rst = '1' ) then 
	    cState <= init;
	elsif( clk'event and clk = '1' ) then 
	    cState <= nState;
	end if;
    end process;

    process( proceed, comparison, cState )
    begin
	case cState is 
		
	when init =>	if( proceed = '0' ) then 
			    nState <= init;
			else 
			    nState <= s0;
			end if;
			
	when s0 =>	enable <= '0';
			xsel <= '0';
			ysel <= '0';
			xld <= '0';
			yld <= '0';
			nState <= s1;
	
	when s1 =>	enable <= '0';
			xsel <= '0';
			ysel <= '0';
			xld <= '1';
			yld <= '1';
			nState <= s2;
		
	when s2 =>	xld <= '0';
			yld <= '0';
			if( comparison = "10" ) then 
			    nState <= s3;
			elsif( comparison = "01" ) then 
			    nState <= s4; 	
			elsif( comparison = "11" ) then 
			    nState <= s5;   	
			end if;
		
	when s3 =>	enable <= '0';
			xsel <= '1';
			ysel <= '0';
			xld <= '1';
			yld <= '0';
			nState <= s2;
	
	when s4 =>	enable <= '0';
			xsel <= '0';
			ysel <= '1';
			xld <= '0';
			yld <= '1';
			nState <= s2;

	when s5 =>	enable <= '1';
			xsel <= '1';
			ysel <= '1';
			xld <= '1';
			yld <= '1';
			nState <= s0;
			
	when others =>	nState <= s0;
			
        end case;
	
    end process;
	
end fsm_arc;

----------------------------------------------------------------------
-- GCD Calculator: top level design using structural modeling
-- FSM + Datapath (mux, registers, subtracter and comparator)
----------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
use work.all;

entity gcd is
    port(	rst, clk, go_i: in std_logic;
		x_i, y_i: in std_logic_vector( 3 downto 0 );
		d_o: out std_logic_vector( 3 downto 0 )
    );
end gcd;

architecture gcd_arc of gcd is

component fsm is
    port(	rst, clk, proceed: in std_logic;
		comparison: in std_logic_vector( 1 downto 0 );
		enable, xsel, ysel, xld, yld: out std_logic
    );
end component;

component mux is 
    port(	rst, sLine: in std_logic;
		load, result: in std_logic_vector( 3 downto 0 );
		output: out std_logic_vector( 3 downto 0 )
    );
end component;

component comparator is
    port(	rst: in std_logic;
		x, y: in std_logic_vector( 3 downto 0 );
		output: out std_logic_vector( 1 downto 0 )
    );
end component;

component subtractor is
    port( 	rst: in std_logic; 
		cmd: in std_logic_vector( 1 downto 0 );
		x, y: in std_logic_vector( 3 downto 0 );
		xout, yout: out std_logic_vector( 3 downto 0 )
    );
end component;

component regis is
    port(	rst, clk, load: in std_logic;
		input: in std_logic_vector( 3 downto 0 );
		output: out std_logic_vector( 3 downto 0 )
    );
end component;

signal xld, yld, xsel, ysel, enable: std_logic;
signal comparison: std_logic_vector( 1 downto 0 );
signal result: std_logic_vector( 3 downto 0 );					

signal xsub, ysub, xmux, ymux, xreg, yreg: std_logic_vector( 3 downto 0 );

begin

    -- doing structure modeling here

    -- FSM controller
    TOFSM: fsm port map(    rst, clk, go_i, comparison, 
			    enable, xsel, ysel, xld, yld );   				 
    -- Datapath
    X_MUX: mux port map( rst, xsel, x_i, xsub, xmux );
    Y_MUX: mux port map( rst, ysel, y_i, ysub, ymux );
    X_REG: regis port map( rst, clk, xld, xmux, xreg ); 
    Y_REG: regis port map( rst, clk, yld, ymux, yreg ); 
    U_COMP: comparator port map( rst, xreg, yreg, comparison );
    X_SUB: subtractor port map( rst, comparison, xreg, yreg, xsub, ysub );
    OUT_REG: regis port map( rst, clk, enable, xsub, result );
	
    d_o <= result; 

end gcd_arc;

---------------------------------------------------------------------------
