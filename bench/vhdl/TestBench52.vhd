library IEEE;
use IEEE.std_logic_1164.all;
use work.StimLog.all;

entity TestBench52 is
end TestBench52;

architecture behaviour of TestBench52 is

	signal Clk			: std_logic := '0';
	signal Rst_n		: std_logic := '0';
	signal RXD			: std_logic;
	signal RXD_IsOut	: std_logic;
	signal RXD_Out		: std_logic;
	signal TXD			: std_logic;
	signal INT0			: std_logic := '0';
	signal P0			: std_logic_vector(7 downto 0);
	signal P1			: std_logic_vector(7 downto 0);
	signal P2			: std_logic_vector(7 downto 0);
	signal P3			: std_logic_vector(7 downto 0);
	signal ExSel		: std_logic;
	signal ExRd			: std_logic;
	signal ExWr			: std_logic;
	signal ExAddr		: std_logic_vector(15 downto 0);
	signal ExDI			: std_logic_vector(7 downto 0);
	signal ExDO			: std_logic_vector(7 downto 0);

begin

	u0 : entity work.T8052
		port map(
			Clk => Clk,
			Rst_n => Rst_n,
			P0 => P0,
			P1 => P1,
			P2 => P2,
			P3 => P3,
			INT0 => INT0,
			INT1 => '1',
			T0 => '1',
			T1 => '1',
			T2 => '1',
			T2EX => '1',
			RXD => RXD,
			RXD_IsO => RXD_IsOut,
			RXD_O => RXD_Out,
			TXD => TXD,
			ExSel => ExSel,
			ExRd => ExRd,
			ExWr => ExWr,
			ExAddr => ExAddr,
			ExDI => ExDI,
			ExDO => ExDO);

	P3(0) <= RXD;
	ExDI <= (others => '1');

	as : AsyncStim generic map(FileName => "BASIC.txt", InterCharDelay => 12000 us, Baud => 115200, Bits => 8)
				port map(RXD);

	al : AsyncLog generic map(FileName => "RX_Log.txt", Baud => 14400, Bits => 8)
				port map(TXD);

	Clk <= not Clk after 45 ns;
	Rst_n <= '1' after 200 ns;

	INT0 <= not INT0 after 100 us;

end;
