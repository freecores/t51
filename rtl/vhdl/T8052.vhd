--
-- 8052 compatible microcontroller, with internal RAM & ROM
--
-- Version : 0218
--
-- Copyright (c) 2001-2002 Daniel Wallner (jesus@opencores.org)
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- Please report bugs to the author, but before you do so, please
-- make sure that this is not a derivative work and that
-- you have the latest version of this file.
--
-- The latest version of this file can be found at:
--	http://www.opencores.org/cvsweb.shtml/t51/
--
-- Limitations :
--
-- File history :
--

library IEEE;
use IEEE.std_logic_1164.all;
use work.T51_Pack.all;

entity T8052 is
	generic(
		ROMAddressWidth		: integer := 13;
		IRAMAddressWidth	: integer := 8;
		XRAMAddressWidth	: integer := 11);
	port(
		Clk			: in std_logic;
		Rst_n		: in std_logic;
		P0			: inout std_logic_vector(7 downto 0);
		P1			: inout std_logic_vector(7 downto 0);
		P2			: inout std_logic_vector(7 downto 0);
		P3			: inout std_logic_vector(7 downto 0);
		INT0		: in std_logic;
		INT1		: in std_logic;
		T0			: in std_logic;
		T1			: in std_logic;
		T2			: in std_logic;
		T2EX		: in std_logic;
		RXD			: in std_logic;
		RXD_IsO		: out std_logic;
		RXD_O		: out std_logic;
		TXD			: out std_logic;
		ExSel		: out std_logic;
		ExRd		: out std_logic;
		ExWr		: out std_logic;
		ExAddr		: out std_logic_vector(15 downto 0);
		ExDI		: in std_logic_vector(7 downto 0);
		ExDO		: out std_logic_vector(7 downto 0)
	);
end T8052;

architecture rtl of T8052 is

	component ROM52
		port(
			Clk	: in std_logic;
			A	: in std_logic_vector(ROMAddressWidth - 1 downto 0);
			D	: out std_logic_vector(7 downto 0)
		);
	end component;

	component SSRAM
	generic(
		AddrWidth	: integer := 16;
		DataWidth	: integer := 8
	);
	port(
		Clk			: in std_logic;
		CE_n		: in std_logic;
		WE_n		: in std_logic;
		A			: in std_logic_vector(AddrWidth - 1 downto 0);
		DIn			: in std_logic_vector(DataWidth - 1 downto 0);
		DOut		: out std_logic_vector(DataWidth - 1 downto 0)
	);
	end component;

	signal	Rst_s_n		: std_logic;
	signal	ROM_Addr	: std_logic_vector(15 downto 0);
	signal	ROM_Data	: std_logic_vector(7 downto 0);
	signal	RAM_Addr	: std_logic_vector(15 downto 0);
	signal	RAM_RData	: std_logic_vector(7 downto 0);
	signal	RAM_DO		: std_logic_vector(7 downto 0);
	signal	RAM_WData	: std_logic_vector(7 downto 0);
	signal	RAM_Rd		: std_logic;
	signal	RAM_Wr		: std_logic;
	signal	RAM_WE_n	: std_logic;
	signal	RAM_Sel_Addr: std_logic_vector(15 downto XRAMAddressWidth);
	signal	RAM_Sel_n	: std_logic;
	signal	Ex_Sel_i	: std_logic;
	signal	IO_Rd		: std_logic;
	signal	IO_Wr		: std_logic;
	signal	IO_Addr		: std_logic_vector(6 downto 0);
	signal	IO_Addr_r	: std_logic_vector(6 downto 0);
	signal	IO_WData	: std_logic_vector(7 downto 0);
	signal	IO_RData	: std_logic_vector(7 downto 0);

	signal	P0_Sel		: std_logic;
	signal	P1_Sel		: std_logic;
	signal	P2_Sel		: std_logic;
	signal	P3_Sel		: std_logic;
	signal	TMOD_Sel	: std_logic;
	signal	TL0_Sel		: std_logic;
	signal	TL1_Sel		: std_logic;
	signal	TH0_Sel		: std_logic;
	signal	TH1_Sel		: std_logic;
	signal	T2CON_Sel	: std_logic;
	signal	RCAP2L_Sel	: std_logic;
	signal	RCAP2H_Sel	: std_logic;
	signal	TL2_Sel		: std_logic;
	signal	TH2_Sel		: std_logic;
	signal	SCON_Sel	: std_logic;
	signal	SBUF_Sel	: std_logic;

	signal	P0_Wr		: std_logic;
	signal	P1_Wr		: std_logic;
	signal	P2_Wr		: std_logic;
	signal	P3_Wr		: std_logic;
	signal	TMOD_Wr		: std_logic;
	signal	TL0_Wr		: std_logic;
	signal	TL1_Wr		: std_logic;
	signal	TH0_Wr		: std_logic;
	signal	TH1_Wr		: std_logic;
	signal	T2CON_Wr	: std_logic;
	signal	RCAP2L_Wr	: std_logic;
	signal	RCAP2H_Wr	: std_logic;
	signal	TL2_Wr		: std_logic;
	signal	TH2_Wr		: std_logic;
	signal	SCON_Wr		: std_logic;
	signal	SBUF_Wr		: std_logic;

	signal	UseR2		: std_logic;
	signal	UseT2		: std_logic;
	signal	UART_Clk	: std_logic;
	signal	R0			: std_logic;
	signal	R1			: std_logic;
	signal	SMOD		: std_logic;

	signal	Int_Trig	: std_logic_vector(6 downto 0);
	signal	Int_Acc		: std_logic_vector(6 downto 0);

	signal	RI			: std_logic;
	signal	TI			: std_logic;
	signal	OF0			: std_logic;
	signal	OF1			: std_logic;
	signal	OF2			: std_logic;

begin

	-- Synchronise reset
	process (Rst_n, Clk)
	begin
		if Rst_n = '0' then
			Rst_s_n <= '0';
		elsif Clk'event and Clk = '1' then
			Rst_s_n <= '1';
		end if;
	end process;

	ExSel <= Ex_Sel_i;
	ExRd <= RAM_Rd;
	ExWr <= RAM_Wr;
	ExDO <= RAM_WData;

	process (Clk)
	begin
		if Clk'event and Clk = '1' then
			IO_Addr_r <= IO_Addr;
			Ex_Sel_i <= RAM_Sel_n;
			ExAddr <= RAM_Addr;
		end if;
	end process;

	rom : ROM52 port map(
			Clk => Clk,
			A => ROM_Addr(ROMAddressWidth - 1 downto 0),
			D => ROM_Data);

	g_rams0 : if XRAMAddressWidth > 15 generate
		RAM_Sel_n <= '0';
	end generate;

	g_rams1 : if XRAMAddressWidth < 16 and  XRAMAddressWidth > 0 generate
		RAM_Sel_n <= '0' when RAM_Addr(15 downto XRAMAddressWidth) = RAM_Sel_Addr else '1';
	end generate;

	RAM_Sel_Addr <= (others => '0');
	RAM_WE_n <= not RAM_Wr;
	RAM_RData <= ExDI when Ex_Sel_i = '1' else RAM_DO;

	g_ram : if XRAMAddressWidth > 0 generate
		ram : SSRAM
			generic map(
				AddrWidth => XRAMAddressWidth)
			port map(
				Clk => Clk,
				CE_n => Ex_Sel_i,
				WE_n => RAM_WE_n,
				A => RAM_Addr(XRAMAddressWidth - 1 downto 0),
				DIn => RAM_WData,
				DOut => RAM_DO);
	end generate;

	core51 : T51
		generic map(
			RAMAddressWidth => IRAMAddressWidth)
		port map(
			Clk => Clk,
			Rst_n => Rst_s_n,
			ROM_Addr => ROM_Addr,
			ROM_Data => ROM_Data,
			RAM_Addr => RAM_Addr,
			RAM_RData => RAM_RData,
			RAM_WData => RAM_WData,
			RAM_Rd => RAM_Rd,
			RAM_Wr => RAM_Wr,
			Int_Trig => Int_Trig,
			Int_Acc => Int_Acc,
			SFR_Rd_RMW => IO_Rd,
			SFR_Wr => IO_Wr,
			SFR_Addr => IO_Addr,
			SFR_WData => IO_WData,
			SFR_RData => IO_RData);

	glue51 : T51_Glue
		port map(
			Clk => Clk,
			Rst_n => Rst_s_n,
			INT0 => INT0,
			INT1 => INT1,
			T0 => T0,
			T1 => T1,
			T2 => T2,
			T2EX => T2EX,
			RI => RI,
			TI => TI,
			OF0 => OF0,
			OF1 => OF1,
			OF2 => OF2,
			IO_Rd => IO_Rd,
			IO_Wr => IO_Wr,
			IO_Addr => IO_Addr,
			IO_Addr_r => IO_Addr_r,
			IO_WData => IO_WData,
			IO_RData => IO_RData,
			Int_Acc => Int_Acc,
			R0 => R0,
			R1 => R1,
			SMOD => SMOD,
			P0_Sel => P0_Sel,
			P1_Sel => P1_Sel,
			P2_Sel => P2_Sel,
			P3_Sel => P3_Sel,
			TMOD_Sel => TMOD_Sel,
			TL0_Sel => TL0_Sel,
			TL1_Sel => TL1_Sel,
			TH0_Sel => TH0_Sel,
			TH1_Sel => TH1_Sel,
			T2CON_Sel => T2CON_Sel,
			RCAP2L_Sel =>RCAP2L_Sel,
			RCAP2H_Sel => RCAP2H_Sel,
			TL2_Sel => TL2_Sel,
			TH2_Sel => TH2_Sel,
			SCON_Sel => SCON_Sel,
			SBUF_Sel => SBUF_Sel,
			P0_Wr => P0_Wr,
			P1_Wr => P1_Wr,
			P2_Wr => P2_Wr,
			P3_Wr => P3_Wr,
			TMOD_Wr => TMOD_Wr,
			TL0_Wr => TL0_Wr,
			TL1_Wr => TL1_Wr,
			TH0_Wr => TH0_Wr,
			TH1_Wr => TH1_Wr,
			T2CON_Wr => T2CON_Wr,
			RCAP2L_Wr => RCAP2L_Wr,
			RCAP2H_Wr => RCAP2H_Wr,
			TL2_Wr => TL2_Wr,
			TH2_Wr => TH2_Wr,
			SCON_Wr => SCON_Wr,
			SBUF_Wr => SBUF_Wr,
			Int_Trig => Int_Trig);

	tp0 : T51_Port
		port map(
			Clk => Clk,
			Rst_n => Rst_s_n,
			Sel => P0_Sel,
			Rd_RMW => IO_Rd,
			Wr => P0_Wr,
			Data_In => IO_WData,
			Data_Out => IO_RData,
			IOPort => P0);

	tp1 : T51_Port
		port map(
			Clk => Clk,
			Rst_n => Rst_s_n,
			Sel => P1_Sel,
			Rd_RMW => IO_Rd,
			Wr => P1_Wr,
			Data_In => IO_WData,
			Data_Out => IO_RData,
			IOPort => P1);

	tp2 : T51_Port
		port map(
			Clk => Clk,
			Rst_n => Rst_s_n,
			Sel => P2_Sel,
			Rd_RMW => IO_Rd,
			Wr => P2_Wr,
			Data_In => IO_WData,
			Data_Out => IO_RData,
			IOPort => P2);

	tp3 : T51_Port
		port map(
			Clk => Clk,
			Rst_n => Rst_s_n,
			Sel => P3_Sel,
			Rd_RMW => IO_Rd,
			Wr => P3_Wr,
			Data_In => IO_WData,
			Data_Out => IO_RData,
			IOPort => P3);

	tc01 : T51_TC01
		generic map(
			FastCount => false)
		port map(
			Clk => Clk,
			Rst_n => Rst_s_n,
			T0 => T0,
			T1 => T1,
			INT0 => INT0,
			INT1 => INT1,
			M_Sel => TMOD_Sel,
			H0_Sel => TH0_Sel,
			L0_Sel => TL0_Sel,
			H1_Sel => TH1_Sel,
			L1_Sel => TL1_Sel,
			R0 => R0,
			R1 => R1,
			M_Wr => TMOD_Wr,
			H0_Wr => TH0_Wr,
			L0_Wr => TL0_Wr,
			H1_Wr => TH1_Wr,
			L1_Wr => TL1_Wr,
			Data_In => IO_WData,
			Data_Out => IO_RData,
			OF0 => OF0,
			OF1 => OF1);

	tc2 : T51_TC2
		generic map(
			FastCount => false)
		port map(
			Clk => Clk,
			Rst_n => Rst_s_n,
			T2 => T2,
			T2EX => T2EX,
			C_Sel => T2CON_Sel,
			CH_Sel => RCAP2H_Sel,
			CL_Sel => RCAP2L_Sel,
			H_Sel => TH2_Sel,
			L_Sel => TL2_Sel,
			C_Wr => T2CON_Wr,
			CH_Wr => RCAP2H_Wr,
			CL_Wr => RCAP2L_Wr,
			H_Wr => TH2_Wr,
			L_Wr => TL2_Wr,
			Data_In => IO_WData,
			Data_Out => IO_RData,
			UseR2 => UseR2,
			UseT2 => UseT2,
			UART_Clk => UART_Clk,
			F => OF2);

	uart : T51_UART
		generic map(
			FastCount => false)
		port map(
			Clk => Clk,
			Rst_n => Rst_s_n,
			UseR2 => UseR2,
			UseT2 => UseT2,
			BaudC2 => UART_Clk,
			BaudC1 => OF1,
			SC_Sel => SCON_Sel,
			SB_Sel => SBUF_Sel,
			SC_Wr => SCON_Wr,
			SB_Wr => SBUF_Wr,
			SMOD => SMOD,
			Data_In => IO_WData,
			Data_Out => IO_RData,
			RXD => RXD,
			RXD_IsO => RXD_IsO,
			RXD_O => RXD_O,
			TXD => TXD,
			RI => RI,
			TI => TI);

end;
