--
-- 8051 compatible microcontroller core
--
-- Version : 0222
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
use IEEE.numeric_std.all;
use work.T51_Pack.all;

entity T51 is
	generic(
		DualBus : boolean := false;
		RAMAddressWidth : integer := 8
	);
	port(
		Clk			: in std_logic;
		Rst_n		: in std_logic;
		Ready		: in std_logic;
		ROM_Addr	: out std_logic_vector(15 downto 0);
		ROM_Data	: in std_logic_vector(7 downto 0);
		RAM_Addr	: out std_logic_vector(15 downto 0);
		RAM_RData	: in std_logic_vector(7 downto 0);
		RAM_WData	: out std_logic_vector(7 downto 0);
		RAM_Cycle	: out std_logic;
		RAM_Rd		: out std_logic;
		RAM_Wr		: out std_logic;
		Int_Trig	: in std_logic_vector(6 downto 0);
		Int_Acc		: out std_logic_vector(6 downto 0);
		SFR_Rd_RMW	: out std_logic;
		SFR_Wr		: out std_logic;
		SFR_Addr	: out std_logic_vector(6 downto 0);
		SFR_WData	: out std_logic_vector(7 downto 0);
		SFR_RData	: inout std_logic_vector(7 downto 0)
	);
end T51;

architecture rtl of T51 is

	-- Registers
	signal	ACC				: std_logic_vector(7 downto 0);
	signal	B				: std_logic_vector(7 downto 0);
	signal	PSW				: std_logic_vector(7 downto 1);	-- Bit 0 is parity
	signal	PSW0			: std_logic;
	signal	IP				: std_logic_vector(7 downto 0);
	signal	SP				: unsigned(7 downto 0);
	signal	DPL				: std_logic_vector(7 downto 0);	-- DPTR
	signal	DPH				: std_logic_vector(7 downto 0);	-- DPTR
	signal	PC				: unsigned(15 downto 0);
	signal	P2R				: std_logic_vector(7 downto 0);

	signal	PCC				: std_logic_vector(15 downto 0);
	signal	NPC				: unsigned(15 downto 0);
	signal	OPC				: unsigned(15 downto 0);

	-- ALU signals
	signal	Op_A			: std_logic_vector(7 downto 0);
	signal	Op_B			: std_logic_vector(7 downto 0);
	signal	Mem_A			: std_logic_vector(7 downto 0);
	signal	Mem_B			: std_logic_vector(7 downto 0);
	signal	Old_Mem_B		: std_logic_vector(7 downto 0);
	signal	ACC_Q			: std_logic_vector(7 downto 0);
	signal	B_Q				: std_logic_vector(7 downto 0);
	signal	Res_Bus			: std_logic_vector(7 downto 0);
	signal	Status_D		: std_logic_vector(7 downto 5);
	signal	Status_Wr		: std_logic_vector(7 downto 5);

	-- Misc signals
	signal	Int_AddrA		: std_logic_vector(7 downto 0);
	signal	Int_AddrA_r		: std_logic_vector(7 downto 0);
	signal	Int_AddrB		: std_logic_vector(7 downto 0);

	signal	MCode			: std_logic_vector(3 downto 0);
	signal	FCycle			: std_logic_vector(1 downto 0);

	signal	RET_r			: std_logic;
	signal	RET				: std_logic;

	signal	Ri_Stall		: std_logic;
	signal	PSW_Stall		: std_logic;

	signal	Next_PSW7		: std_logic;
	signal	Next_ACC_Z		: std_logic;
	signal	ACC_Wr			: std_logic;

	signal	SFR_RData_r		: std_logic_vector(7 downto 0);

	signal	Mem_Din			: std_logic_vector(7 downto 0);

	signal	Bit_Pattern		: std_logic_vector(7 downto 0);

	-- Registered instruction words.
	signal	Inst			: std_logic_vector(7 downto 0);
	signal	Inst1			: std_logic_vector(7 downto 0);
	signal	Inst2			: std_logic_vector(7 downto 0);

	-- Control signals
	signal	Rst_r_n			: std_logic;
	signal	Last			: std_logic;
	signal	SFR_Wr_i		: std_logic;
	signal	Mem_Wr			: std_logic;
	signal	J_Skip			: std_logic;
	signal	IPending		: std_logic;
	signal	Int_Trig_r		: std_logic_vector(6 downto 0);
	signal	IStart			: std_logic;
	signal	ICall			: std_logic;
	signal	HPInt			: std_logic;
	signal	LPInt			: std_logic;
	signal	PCPaused		: std_logic_vector(3 downto 0);
	signal	PCPause			: std_logic;
	signal	Inst_Skip		: std_logic;
	signal	Div_Rdy			: std_logic;
	signal	RAM_Rd_i		: std_logic;
	signal	INC_DPTR		: std_logic;
	signal	CJNE			: std_logic;
	signal	DJNZ			: std_logic;

	-- Mux control
	signal	AMux_SFR		: std_logic;
	signal	BMux_Inst2		: std_logic;
	signal	RMux_PCL		: std_logic;
	signal	RMux_PCH		: std_logic;

begin

	Last <= '1' when ICall = '1' and FCycle = "11" else
			'0' when ICall = '1' else
			'1' when MCode(1 downto 0) = FCycle and Ready = '1' else '0';

	process (ROM_Data, ICall, Inst, Inst1, Inst2, Last, FCycle, PSW, Mem_B, SP, Old_Mem_B, Ready, Int_AddrA_r)
	begin
		Int_AddrA <= "--------";
		Int_AddrB <= "--------";
		if Inst(3 downto 0) = "0000" or Inst(3 downto 0) = "0010" then
			if Inst(3 downto 0) = "0000" or (Inst(3 downto 0) = "0010" and (Inst(7) = '1' or Inst(6 downto 4) = "111")) then
				if Inst1(7) = '0' then
					Int_AddrA <= "0010" & Inst1(6 downto 3);
				else
					Int_AddrA <= "1" & Inst1(6 downto 3) & "000";
				end if;
			else
				Int_AddrA <= Inst1;
			end if;
			if Inst = "00010010" or ICall = '1' then
				-- LCALL
				if FCycle = "01" then
					Int_AddrA <= std_logic_vector(SP + 1);
				else
					Int_AddrA <= std_logic_vector(SP + 2);
				end if;
			end if;
			if Inst = "11000000" then
				-- 11000000 2 PUSH  data addr		INC SP: MOV "@SP",<src>
				if FCycle = "10" then
					Int_AddrA <= std_logic_vector(SP);
				else
					Int_AddrA <= Inst1;
				end if;
			end if;
			if Inst = "11010000" then
				-- 11010000 2 POP   data addr		MOV <dest>,"@SP": DEC SP
				if FCycle = "10" then
					Int_AddrA <= Inst1;
				else
					Int_AddrA <= std_logic_vector(SP);
				end if;
			end if;
			if Inst(7 downto 5) = "001" and Inst(3 downto 0) = "0010" then
				-- RET, RETI
				Int_AddrA <= std_logic_vector(SP);
				Int_AddrB <= std_logic_vector(SP - 1);
			end if;
		elsif Inst(4 downto 0) = "10001" then
			-- ACALL
			if FCycle = "01" then
				Int_AddrA <= std_logic_vector(SP + 1);
			else
				Int_AddrA <= std_logic_vector(SP + 2);
			end if;
		elsif Inst(3 downto 0) = "0011" then
			Int_AddrA <= inst1;
		elsif Inst(3 downto 0) = "0100" then
		elsif Inst(3 downto 0) = "0101" then
			if Inst(7 downto 4) = "1000" and FCycle = "11" then
				Int_AddrA <= Inst2;
			else
				Int_AddrA <= Inst1;
			end if;
		elsif Inst(3 downto 1) = "011" then
			if FCycle(1 downto 0) = "01" then
				Int_AddrA <= Mem_B;
			else
				Int_AddrA <= Old_Mem_B;
			end if;
			if Inst(7 downto 4) = "1000" and FCycle = "10" then
				Int_AddrA <= Inst1;
			end if;
			if Inst(7 downto 4) = "1010" and FCycle = "01" then
				Int_AddrA <= ROM_Data;
			end if;
		elsif Inst(3) = '1' then
			Int_AddrA <= "000" & PSW(4 downto 3) & Inst(2 downto 0);
			if Inst(7 downto 4) = "1000" and FCycle = "10" then
				Int_AddrA <= Inst1;
			end if;
			if Inst(7 downto 4) = "1010" and FCycle = "01" then
				Int_AddrA <= ROM_Data;
			end if;
		end if;
		if Last = '1' then
			if ROM_Data(3 downto 1) = "011" then
				Int_AddrB <= "000" & PSW(4 downto 3) & "00" & ROM_Data(0);
			end if;
		end if;
		if Ready = '0' then
			Int_AddrA <= Int_AddrA_r;
		end if;
	end process;
	Op_A <= SFR_RData_r when AMux_SFR = '1' else Mem_A;
	Op_B <= Inst2 when BMux_Inst2 = '1' else Inst1;
	Mem_Din <= PCC(7 downto 0) when RMux_PCL = '1' else
			PCC(15 downto 8) when RMux_PCH = '1' else
			Res_Bus;
	process (Clk)
	begin
		if Clk'event and Clk = '1' then
			AMux_SFR <= '0';
			BMux_Inst2 <= '0';
			RMux_PCL <= '0';
			RMux_PCH <= '0';

			if Int_AddrA(7) = '1' then
				AMux_SFR <= '1';
			end if;
			if Inst(3 downto 1) = "011" then
				if not (Inst(7 downto 4) = "1010" and FCycle = "10") then
					-- Indirect addressing
					AMux_SFR <= '0';
				end if;
			end if;
			if Inst = "11010000" then
				-- 11010000 2 POP   data addr		MOV <dest>,"@SP": DEC SP
				if FCycle = "10" then
					AMux_SFR <= '0';
				end if;
			end if;

			if Inst(3 downto 0) = "0011" or Inst(3 downto 0) = "0101" then
				BMux_Inst2 <= '1';
			end if;

			-- LCALL, ACALL, Int
			if (Inst = "00010010" or Inst(4 downto 0) = "10001" or ICall = '1') then
				if FCycle = "01" then
					RMux_PCL <= '1';
				elsif FCycle = "10" then
					RMux_PCH <= '1';
				end if;
			end if;
		end if;
	end process;
	SFR_Wr <= SFR_Wr_i;
	SFR_Addr <= Int_AddrA(6 downto 0);
	SFR_WData <= Res_Bus;
	SFR_Rd_RMW <= '1' when Last = '1' and MCode(3) = '1' and Int_AddrA(7) = '1' and (Inst(7 downto 4) = "1000" or Inst(3 downto 1) /= "011") else '0';
	process (Clk)
	begin
		if Clk'event and Clk = '1' then
			SFR_Wr_i <= '0';
			Mem_Wr <= '0';
			if Last = '1' and MCode(3) = '1' then
				if Int_AddrA(7) = '1' and (Inst(7 downto 4) = "1000" or Inst(3 downto 1) /= "011") then
					-- Direct addressing
					SFR_Wr_i <= '1';
				else
					Mem_Wr <= '1';
				end if;
			end if;

			-- LCALL, ACALL, Int
			if Ready = '1' and (Inst = "00010010" or Inst(4 downto 0) = "10001" or ICall = '1') then
				if FCycle /= "11" then -- LCALL
					Mem_Wr <= '1';
				end if;
			end if;
		end if;
	end process;

	-- Instruction register
	Inst_Skip <= RET_r or J_Skip; -- '1' when (RET_r = '1' and unsigned(Mem_B) /= PC(15 downto 8)) else J_Skip; ????????????
	Ri_Stall <= '1' when Inst /= "00000000" and
				Int_AddrA = "000" & PSW(4 downto 3) & Inst(2 downto 0) and
				ROM_Data(3 downto 1) = "011" and
				Last = '1' and MCode(3) = '1' else '0';
	PSW_Stall <= '1' when Inst /= "00000000" and
				Int_AddrA = "11100000" and -- to restrictive, will stall on memory access also !!!!!!!!!!
				(ROM_Data(3 downto 1) = "011" or ROM_Data(3) = '1') and
				Last = '1' and MCode(3) = '1' else '0';
	process (Rst_n, Clk)
	begin
		if Rst_n = '0' then
			Rst_r_n <= '0';
			Inst <= (others => '0'); -- Force NOP at reset.
			Inst1 <= (others => '0');
			Inst2 <= (others => '0');
			Bit_Pattern <= "00000000";
		elsif Clk'event and Clk = '1' then
			Rst_r_n <= '1';
			if Ready = '0' then
			elsif Rst_r_n = '0' or Inst_Skip = '1' or IStart = '1' or Ri_Stall = '1' or PSW_Stall = '1' then
				-- Skip/Stall/Flush: NOP insertion
				Inst <= (others => '0');
			elsif Inst = "10000100" and PCPause = '1' then
			else
				if Last = '1' then
					Inst <= ROM_Data;
				end if;
				if FCycle = "01" then
					Inst1 <= ROM_Data;
				end if;
				if FCycle = "10" then
					Inst2 <= ROM_Data;
				end if;
			end if;
			if FCycle = "01" then
				case ROM_Data(2 downto 0) is
				when "000" =>
					Bit_Pattern <= "00000001";
				when "001" =>
					Bit_Pattern <= "00000010";
				when "010" =>
					Bit_Pattern <= "00000100";
				when "011" =>
					Bit_Pattern <= "00001000";
				when "100" =>
					Bit_Pattern <= "00010000";
				when "101" =>
					Bit_Pattern <= "00100000";
				when "110" =>
					Bit_Pattern <= "01000000";
				when others =>
					Bit_Pattern <= "10000000";
				end case;
			end if;
		end if;
	end process;

	-- Accumulator, B and status register
	SFR_RData <= PSW & PSW0 when Int_AddrA = "11010000" else "ZZZZZZZZ";
	SFR_RData <= ACC when Int_AddrA = "11100000" else "ZZZZZZZZ";
	SFR_RData <= B when Int_AddrA = "11110000" else "ZZZZZZZZ";
	PSW0 <= ACC(7) xor ACC(6) xor ACC(5) xor ACC(4) xor ACC(3) xor ACC(2) xor ACC(1) xor ACC(0);
	Next_PSW7 <= Res_Bus(7) when SFR_Wr_i = '1' and Int_AddrA_r = "11010000" else
				Status_D(7) when Status_Wr(7) = '1' else
				PSW(7);
	Next_ACC_Z <= '1' when ACC_Q = "00000000" and ACC_Wr = '1' else
				'1' when ACC = "00000000" else '0';
	process (Rst_n, Clk)
		variable B_Wr : std_logic;
	begin
		if Rst_n = '0' then
			PSW <= "0000000";
			ACC <= "00000000";
			B <= "00000000";
			ACC_Wr <= '0';
			B_Wr := '0';
		elsif Clk'event and Clk = '1' then
			if ACC_Wr = '1' then
				ACC <= ACC_Q;
			end if;
			if B_Wr = '1' then
				B <= B_Q;
			end if;
			if (MCode(2) and Last) = '1' or (Inst = "10000100" and PCPause = '0') then
				ACC_Wr <= '1';
			else
				ACC_Wr <= '0';
			end if;
			if ((Inst = "10000100" and PCPause = '0') or Inst = "10100100") and Last = '1' then --  DIV, MUL
				B_Wr := '1';
			else
				B_Wr := '0';
			end if;

			if SFR_Wr_i = '1' and Int_AddrA_r = "11100000" then
				ACC <= Res_Bus;
			end if;

			if RAM_Rd_i = '1' then
				ACC <= RAM_RData;
			end if;

			if SFR_Wr_i = '1' and Int_AddrA_r = "11110000" then
				B <= Res_Bus;
			end if;

			if Inst(7 downto 5) = "100" and Inst(3 downto 0) = "0011" then -- MOVC
				if FCycle = "11" then
					ACC <= ROM_Data;
				end if;
			end if;

			if SFR_Wr_i = '1' and Int_AddrA_r = "11010000" then
				PSW <= Res_Bus(7 downto 1);
			end if;
			-- CY
			if Status_Wr(7) = '1' then PSW(7) <= Status_D(7); end if;
			-- AC
			if Status_Wr(6) = '1' then PSW(6) <= Status_D(6); end if;
			-- OV
			if Status_Wr(5) = '1' then PSW(2) <= Status_D(5); end if;
		end if;
	end process;

	-- Stack pointer
	SFR_RData <= std_logic_vector(SP) when Int_AddrA = "10000001" else "ZZZZZZZZ";
	process (Rst_n, Clk)
	begin
		if Rst_n = '0' then
			SP <= "00000111";
		elsif Clk'event and Clk = '1' then
			if SFR_Wr_i = '1' and Int_AddrA_r = "10000001" then
				SP <= unsigned(Res_Bus);
			end if;
			if Ready = '1' then
				if Inst(7 downto 5) = "001" and Inst(3 downto 0) = "0010" then
					SP <= SP - 2;
				end if;
				if (Inst = "00010010" or Inst(4 downto 0) = "10001" or ICall = '1') and Last = '1' then
					-- LCALL, ACALL, ICall
					SP <= SP + 2;
				end if;
				if Inst = "11000000" and PCPaused(0) = '1' then
					-- 11000000 2 PUSH  data addr		INC SP: MOV "@SP",<src>
					SP <= SP + 1;
				end if;
				if Inst = "11010000" and Last = '1' then
					-- 11010000 2 POP   data addr		MOV <dest>,"@SP": DEC SP
					SP <= SP - 1;
				end if;
			end if;
		end if;
	end process;

	-- DPTR/RAM_Addr
	RAM_WData <= ACC;
	process (Inst, P2R, DPH, DPL, Int_AddrA_r, SFR_Wr_i, Res_Bus, INC_DPTR, Mem_B)
	begin
		RAM_Addr <= DPH & DPL;
		if Inst(1) = '0' then
			if SFR_Wr_i = '1' and Int_AddrA_r = "10000010" then
				RAM_Addr(7 downto 0) <= Res_Bus;
			end if;
			if SFR_Wr_i = '1' and Int_AddrA_r = "10000011" then
				RAM_Addr(15 downto 8) <= Res_Bus;
			end if;
			-- 10100011 1 INC   DPTR
			if INC_DPTR = '1' then
				RAM_Addr <= std_logic_vector(unsigned(DPH) & unsigned(DPL) + 1);
			end if;
		else
			RAM_Addr <= P2R & Mem_B;
			if SFR_Wr_i = '1' and Int_AddrA_r = "10100000" then
				RAM_Addr(15 downto 8) <= Res_Bus;
			end if;
		end if;
	end process;
	SFR_RData <= DPL when Int_AddrA = "10000010" else "ZZZZZZZZ";
	SFR_RData <= DPH when Int_AddrA = "10000011" else "ZZZZZZZZ";
	RAM_Cycle <= '1' when Inst(7 downto 5) = "111" and Inst(3 downto 2) = "00" and Inst(1 downto 0) /= "01" and PCPaused(0) = '0' else '0';
	RAM_Rd <= RAM_Rd_i;
	process (Rst_n, Clk)
		variable tmp : unsigned(15 downto 0);
	begin
		if Rst_n = '0' then
			P2R <= "11111111";
			DPL <= "00000000";
			DPH <= "00000000";
			INC_DPTR <= '0';
			RAM_Rd_i <= '0';
			RAM_Wr <= '0';
		elsif Clk'event and Clk = '1' then
			if SFR_Wr_i = '1' and Int_AddrA_r = "10100000" then
				P2R <= Res_Bus;
			end if;
			if SFR_Wr_i = '1' and Int_AddrA_r = "10000010" then
				DPL <= Res_Bus;
			end if;
			if SFR_Wr_i = '1' and Int_AddrA_r = "10000011" then
				DPH <= Res_Bus;
			end if;
			if Ready = '1' then
				-- 10010000 3 MOV   DPTR,#data
				if Inst = "10010000" and FCycle = "10" then
					DPH <= Inst1;
				end if;
				if Inst = "10010000" and FCycle = "11" then
					DPL <= Inst2;
				end if;
				-- 10100011 1 INC   DPTR
				if INC_DPTR = '1' then
					tmp := unsigned(DPH) & unsigned(DPL) + 1;
					DPH <= std_logic_vector(tmp(15 downto 8));
					DPL <= std_logic_vector(tmp(7 downto 0));
				end if;
				INC_DPTR <= '0';
				if Inst = "10100011" then
					INC_DPTR <= '1';
				end if;
			end if;
			RAM_Wr <= '0';
			if (Inst(7 downto 2) = "111100" and Inst(1 downto 0) /= "01" and DualBus) or
				(Inst(7 downto 2) = "111100" and Inst(1 downto 0) /= "01" and Ready = '0' and not DualBus) then
				RAM_Wr <= '1';
			end if;
			RAM_Rd_i <= '0';
			if Inst(7 downto 2) = "111000" and Inst(1 downto 0) /= "01" and Ready = '1' then
				RAM_Rd_i <= '1';
			end if;
		end if;
	end process;

	-- Interrupts
	SFR_RData <= IP when Int_AddrA = "10111000" else "ZZZZZZZZ";
	IStart <= Last and IPending and not Inst_Skip;
	process (Rst_n, Clk)
	begin
		if Rst_n = '0' then
			LPInt <= '0';
			HPInt <= '0';
			Int_Acc <= (others => '0');
			Int_Trig_r <= (others => '0');
			IPending <= '0';
			IP <= "00000000";
			ICall <= '0';
		elsif Clk'event and Clk = '1' then
			if SFR_Wr_i = '1' and Int_AddrA_r = "10111000" then
				IP <= Res_Bus;
			end if;
			if Ready = '1' then
				if (Int_Trig and IP(6 downto 0)) /= "0000000" and HPInt = '0' and IPending = '0' and ICall = '0' then
					Int_Trig_r <= Int_Trig and IP(6 downto 0);
					IPending <= '1';
					HPInt <= '1';
				elsif Int_Trig /= "0000000" and LPInt = '0' and HPInt = '0'  and IPending = '0' and ICall = '0' then
					IPending <= '1';
					Int_Trig_r <= Int_Trig;
					LPInt <= '1';
				end if;
				if ICall = '1' then
					IPending <= '0';
				end if;
				if IStart = '1' then
					ICall <= '1';
				end if;
				if ICall = '1' and Last = '1' then
					ICall <= '0';
					Int_Trig_r <= (others => '0');
				end if;
				Int_Acc <= (others => '0');
				if IPending = '1' and ICall = '1' then
					if Int_Trig_r(0) = '1' then Int_Acc(0) <= '1';
					elsif Int_Trig_r(1) = '1' then Int_Acc(1) <= '1';
					elsif Int_Trig_r(2) = '1' then Int_Acc(2) <= '1';
					elsif Int_Trig_r(3) = '1' then Int_Acc(3) <= '1';
					elsif Int_Trig_r(4) = '1' then Int_Acc(4) <= '1';
					elsif Int_Trig_r(5) = '1' then Int_Acc(5) <= '1';
					elsif Int_Trig_r(6) = '1' then Int_Acc(6) <= '1';
					end if;
				end if;
				if Inst = "00110010" then
					if HPInt = '0' then
						LPInt <= '0';
					else
						HPInt <= '0';
					end if;
				end if;
			end if;
		end if;
	end process;

	-- Program counter
	ROM_Addr <= std_logic_vector(NPC);
	process (Rst_n, Clk)
	begin
		if Rst_n = '0' then
			PC <= (others => '0');
			OPC <= (others => '0');
			FCycle <= "01";
			RET_r <= '0';
			PCPaused <= (others => '0');
		elsif Clk'event and Clk = '1' then
			if Ready = '1' then
				PC <= NPC;
				RET_r <= RET;

				if PCPause = '1' then
					PCPaused <= std_logic_vector(unsigned(PCPaused) + 1);
				else
					PCPaused <= (others => '0');
				end if;

				if PCPause = '0' then
					FCycle <= std_logic_vector(unsigned(FCycle) + 1);
					if Last = '1' then
						FCycle <= "01";
					end if;
				end if;

				if Inst(7 downto 5) = "100" and Inst(3 downto 0) = "0011" then -- MOVC
					if FCycle = "01" then
						OPC <= PC;
					end if;
				end if;
			end if;
		end if;
	end process;
	process (FCycle, Inst, Inst2, CJNE, DJNZ, PC, OPC, Inst1, ROM_Data,
			Next_PSW7, Next_ACC_Z, DPL, DPH, ACC, PCPaused, Op_A, Mem_A,Mem_B,
			Bit_Pattern, Ri_Stall, PSW_Stall, RET_r, Div_Rdy, ICall,
			Int_Trig_r, Ready, Rst_r_n)
	begin
		NPC <= PC;
		J_Skip <= '0';
		RET <= '0';
		PCPause <= '0';
		if (Inst(7 downto 5) = "110" and Inst(3 downto 0) = "0000" and FCycle = "01" and PCPaused(0) = '0') or -- PUSH, POP
			(Inst(7 downto 5) = "111" and Inst(3 downto 2) = "00" and Inst(1 downto 0) /= "01" and not DualBus and PCPaused(0) = '0') or -- Single bus MOVX
			(Inst = "10000100" and (PCPaused(3 downto 1) = "000" or Div_Rdy = '0')) then -- DIV
			PCPause <= '1';
		else
			if Ri_Stall = '0' and PSW_Stall = '0' then
				NPC <= PC + 1;
			end if;
		end if;
		-- Single bus MOVX
		if Inst(7 downto 5) = "111" and Inst(3 downto 2) = "00" and Inst(1 downto 0) /= "01" and not DualBus then
			J_Skip <= '1';
		end if;
		-- Return
		if Inst(7 downto 5) = "001" and Inst(3 downto 0) = "0010" then -- RET, RETI
			RET <= '1';
			J_Skip <= '1';
		end if;
		-- MOVC
		if Inst(7 downto 5) = "100" and Inst(3 downto 0) = "0011" and FCycle = "11" then
			NPC <= OPC;
			J_Skip <= '1';
		end if;
		-- 2 byte 8 bit relative jump
		if FCycle = "10" then
			if (Inst = "01000000" and Next_PSW7 = '1') or -- JC
				(Inst = "01010000" and Next_PSW7 = '0') or -- JNC
				(Inst = "01100000" and Next_ACC_Z = '1') or -- JZ
				(Inst = "01110000" and Next_ACC_Z = '0') or -- JNZ
				(Inst(7 downto 3) = "11011" and DJNZ = '1') or -- DJNZ
				Inst = "10000000" then -- SJMP
				NPC <= PC + unsigned(resize(signed(Inst1),16));
				J_Skip <= '1';
			end if;
		end if;
		-- 3 byte 8 bit relative jump
		if FCycle = "11" then
			if (Inst = "00100000" or Inst = "00010000") and (Bit_Pattern and Op_A) /= "00000000" then -- JB, JBC
				NPC <= PC + unsigned(resize(signed(Inst2),16));
				J_Skip <= '1';
			end if;
			if Inst = "00110000" and (Bit_Pattern and Op_A) = "00000000" then -- JNB
				NPC <= PC + unsigned(resize(signed(Inst2),16));
				J_Skip <= '1';
			end if;
			if Inst(7 downto 4) = "1011" and Inst(3 downto 2) /= "00" and CJNE = '1' then -- CJNE
				NPC <= PC + unsigned(resize(signed(Inst2),16));
				J_Skip <= '1';
			end if;
			if Inst = "11010101" and DJNZ = '1' then -- DJNZ
				NPC <= PC + unsigned(resize(signed(Inst2),16));
				J_Skip <= '1';
			end if;
		end if;
		-- 11 bit absolute
		if FCycle = "10" then
			if Inst(4 downto 0) = "00001" or Inst(4 downto 0) = "10001" then
				-- AJMP, ACALL
				NPC(15 downto 11) <= PC(15 downto 11);
				NPC(10 downto 8) <= unsigned(Inst(7 downto 5));
				NPC(7 downto 0) <= unsigned(Inst1);
				J_Skip <= '1';
			end if;
		end if;
		-- 16 bit absolute
		if FCycle = "10" then
			if Inst = "00000010" or Inst = "00010010" then
				-- LJMP, LCALL
				NPC(15 downto 8) <= unsigned(Inst1);
				NPC(7 downto 0) <= unsigned(ROM_Data);
			end if;
			if ICall = '1' then
				NPC <= (1 => '1', 0 => '1', others => '0');
				if Int_Trig_r(1) = '1' then NPC(5 downto 3) <= "001";
				elsif Int_Trig_r(2) = '1' then NPC(5 downto 3) <= "010";
				elsif Int_Trig_r(3) = '1' then NPC(5 downto 3) <= "011";
				elsif Int_Trig_r(4) = '1' then NPC(5 downto 3) <= "100";
				elsif Int_Trig_r(5) = '1' then NPC(5 downto 3) <= "101";
				elsif Int_Trig_r(6) = '1' then NPC(5 downto 3) <= "110";
				end if;
			end if;
		end if;
		-- A+DPTR Absolute
		if Inst = "01110011" then
			-- JMP @A+DPTR
			NPC <= (unsigned(DPH) & unsigned(DPL)) + unsigned(resize(signed(ACC),16));
			J_Skip <= '1';
		end if;

		if Inst(7 downto 5) = "100" and Inst(3 downto 0) = "0011" then -- MOVC
			if FCycle = "10" then
				if Inst(4) = '0' then
					NPC <= unsigned(ACC) + OPC;
				else
					NPC <= unsigned(ACC) + (unsigned(DPH) & unsigned(DPL));
				end if;
			end if;
		end if;

		if RET_r = '1' then -- and unsigned(Mem_A) /= PC(15 downto 8) then ???????????????????????????
			NPC <= unsigned(Mem_A) & unsigned(Mem_B);
		end if;

		if Ready = '0' then
			NPC <= PC;
		end if;
		if Rst_r_n = '0' then
			NPC <= (others => '0');
		end if;
	end process;

	-- ALU
	alu : T51_ALU
		port map(
			Clk => Clk,
			Last => Last,
			OpCode => Inst,
			ACC => ACC,
			B => B,
			IA => Op_A,
			IB => Op_B,
			Bit_Pattern	=> Bit_Pattern,
			CY_In => PSW(7),
			AC_In => PSW(6),
			ACC_Q => ACC_Q,
			B_Q => B_Q,
			IDCPBL_Q =>	Res_Bus,
			Div_Rdy => Div_Rdy,
			CJNE => CJNE,
			DJNZ => DJNZ,
			CY_Out => Status_D(7),
			AC_Out => Status_D(6),
			OV_Out => Status_D(5),
			CY_Wr => Status_Wr(7),
			AC_Wr => Status_Wr(6),
			OV_Wr => Status_Wr(5));

	process (Clk)
	begin
		if Clk'event and Clk = '1' then
			Old_Mem_B <= Mem_B;
			if FCycle = "01" then
				if Inst(1) = '1' then
					PCC <= std_logic_vector(PC + 2);
				else
					PCC <= std_logic_vector(PC + 1);
				end if;
				if ICall = '1' then
					PCC <= std_logic_vector(PC - 1);
				end if;
			end if;
			SFR_RData_r <= SFR_RData;
		end if;
	end process;

	ram : T51_RAM
		generic map(
			RAMAddressWidth => RAMAddressWidth)
		port map(
			Clk => Clk,
			Rst_n => Rst_n,
			ARE => Ready,
			Wr => Mem_Wr,
			DIn => Mem_Din,
			Int_AddrA => Int_AddrA,
			Int_AddrA_r => Int_AddrA_r,
			Int_AddrB => Int_AddrB,
			Mem_A => Mem_A,
			Mem_B => Mem_B);

	process (Inst)
	begin
		case Inst is
		-- 1 downto 0 instruction length
		-- 2 write ACC
		-- 3 write register file
		when "00000000" => MCode <= "0001";	-- 00000000 1 NOP
		when "00000001" => MCode <= "0010";	-- aaa00001 2 AJMP  code addr
		when "00000010" => MCode <= "0011";	-- 00000010 3 LJMP  code addr
		when "00000011" => MCode <= "0101";	-- 00000011 1 RR    A
		when "00000100" => MCode <= "0101";	-- 00000100 1 INC   A
		when "00000101" => MCode <= "1010";	-- 00000101 2 INC   data addr
		when "00000110" => MCode <= "1001";	-- 0000011i 1 INC   @Ri
		when "00000111" => MCode <= "1001";	-- 0000011i 1 INC   @Ri
		when "00001000" => MCode <= "1001";	-- 00001rrr 1 INC   Rn
		when "00001001" => MCode <= "1001";	-- 00001rrr 1 INC   Rn
		when "00001010" => MCode <= "1001";	-- 00001rrr 1 INC   Rn
		when "00001011" => MCode <= "1001";	-- 00001rrr 1 INC   Rn
		when "00001100" => MCode <= "1001";	-- 00001rrr 1 INC   Rn
		when "00001101" => MCode <= "1001";	-- 00001rrr 1 INC   Rn
		when "00001110" => MCode <= "1001";	-- 00001rrr 1 INC   Rn
		when "00001111" => MCode <= "1001";	-- 00001rrr 1 INC   Rn
		when "00010000" => MCode <= "1011";	-- 00010000 3 JBC   bit addr, code addr
		when "00010001" => MCode <= "0010";	-- aaa10001 2 ACALL code addr
		when "00010010" => MCode <= "0011";	-- 00010010 3 LCALL code addr
		when "00010011" => MCode <= "0101";	-- 00010011 1 RRC   A
		when "00010100" => MCode <= "0101";	-- 00010100 1 DEC   A
		when "00010101" => MCode <= "1010";	-- 00010101 2 DEC   data addr
		when "00010110" => MCode <= "1001";	-- 0001011i 1 DEC   @Ri
		when "00010111" => MCode <= "1001";	-- 0001011i 1 DEC   @Ri
		when "00011000" => MCode <= "1001";	-- 00011rrr 1 DEC   Rn
		when "00011001" => MCode <= "1001";	-- 00011rrr 1 DEC   Rn
		when "00011010" => MCode <= "1001";	-- 00011rrr 1 DEC   Rn
		when "00011011" => MCode <= "1001";	-- 00011rrr 1 DEC   Rn
		when "00011100" => MCode <= "1001";	-- 00011rrr 1 DEC   Rn
		when "00011101" => MCode <= "1001";	-- 00011rrr 1 DEC   Rn
		when "00011110" => MCode <= "1001";	-- 00011rrr 1 DEC   Rn
		when "00011111" => MCode <= "1001";	-- 00011rrr 1 DEC   Rn
		when "00100000" => MCode <= "0011";	-- 00100000 3 JB    bit addr, code addr
		when "00100001" => MCode <= "0010";	-- aaa00001 2 AJMP  code addr
		when "00100010" => MCode <= "0001";	-- 00100010 1 RET
		when "00100011" => MCode <= "0101";	-- 00100011 1 RL    A
		when "00100100" => MCode <= "0110";	-- 00100100 2 ADD   A,#data
		when "00100101" => MCode <= "0110";	-- 00100101 2 ADD   A,data addr
		when "00100110" => MCode <= "0101";	-- 0010011i 1 ADD   A,@Ri
		when "00100111" => MCode <= "0101";	-- 0010011i 1 ADD   A,@Ri
		when "00101000" => MCode <= "0101";	-- 00101rrr 1 ADD   A,Rn
		when "00101001" => MCode <= "0101";	-- 00101rrr 1 ADD   A,Rn
		when "00101010" => MCode <= "0101";	-- 00101rrr 1 ADD   A,Rn
		when "00101011" => MCode <= "0101";	-- 00101rrr 1 ADD   A,Rn
		when "00101100" => MCode <= "0101";	-- 00101rrr 1 ADD   A,Rn
		when "00101101" => MCode <= "0101";	-- 00101rrr 1 ADD   A,Rn
		when "00101110" => MCode <= "0101";	-- 00101rrr 1 ADD   A,Rn
		when "00101111" => MCode <= "0101";	-- 00101rrr 1 ADD   A,Rn
		when "00110000" => MCode <= "0011";	-- 00110000 3 JNB   bit addr, code addr
		when "00110001" => MCode <= "0010";	-- aaa10001 2 ACALL code addr
		when "00110010" => MCode <= "0001";	-- 00110010 1 RETI
		when "00110011" => MCode <= "0101";	-- 00110011 1 RLC   A
		when "00110100" => MCode <= "0110";	-- 00110100 2 ADDC  A,#data
		when "00110101" => MCode <= "0110";	-- 00110101 2 ADDC  A,data addr
		when "00110110" => MCode <= "0101";	-- 0011011i 1 ADDC  A,@Ri
		when "00110111" => MCode <= "0101";	-- 0011011i 1 ADDC  A,@Ri
		when "00111000" => MCode <= "0101";	-- 00111rrr 1 ADDC  A,Rn
		when "00111001" => MCode <= "0101";	-- 00111rrr 1 ADDC  A,Rn
		when "00111010" => MCode <= "0101";	-- 00111rrr 1 ADDC  A,Rn
		when "00111011" => MCode <= "0101";	-- 00111rrr 1 ADDC  A,Rn
		when "00111100" => MCode <= "0101";	-- 00111rrr 1 ADDC  A,Rn
		when "00111101" => MCode <= "0101";	-- 00111rrr 1 ADDC  A,Rn
		when "00111110" => MCode <= "0101";	-- 00111rrr 1 ADDC  A,Rn
		when "00111111" => MCode <= "0101";	-- 00111rrr 1 ADDC  A,Rn
		when "01000000" => MCode <= "0010";	-- 01000000 2 JC    code addr
		when "01000001" => MCode <= "0010";	-- aaa00001 2 AJMP  code addr
		when "01000010" => MCode <= "1010";	-- 01000010 2 ORL   data addr,A
		when "01000011" => MCode <= "1011";	-- 01000011 3 ORL   data addr,#data
		when "01000100" => MCode <= "0110";	-- 01000100 2 ORL   A,#data
		when "01000101" => MCode <= "0110";	-- 01000101 2 ORL   A,data addr
		when "01000110" => MCode <= "0101";	-- 0100011i 1 ORL   A,@Ri
		when "01000111" => MCode <= "0101";	-- 0100011i 1 ORL   A,@Ri
		when "01001000" => MCode <= "0101";	-- 01001rrr 1 ORL   A,Rn
		when "01001001" => MCode <= "0101";	-- 01001rrr 1 ORL   A,Rn
		when "01001010" => MCode <= "0101";	-- 01001rrr 1 ORL   A,Rn
		when "01001011" => MCode <= "0101";	-- 01001rrr 1 ORL   A,Rn
		when "01001100" => MCode <= "0101";	-- 01001rrr 1 ORL   A,Rn
		when "01001101" => MCode <= "0101";	-- 01001rrr 1 ORL   A,Rn
		when "01001110" => MCode <= "0101";	-- 01001rrr 1 ORL   A,Rn
		when "01001111" => MCode <= "0101";	-- 01001rrr 1 ORL   A,Rn
		when "01010000" => MCode <= "0010";	-- 01010000 2 JNC   code addr
		when "01010001" => MCode <= "0010";	-- aaa10001 2 ACALL code addr
		when "01010010" => MCode <= "1010";	-- 01010010 2 ANL   data addr,A
		when "01010011" => MCode <= "1011";	-- 01010011 3 ANL   data addr,#data
		when "01010100" => MCode <= "0110";	-- 01010100 2 ANL   A,#data
		when "01010101" => MCode <= "0110";	-- 01010101 2 ANL   A,data addr
		when "01010110" => MCode <= "0101";	-- 0101011i 1 ANL   A,@Ri
		when "01010111" => MCode <= "0101";	-- 0101011i 1 ANL   A,@Ri
		when "01011000" => MCode <= "0101";	-- 01011rrr 1 ANL   A,Rn
		when "01011001" => MCode <= "0101";	-- 01011rrr 1 ANL   A,Rn
		when "01011010" => MCode <= "0101";	-- 01011rrr 1 ANL   A,Rn
		when "01011011" => MCode <= "0101";	-- 01011rrr 1 ANL   A,Rn
		when "01011100" => MCode <= "0101";	-- 01011rrr 1 ANL   A,Rn
		when "01011101" => MCode <= "0101";	-- 01011rrr 1 ANL   A,Rn
		when "01011110" => MCode <= "0101";	-- 01011rrr 1 ANL   A,Rn
		when "01011111" => MCode <= "0101";	-- 01011rrr 1 ANL   A,Rn
		when "01100000" => MCode <= "0010";	-- 01100000 2 JZ    code addr
		when "01100001" => MCode <= "0010";	-- aaa00001 2 AJMP  code addr
		when "01100010" => MCode <= "1010";	-- 01100010 2 XRL   data addr,A
		when "01100011" => MCode <= "1011";	-- 01100011 3 XRL   data addr,#data
		when "01100100" => MCode <= "0110";	-- 01100100 2 XRL   A,#data
		when "01100101" => MCode <= "0110";	-- 01100101 2 XRL   A,data addr
		when "01100110" => MCode <= "0101";	-- 0110011i 1 XRL   A,@Ri
		when "01100111" => MCode <= "0101";	-- 0110011i 1 XRL   A,@Ri
		when "01101000" => MCode <= "0101";	-- 01101rrr 1 XRL   A,Rn
		when "01101001" => MCode <= "0101";	-- 01101rrr 1 XRL   A,Rn
		when "01101010" => MCode <= "0101";	-- 01101rrr 1 XRL   A,Rn
		when "01101011" => MCode <= "0101";	-- 01101rrr 1 XRL   A,Rn
		when "01101100" => MCode <= "0101";	-- 01101rrr 1 XRL   A,Rn
		when "01101101" => MCode <= "0101";	-- 01101rrr 1 XRL   A,Rn
		when "01101110" => MCode <= "0101";	-- 01101rrr 1 XRL   A,Rn
		when "01101111" => MCode <= "0101";	-- 01101rrr 1 XRL   A,Rn
		when "01110000" => MCode <= "0010";	-- 01110000 2 JNZ   code addr
		when "01110001" => MCode <= "0010";	-- aaa10001 2 ACALL code addr
		when "01110010" => MCode <= "0010";	-- 01110010 2 ORL   C, bit addr
		when "01110011" => MCode <= "0001";	-- 01110011 1 JMP   @A+DPTR
		when "01110100" => MCode <= "0110";	-- 01110100 2 MOV   A,#data
		when "01110101" => MCode <= "1011";	-- 01110101 3 MOV   data addr,#data
		when "01110110" => MCode <= "1010";	-- 0111011i 2 MOV   @Ri,#data
		when "01110111" => MCode <= "1010";	-- 0111011i 2 MOV   @Ri,#data
		when "01111000" => MCode <= "1010";	-- 01111rrr 2 MOV   Rn,#data
		when "01111001" => MCode <= "1010";	-- 01111rrr 2 MOV   Rn,#data
		when "01111010" => MCode <= "1010";	-- 01111rrr 2 MOV   Rn,#data
		when "01111011" => MCode <= "1010";	-- 01111rrr 2 MOV   Rn,#data
		when "01111100" => MCode <= "1010";	-- 01111rrr 2 MOV   Rn,#data
		when "01111101" => MCode <= "1010";	-- 01111rrr 2 MOV   Rn,#data
		when "01111110" => MCode <= "1010";	-- 01111rrr 2 MOV   Rn,#data
		when "01111111" => MCode <= "1010";	-- 01111rrr 2 MOV   Rn,#data
		when "10000000" => MCode <= "0010";	-- 10000000 2 SJMP  code addr
		when "10000001" => MCode <= "0010";	-- aaa00001 2 AJMP  code addr
		when "10000010" => MCode <= "0010";	-- 10000010 2 ANL   C,bit addr
		when "10000011" => MCode <= "0011";	-- 10000011 1 MOVC  A,@A+PC
		when "10000100" => MCode <= "0001";	-- 10000100 1 DIV   AB
		when "10000101" => MCode <= "1011";	-- 10000101 3 MOV   data addr,data addr
		when "10000110" => MCode <= "1010";	-- 1000011i 2 MOV   data addr,@Ri
		when "10000111" => MCode <= "1010";	-- 1000011i 2 MOV   data addr,@Ri
		when "10001000" => MCode <= "1010";	-- 10001rrr 2 MOV   data addr,Rn
		when "10001001" => MCode <= "1010";	-- 10001rrr 2 MOV   data addr,Rn
		when "10001010" => MCode <= "1010";	-- 10001rrr 2 MOV   data addr,Rn
		when "10001011" => MCode <= "1010";	-- 10001rrr 2 MOV   data addr,Rn
		when "10001100" => MCode <= "1010";	-- 10001rrr 2 MOV   data addr,Rn
		when "10001101" => MCode <= "1010";	-- 10001rrr 2 MOV   data addr,Rn
		when "10001110" => MCode <= "1010";	-- 10001rrr 2 MOV   data addr,Rn
		when "10001111" => MCode <= "1010";	-- 10001rrr 2 MOV   data addr,Rn
		when "10010000" => MCode <= "0011";	-- 10010000 3 MOV   DPTR,#data
		when "10010001" => MCode <= "0010";	-- aaa10001 2 ACALL code addr
		when "10010010" => MCode <= "1010";	-- 10010010 2 MOV   bit addr,C
		when "10010011" => MCode <= "0011";	-- 10010011 1 MOVC  A,@A+DPTR
		when "10010100" => MCode <= "0110";	-- 10010100 2 SUBB  A,#data
		when "10010101" => MCode <= "0110";	-- 10010101 2 SUBB  A,data addr
		when "10010110" => MCode <= "0101";	-- 1001011i 1 SUBB  A,@Ri
		when "10010111" => MCode <= "0101";	-- 1001011i 1 SUBB  A,@Ri
		when "10011000" => MCode <= "0101";	-- 10011rrr 1 SUBB  A,Rn
		when "10011001" => MCode <= "0101";	-- 10011rrr 1 SUBB  A,Rn
		when "10011010" => MCode <= "0101";	-- 10011rrr 1 SUBB  A,Rn
		when "10011011" => MCode <= "0101";	-- 10011rrr 1 SUBB  A,Rn
		when "10011100" => MCode <= "0101";	-- 10011rrr 1 SUBB  A,Rn
		when "10011101" => MCode <= "0101";	-- 10011rrr 1 SUBB  A,Rn
		when "10011110" => MCode <= "0101";	-- 10011rrr 1 SUBB  A,Rn
		when "10011111" => MCode <= "0101";	-- 10011rrr 1 SUBB  A,Rn
		when "10100000" => MCode <= "0010";	-- 10100000 2 ORL   C,/bit addr
		when "10100001" => MCode <= "0010";	-- aaa00001 2 AJMP  code addr
		when "10100010" => MCode <= "0010";	-- 10100010 2 MOV   C,bit addr
		when "10100011" => MCode <= "0001";	-- 10100011 1 INC   DPTR
		when "10100100" => MCode <= "0101";	-- 10100100 1 MUL   AB
		when "10100101" => MCode <= "0001";	-- 10100101   reserved
		when "10100110" => MCode <= "1010";	-- 1010011i 2 MOV   @Ri,data addr
		when "10100111" => MCode <= "1010";	-- 1010011i 2 MOV   @Ri,data addr
		when "10101000" => MCode <= "1010";	-- 10101rrr 2 MOV   Rn,data addr
		when "10101001" => MCode <= "1010";	-- 10101rrr 2 MOV   Rn,data addr
		when "10101010" => MCode <= "1010";	-- 10101rrr 2 MOV   Rn,data addr
		when "10101011" => MCode <= "1010";	-- 10101rrr 2 MOV   Rn,data addr
		when "10101100" => MCode <= "1010";	-- 10101rrr 2 MOV   Rn,data addr
		when "10101101" => MCode <= "1010";	-- 10101rrr 2 MOV   Rn,data addr
		when "10101110" => MCode <= "1010";	-- 10101rrr 2 MOV   Rn,data addr
		when "10101111" => MCode <= "1010";	-- 10101rrr 2 MOV   Rn,data addr
		when "10110000" => MCode <= "0010";	-- 10110000 2 ANL   C,/bit addr
		when "10110001" => MCode <= "0010";	-- aaa10001 2 ACALL code addr
		when "10110010" => MCode <= "1010";	-- 10110010 2 CPL   bit addr
		when "10110011" => MCode <= "0001";	-- 10110011 1 CPL   C
		when "10110100" => MCode <= "0011";	-- 10110100 3 CJNE  A,#data,code addr
		when "10110101" => MCode <= "0011";	-- 10110101 3 CJNE  A,data addr,code addr
		when "10110110" => MCode <= "0011";	-- 1011011i 3 CJNE  @Ri,#data,code addr
		when "10110111" => MCode <= "0011";	-- 1011011i 3 CJNE  @Ri,#data,code addr
		when "10111000" => MCode <= "0011";	-- 10111rrr 3 CJNE  Rn,#data,code addr
		when "10111001" => MCode <= "0011";	-- 10111rrr 3 CJNE  Rn,#data,code addr
		when "10111010" => MCode <= "0011";	-- 10111rrr 3 CJNE  Rn,#data,code addr
		when "10111011" => MCode <= "0011";	-- 10111rrr 3 CJNE  Rn,#data,code addr
		when "10111100" => MCode <= "0011";	-- 10111rrr 3 CJNE  Rn,#data,code addr
		when "10111101" => MCode <= "0011";	-- 10111rrr 3 CJNE  Rn,#data,code addr
		when "10111110" => MCode <= "0011";	-- 10111rrr 3 CJNE  Rn,#data,code addr
		when "10111111" => MCode <= "0011";	-- 10111rrr 3 CJNE  Rn,#data,code addr
		when "11000000" => MCode <= "1010";	-- 11000000 2 PUSH  data addr
		when "11000001" => MCode <= "0010";	-- aaa00001 2 AJMP  code addr
		when "11000010" => MCode <= "1010";	-- 11000010 2 CLR   bit addr
		when "11000011" => MCode <= "0001";	-- 11000011 1 CLR   C
		when "11000100" => MCode <= "0101";	-- 11000100 1 SWAP  A
		when "11000101" => MCode <= "1110";	-- 11000101 2 XCH   A,data addr
		when "11000110" => MCode <= "1101";	-- 1100011i 1 XCH   A,@Ri
		when "11000111" => MCode <= "1101";	-- 1100011i 1 XCH   A,@Ri
		when "11001000" => MCode <= "1101";	-- 11001rrr 1 XCH   A,Rn
		when "11001001" => MCode <= "1101";	-- 11001rrr 1 XCH   A,Rn
		when "11001010" => MCode <= "1101";	-- 11001rrr 1 XCH   A,Rn
		when "11001011" => MCode <= "1101";	-- 11001rrr 1 XCH   A,Rn
		when "11001100" => MCode <= "1101";	-- 11001rrr 1 XCH   A,Rn
		when "11001101" => MCode <= "1101";	-- 11001rrr 1 XCH   A,Rn
		when "11001110" => MCode <= "1101";	-- 11001rrr 1 XCH   A,Rn
		when "11001111" => MCode <= "1101";	-- 11001rrr 1 XCH   A,Rn
		when "11010000" => MCode <= "1010";	-- 11010000 2 POP   data addr
		when "11010001" => MCode <= "0010";	-- aaa10001 2 ACALL code addr
		when "11010010" => MCode <= "1010";	-- 11010010 2 SETB  bit addr
		when "11010011" => MCode <= "0001";	-- 11010011 1 SETB  C
		when "11010100" => MCode <= "0101";	-- 11010100 1 DA    A
		when "11010101" => MCode <= "1011";	-- 11010101 3 DJNZ  data addr, code addr
		when "11010110" => MCode <= "1101";	-- 1101011i 1 XCHD  A,@Ri
		when "11010111" => MCode <= "1101";	-- 1101011i 1 XCHD  A,@Ri
		when "11011000" => MCode <= "1010";	-- 11011rrr 2 DJNZ  Rn,code addr
		when "11011001" => MCode <= "1010";	-- 11011rrr 2 DJNZ  Rn,code addr
		when "11011010" => MCode <= "1010";	-- 11011rrr 2 DJNZ  Rn,code addr
		when "11011011" => MCode <= "1010";	-- 11011rrr 2 DJNZ  Rn,code addr
		when "11011100" => MCode <= "1010";	-- 11011rrr 2 DJNZ  Rn,code addr
		when "11011101" => MCode <= "1010";	-- 11011rrr 2 DJNZ  Rn,code addr
		when "11011110" => MCode <= "1010";	-- 11011rrr 2 DJNZ  Rn,code addr
		when "11011111" => MCode <= "1010";	-- 11011rrr 2 DJNZ  Rn,code addr
		when "11100000" => MCode <= "0101";	-- 11100000 1 MOVX  A,@DPTR
		when "11100001" => MCode <= "0010";	-- aaa00001 2 AJMP  code addr
		when "11100010" => MCode <= "0101";	-- 1110001i 1 MOVX  A,@Ri
		when "11100011" => MCode <= "0101";	-- 1110001i 1 MOVX  A,@Ri
		when "11100100" => MCode <= "0101";	-- 11100100 1 CLR   A
		when "11100101" => MCode <= "0110";	-- 11100101 2 MOV   A,data addr
		when "11100110" => MCode <= "0101";	-- 1110011i 1 MOV   A,@Ri
		when "11100111" => MCode <= "0101";	-- 1110011i 1 MOV   A,@Ri
		when "11101000" => MCode <= "0101";	-- 11101rrr 1 MOV   A,Rn
		when "11101001" => MCode <= "0101";	-- 11101rrr 1 MOV   A,Rn
		when "11101010" => MCode <= "0101";	-- 11101rrr 1 MOV   A,Rn
		when "11101011" => MCode <= "0101";	-- 11101rrr 1 MOV   A,Rn
		when "11101100" => MCode <= "0101";	-- 11101rrr 1 MOV   A,Rn
		when "11101101" => MCode <= "0101";	-- 11101rrr 1 MOV   A,Rn
		when "11101110" => MCode <= "0101";	-- 11101rrr 1 MOV   A,Rn
		when "11101111" => MCode <= "0101";	-- 11101rrr 1 MOV   A,Rn
		when "11110000" => MCode <= "0001";	-- 11110000 1 MOVX  @DPTR,A
		when "11110001" => MCode <= "0010";	-- aaa10001 2 ACALL code addr
		when "11110010" => MCode <= "0001";	-- 1111001i 1 MOVX  @Ri,A
		when "11110011" => MCode <= "0001";	-- 1111001i 1 MOVX  @Ri,A
		when "11110100" => MCode <= "0101";	-- 11110100 1 CPL   A
		when "11110101" => MCode <= "1010";	-- 11110101 2 MOV   data addr,A
		when "11110110" => MCode <= "1001";	-- 1111011i 1 MOV   @Ri,A
		when "11110111" => MCode <= "1001";	-- 1111011i 1 MOV   @Ri,A
		when "11111000" => MCode <= "1001";	-- 11111rrr 1 MOV   Rn,A
		when "11111001" => MCode <= "1001";	-- 11111rrr 1 MOV   Rn,A
		when "11111010" => MCode <= "1001";	-- 11111rrr 1 MOV   Rn,A
		when "11111011" => MCode <= "1001";	-- 11111rrr 1 MOV   Rn,A
		when "11111100" => MCode <= "1001";	-- 11111rrr 1 MOV   Rn,A
		when "11111101" => MCode <= "1001";	-- 11111rrr 1 MOV   Rn,A
		when "11111110" => MCode <= "1001";	-- 11111rrr 1 MOV   Rn,A
		when "11111111" => MCode <= "1001";	-- 11111rrr 1 MOV   Rn,A
		when others => MCode <= "----";
		end case;
	end process;

end;
