//                              -*- Mode: Verilog -*-
// Filename        : cpu_tb_top.sv
// Description     : Top Level test bench for CPU
// Author          : Steve Robichaud
// Created On      : Mon Sep  5 09:41:11 2022
// Last Modified By: 
// Last Modified On: Mon Sep  5 09:41:11 2022
// Update Count    : 0
// Status          : Unknown, Use with caution!

/*=====================================================================
 TOP LEVEL
 =====================================================================*/
`include "interfaces.sv"

`include "all_instructions_test.sv"

`timescale 1ns/1ps

module cpu_tb;
   reg clk;

   always #62.5 clk =~ clk;

   cpu_if _if (clk);
   z80_top u0 (.CLK(clk),
	       .M1(_if.m1),
	       .MREQ(_if.mreq),
	       .RD(_if.rd),
	       .WR(_if.wr),
	       .RFSH(_if.rfsh),
	       .WAIT_N(_if.wait_n),
	       .HALT(_if.halt),
	       .INT(_if.interrupt),
	       .NMI(_if.nmi),
	       .RESET(_if.reset),
	       .BUSREQ(_if.busreq),
	       .BUSACK(_if.busack),
	       .ADDRESS_BUS(_if.addr_bus),
	       .DATA_BUS_I(_if.data_i),
	       .DATA_BUS_O(_if.data_o)
	       );

   initial  begin
      test t0;
      t0 = new(_if);
      t0.run();
      clk        <= 0;
      _if.reset  <= 0;
      #187.5 _if.reset <= 1;
   end // initial begin

endmodule // cpu_tb

      
