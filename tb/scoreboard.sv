// Get value from monitor and score results

class scoreboard;

   // mailbox handle
   mailbox mon2scb;

   time    instru_timer_1, instru_timer_2;
   int     errors;
   int     reg_used;
   int     check_prefix;

   bit 	   skip_first;
   bit     check_BC, check_DE, check_HL, check_SP;
   bit     check_inc_BC, check_dec_BC;
   bit     check_inc_DE, check_dec_DE;
   bit     check_inc_HL, check_dec_HL;
   bit     check_inc_SP, check_dec_SP;
   bit     check_inc_PC;
   bit     check_inc_B, check_dec_B, check_b;
   bit     check_A, check_c, check_d, check_e, check_h, check_l;
   bit     check_rlca, check_rrca, check_rla, check_rra;
   bit     check_scf;
   bit     check_ccf;
   bit     check_large_mem;	 
   bit     last_m1;
   bit     flags_result;
   bit 	   toggle;
   bit     jump_add, jump_a16, jump_hl, call_a16, check_ret;
   bit     prefix, check_push, check_pop;
   bit     load_mem_fast, check_mem_fast, load_mem_high, check_hl_sp, check_sp_hl, load_a_high;

   logic [1:0] load_a_fast;
   logic [3:0]  check_reset;
   logic [3:0]  check_16_bit_add, check_not_reg, operator;
   logic [1:0]  check_inc_dec_a, check_inc_dec_c, check_inc_dec_d, check_inc_dec_e, check_inc_dec_h, check_inc_dec_l, check_inc_dec_mem;
   logic [15:0] data_in_check, last_rd_addr, last_wr_addr;
   logic [15:0] pre_op_BC, pre_op_DE, pre_op_PC, pre_op_HL, pre_op_SP;
   logic [7:0]  pre_op_A, pre_op_B, pre_op_C, pre_op_D, pre_op_E, pre_op_H, pre_op_L, pre_op_flags, operand;
   logic [6:0] 	check_ld_command;
   logic [2:0] 	check_mem, check_a_mem;

   // constructor
   function new(mailbox mon2scb);
      this.mon2scb	= mon2scb;
      check_reset       = 0;
      check_ret         = 0;
      check_push        = 0;
      load_mem_fast     = 0;
      check_mem_fast    = 0;
      load_mem_high     = 0;
      check_hl_sp       = 0;
      check_sp_hl       = 0;
      load_a_high       = 0;
      load_a_fast       = 0;
      check_pop         = 0;
      call_a16          = 0;
      prefix            = 0;
      jump_a16          = 0;
      jump_hl           = 0;
      skip_first	= 0;
      check_BC		= 0;
      check_scf         = 0;
      check_ccf         = 0;
      check_DE		= 0;
      check_SP          = 0;
      check_inc_BC	= 0;
      check_dec_BC	= 0;
      check_inc_DE	= 0;
      check_dec_DE	= 0;
      check_inc_HL      = 0;
      check_dec_HL      = 0;
      check_inc_SP      = 0;
      check_dec_SP      = 0;
      check_inc_B	= 0;
      check_dec_B	= 0;
      check_b		= 0;
      check_A		= 0;
      check_d           = 0;
      check_e           = 0;
      check_h           = 0;
      check_l           = 0;
      check_rla         = 0;
      check_rra         = 0;
      check_rlca	= 0;
      data_in_check	= 0;
      check_16_bit_add	= 0;
      check_inc_dec_mem = 0;
      last_m1		= 1;
      pre_op_BC		= 0;
      pre_op_DE         = 0;
      pre_op_PC         = 0;
      pre_op_HL         = 0;
      pre_op_SP         = 0;
      pre_op_A		= 0;
      pre_op_C		= 0;
      pre_op_D		= 0;
      pre_op_E		= 0;
      pre_op_H		= 0;
      pre_op_L		= 0;
      check_inc_dec_a	= 0;
      check_inc_dec_c	= 0;
      check_inc_dec_d	= 0;
      check_inc_dec_e	= 0;
      check_inc_dec_h	= 0;
      check_inc_dec_l	= 0;
      check_mem		= 0;
      check_large_mem	= 0;
      errors		= 0;
      flags_result	= 0;
      toggle		= 0;
      check_c		= 0;
      check_rrca	= 0;
      check_a_mem	= 0;
      jump_add          = 0;
      check_not_reg     = 0;
   endfunction // new

   /*********************************************************
    -- Check flags task
    *********************************************************/
   task check_flags(input logic[7:0] expected, flags_to_check);
      flags_result = 0;
      if (expected == (flags_to_check & cpu_tb.u0.flags_reg)) begin
	 $display("Flags set CORRECT");
      end else begin
	 $display("Flags not set INCORRECT: Expected %X; Recieved %X", expected, flags_to_check & cpu_tb.u0.flags_reg);
	 errors++;
	 flags_result = 1;
      end
   endtask

   /*********************************
   * Main Task 
   **********************************/
   task main;
      transaction trans;
      forever begin
	 mon2scb.get(trans);

	 // Update a 16 bit data tracker for rd operations
	 if (trans.rd == 0) begin
	   data_in_check = {trans.data_i, data_in_check[15:8]};
	    last_rd_addr = trans.addr_bus;
	 end

	 // Grab write address
	 if (trans.wr == 0) begin
	    last_wr_addr = trans.addr_bus;
	    if (load_mem_fast == 1) begin
	       load_mem_fast = 0;
	       if ((trans.data_o == cpu_tb.u0.accumulator_reg) && (trans.addr_bus == {8'hFF, data_in_check[15:8]})) begin
		  $display("Load Mem Fast CORRECT");
	       end else begin
		  $display("Load Mem Fast INCORRECT, %X %X %X %X", trans.data_o, cpu_tb.u0.accumulator_reg, trans.addr_bus, {8'hff, data_in_check[15:8]});
		  errors++;
	       end
	    end
	    if (check_mem_fast == 1) begin
	       check_mem_fast = 0;
	       if ((trans.data_o == cpu_tb.u0.accumulator_reg) && (trans.addr_bus == {8'hFF, cpu_tb.u0.c_reg})) begin
		  $display("Load Mem Fast CORRECT");
	       end else begin
		  $display("Load Mem Fast INCORRECT, %X %X %X %X", trans.data_o, cpu_tb.u0.accumulator_reg, trans.addr_bus, {8'hff, cpu_tb.u0.c_reg});
		  errors++;
	       end
	    end
	    if (load_mem_high == 1) begin
	       load_mem_high = 0;
	       if ((trans.data_o == cpu_tb.u0.accumulator_reg) && (trans.addr_bus == data_in_check)) begin
		  $display("Load Mem Fast CORRECT");
	       end else begin
		  $display("Load Mem Fast INCORRECT, %X %X %X %X", trans.data_o, cpu_tb.u0.accumulator_reg, trans.addr_bus, data_in_check);
		  errors++;
	       end
	    end
	 end
	
	 // Check memory write operations
	 if (trans.wr == 0 && check_mem[2] == 1) begin
	    case (check_mem[1:0])
	      0 : begin // BC
		 if (trans.addr_bus == cpu_tb.u0.bc_reg && trans.data_o == cpu_tb.u0.accumulator_reg)
		   $display("Wrote to mem location %X, value %X CORRECTLY", trans.addr_bus, trans.data_o);
		 else begin
		    $display("Wrote to mem location %X, value %X INCORRECTLY; Expected %X, %X", trans.addr_bus, trans.data_o, cpu_tb.u0.bc_reg, cpu_tb.u0.accumulator_reg);
		    errors++;
		 end
	      end
	      1 : begin // DE
		 if (trans.addr_bus == cpu_tb.u0.de_reg && trans.data_o == cpu_tb.u0.accumulator_reg)
		   $display("Wrote to mem location %X, value %X CORRECTLY", trans.addr_bus, trans.data_o);
		 else begin
		    $display("Wrote to mem location %X, value %X INCORRECTLY; Expected %X, %X", trans.addr_bus, trans.data_o, cpu_tb.u0.de_reg, cpu_tb.u0.accumulator_reg);
		    errors++;
		 end
	      end
	      2 : begin // HL (Using pre op version because some OPs increment HL at the same time)
		 if (trans.addr_bus == pre_op_HL && trans.data_o == cpu_tb.u0.accumulator_reg)
		   $display("Wrote to mem location %X, value %X CORRECTLY", trans.addr_bus, trans.data_o);
		 else begin
		    $display("Wrote to mem location %X, value %X INCORRECTLY; Expected %X, %X", trans.addr_bus, trans.data_o, pre_op_HL, cpu_tb.u0.accumulator_reg);
		    errors++;
		 end
	      end
	      3 : begin // Mem
		 if (check_inc_dec_mem == 0) begin
		    if (trans.addr_bus == pre_op_HL && trans.data_o == data_in_check[15:8])
		      $display("Wrote to mem location %X, value %X CORRECTLY", trans.addr_bus, trans.data_o);
		    else begin
		       $display("Wrote to mem location %X, value %X INCORRECTLY; Expected %X, %X", trans.addr_bus, trans.data_o, pre_op_HL, data_in_check[15:8]);
		       errors++;
		    end
		 end else if (check_inc_dec_mem == 2) begin
		    check_flags(8'h0, 8'hE0);
		    if (trans.addr_bus == pre_op_HL && trans.data_o == data_in_check[15:8]+1)
		      $display("Wrote to mem location %X, value %X CORRECTLY", trans.addr_bus, trans.data_o);
		    else begin
		       $display("Wrote to mem location %X, value %X INCORRECTLY; Expected %X, %X", trans.addr_bus, trans.data_o, pre_op_HL, data_in_check[15:8]+1);
		       errors++;
		    end
		 end else begin
		    check_flags(8'h40, 8'hE0);
		    if (trans.addr_bus == pre_op_HL && trans.data_o == data_in_check[15:8]-1)
		      $display("Wrote to mem location %X, value %X CORRECTLY", trans.addr_bus, trans.data_o);
		    else begin
		       $display("Wrote to mem location %X, value %X INCORRECTLY; Expected %X, %X", trans.addr_bus, trans.data_o, pre_op_HL, data_in_check[15:8]-1);
		       errors++;
		    end
		 end
	      end
	    endcase // case (check_mem[1:0])
	    check_mem = 0;
	 end

	 if (trans.wr == 0 && check_large_mem == 1) begin
	    if (toggle == 0) begin
	       toggle = 1;
	       if (trans.addr_bus == data_in_check && trans.data_o == cpu_tb.u0.u_alu.stack_pointer[7:0])
		 $display("Lower SP write to mem CORRECT");
	       else begin
		 $display("Lower SP write to mem INCORRECT; Addr Expected %X Recieved %X; Data Expected %X Recieved %X", data_in_check, trans.addr_bus, cpu_tb.u0.u_alu.stack_pointer[7:0], trans.data_o);
	       errors++;
	       end
	    end else begin
	       toggle = 0;
	       check_large_mem = 0;
	       if (trans.addr_bus == data_in_check && trans.data_o == cpu_tb.u0.u_alu.stack_pointer[15:8])
		 $display("Upper SP write to mem CORRECT");
	       else begin
		  $display("Upper SP write to mem INCORRECT; Addr Expected %X Recieved %X; Data Expected %X Recieved %X", data_in_check, trans.addr_bus, cpu_tb.u0.u_alu.stack_pointer[15:8], trans.data_o);
		  errors++;
	       end
	    end // if (toggle == 0)
	 end // if (trans.wr == 0 && check_large_mem == 1)
	 
	 
	    
	 /*****************************************************
	  *  Checks that occur as new command starts
	  *****************************************************/
	 if (trans.m1 == 0 && last_m1 == 1) begin
	    
	    $display("----------------------");
	    $display("      SCORECARD        ");
	    trans.display("");
	    
	    // **********************************************
	    // Check Program Counter and Address bus
	    // **********************************************
	    // Subtract 1 from PC since check occurs after increment
	    if (( trans.addr_bus == cpu_tb.u0.u_control.program_counter-1 ) ) begin
	       $display("PC sent CORRECT");
	    end else begin
	       $display("PC sent INCORRECT");
	       errors++;
	    end
	    
	    // **********************************************
	    // Check BC
	    // **********************************************
	    if (check_BC == 1) begin
	       check_BC = 0;
	       if (data_in_check == cpu_tb.u0.bc_reg) begin
		  $display("Data register BC set CORRECTLY : %X", data_in_check);
	       end else begin
		  $display("Data register BC set INCORRECTLY; Expected : %X, Recieved : %X", data_in_check, cpu_tb.u0.bc_reg);
		  errors++;
	       end
	    end

	    if (check_inc_BC == 1) begin
	       check_inc_BC = 0;
	       if (pre_op_BC+1 == cpu_tb.u0.bc_reg)
		 $display("BC increment CORRECT");
	       else begin
		 $display("BC incrment INCORRECT; expected : %X, recieved : $X", pre_op_BC+1, cpu_tb.u0.bc_reg);
		  errors++;
	       end
	    end

	    if (check_dec_BC == 1) begin
	       check_dec_BC = 0;
	       if (pre_op_BC-1 == cpu_tb.u0.bc_reg)
		 $display("BC increment CORRECT");
	       else begin
		 $display("BC incrment INCORRECT; expected : %X, recieved : $X", pre_op_BC-1, cpu_tb.u0.bc_reg);
		  errors++;
	       end
	    end

	    // **********************************************
	    // Check DE
	    // **********************************************
	    if (check_DE == 1) begin
	       check_DE = 0;
	       if (data_in_check == cpu_tb.u0.de_reg) begin
		  $display("Data register DE set CORRECTLY : %X", data_in_check);
	       end else begin
		  $display("Data register DE set INCORRECTLY; Expected : %X, Recieved : %X", data_in_check, cpu_tb.u0.de_reg);
		  errors++;
	       end
	    end

	    if (check_inc_DE == 1) begin
	       check_inc_DE = 0;
	       if (pre_op_DE+1 == cpu_tb.u0.de_reg)
		 $display("DE increment CORRECT");
	       else begin
		 $display("DE incrment INCORRECT; expected : %X, recieved : $X", pre_op_DE+1, cpu_tb.u0.de_reg);
		  errors++;
	       end
	    end

	    if (check_dec_DE == 1) begin
	       check_dec_DE = 0;
	       if (pre_op_DE-1 == cpu_tb.u0.de_reg)
		 $display("DE increment CORRECT");
	       else begin
		 $display("DE incrment INCORRECT; expected : %X, recieved : $X", pre_op_DE-1, cpu_tb.u0.de_reg);
		  errors++;
	       end
	    end

	    // **********************************************
	    // Check HL
	    // **********************************************
	    if (check_HL == 1) begin
	       check_HL = 0;
	       if (data_in_check == cpu_tb.u0.hl_reg) begin
		  $display("Data register HL set CORRECTLY : %X", data_in_check);
	       end else begin
		  $display("Data register HL set INCORRECTLY; Expected : %X, Recieved : %X", data_in_check, cpu_tb.u0.hl_reg);
		  errors++;
	       end
	    end

	    if (check_inc_HL == 1) begin
	       check_inc_HL = 0;
	       if (pre_op_HL+1 == cpu_tb.u0.hl_reg)
		 $display("HL increment CORRECT");
	       else begin
		 $display("HL incrment INCORRECT; expected : %X, recieved : $X", pre_op_HL+1, cpu_tb.u0.hl_reg);
		  errors++;
	       end
	    end

	    if (check_dec_HL == 1) begin
	       check_dec_HL = 0;
	       if (pre_op_HL-1 == cpu_tb.u0.hl_reg)
		 $display("HL decrement CORRECT");
	       else begin
		 $display("HL decrment INCORRECT; expected : %X, recieved : $X", pre_op_HL-1, cpu_tb.u0.hl_reg);
		  errors++;
	       end
	    end

	    if (check_hl_sp == 1) begin
	       check_hl_sp = 0;
	       if (cpu_tb.u0.hl_reg == (cpu_tb.u0.u_alu.stack_pointer+data_in_check[15:8])) begin
		  $display("LD HL SP+r8 CORRECT");
	       end else begin
		  $display("LD HL SP+r8 INCORRECT, %X %X %X", cpu_tb.u0.hl_reg, cpu_tb.u0.u_alu.stack_pointer, data_in_check[15:8]);
		  errors++;
	       end
	    end

	    // **********************************************
	    // Check SP
	    // **********************************************
	    if (check_SP == 1) begin
	       check_SP = 0;
	       if (data_in_check == cpu_tb.u0.u_alu.stack_pointer) begin
		  $display("Data register SP set CORRECTLY : %X", data_in_check);
	       end else begin
		  $display("Data register SP set INCORRECTLY; Expected : %X, Recieved : %X", data_in_check, cpu_tb.u0.u_alu.stack_pointer);
		  errors++;
	       end
	    end

	    if (check_inc_SP == 1) begin
	       check_inc_SP = 0;
	       if (pre_op_SP+1 == cpu_tb.u0.u_alu.stack_pointer)
		 $display("SP increment CORRECT");
	       else begin
		 $display("SP incrment INCORRECT; expected : %X, recieved : $X", pre_op_SP+1, cpu_tb.u0.u_alu.stack_pointer);
		  errors++;
	       end
	    end

	    if (check_dec_SP == 1) begin
	       check_dec_SP = 0;
	       if (pre_op_SP-1 == cpu_tb.u0.u_alu.stack_pointer)
		 $display("SP decrement CORRECT");
	       else begin
		 $display("SP decrment INCORRECT; expected : %X, recieved : $X", pre_op_SP-1, cpu_tb.u0.u_alu.stack_pointer);
		  errors++;
	       end
	    end

	    if (check_sp_hl == 1) begin
	       check_sp_hl = 0;
	       if (cpu_tb.u0.hl_reg == cpu_tb.u0.u_alu.stack_pointer)
		 $display("LD SP HL CORRECT");
	       else begin
		 $display("LD SP HL INCORRECT");
		  errors++;
	       end
	    end
	    
	    // ****************************************************
	    // Check A
	    // *****************************************************

	    if (check_rlca == 1) begin
	       check_rlca = 0;
	       check_flags({3'h0, pre_op_A[7], 4'h0}, 8'hF0);
	       if ((cpu_tb.u0.accumulator_reg == {pre_op_A[6:0], pre_op_A[7]}) &&
		   (flags_result == 0)) begin
		   $display("RLCA CORRECT");
		   end else begin
		      $display("RLCA INCORRECT; expected %X, recieved %X, flags %X", {pre_op_A[6:0], pre_op_A[7]}, cpu_tb.u0.accumulator_reg, cpu_tb.u0.flags_reg);
		      errors++;
		   end
	    end // if (check_rlca == 1)

	    if (check_rla == 1) begin
	       check_rla = 0;
	       check_flags({3'h0, pre_op_A[7], 4'h0}, 8'hF0);
	       if ((cpu_tb.u0.accumulator_reg == {pre_op_A[6:0], pre_op_flags[4]}) &&
		   (flags_result == 0)) begin
		   $display("RLA CORRECT");
		   end else begin
		      $display("RLA INCORRECT; expected %X, recieved %X, flags %X", {pre_op_A[6:0], pre_op_flags[4]}, cpu_tb.u0.accumulator_reg, cpu_tb.u0.flags_reg);
		      errors++;
		   end
	    end

	    if (check_rrca == 1) begin
	       check_rrca = 0;
	       check_flags({3'h0, pre_op_A[0], 4'h0}, 8'hF0);
	       if (cpu_tb.u0.accumulator_reg == {pre_op_A[0], pre_op_A[7:1]})
		  $display("RRCA CORRECT");
	       else begin
		  $display("RRCA INCORRECT");
	    	  errors++;
	       end
	    end

	    if (check_rra == 1) begin
	       check_rra = 0;
	       check_flags({3'h0, pre_op_A[0], 4'h0}, 8'hF0);
	       if (cpu_tb.u0.accumulator_reg == {pre_op_flags[4], pre_op_A[7:1]})
		  $display("RRA CORRECT");
	       else begin
		  $display("RRA INCORRECT");
	    	  errors++;
	       end
	    end

	    if (check_16_bit_add != 0) begin
	       case (check_16_bit_add)
		 4'h8 : begin
		    check_flags(8'h0, 8'h70);
		    if (cpu_tb.u0.hl_reg == pre_op_HL+pre_op_BC)
		      $display("Added BC and HL CORRECT");
		    else begin
		       $display("Added BC and HL INCORRECT");
		       errors++;
		    end
		 end
		 4'h9 : begin
		    check_flags(8'h30, 8'h70);
		    if (cpu_tb.u0.hl_reg == pre_op_HL+pre_op_DE)
		      $display("Added DE and HL CORRECT");
		    else begin
		       $display("Added DE and HL INCORRECT");
		       errors++;
		    end
		 end
		 4'ha : begin
		    check_flags(8'h10, 8'h70);
		    if (cpu_tb.u0.hl_reg == pre_op_HL+pre_op_HL)
		      $display("Added HL and HL CORRECT");
		    else begin
		       $display("Added HL and HL INCORRECT");
		       errors++;
		    end
		 end
	       endcase // case check_16_bit_add
	       
	       check_16_bit_add = 0;
	    end // if (check_16_bit_add /= 0)

	    if (check_a_mem != 0) begin
	       case (check_a_mem[1:0])
		 0 : begin // BC
		    if (cpu_tb.u0.accumulator_reg == data_in_check[15:8] && last_rd_addr == cpu_tb.u0.bc_reg)
		      $display("Loaded A from Mem 'CORRECT'");
		    else begin
		       $display("Loaded A from Mem INCORRECT, %X %X %X %X", data_in_check[15:8], cpu_tb.u0.accumulator_reg, cpu_tb.u0.bc_reg, last_rd_addr);
		       errors++;
		    end
		 end
		 1 : begin // DE
		    if (cpu_tb.u0.accumulator_reg == data_in_check[15:8] && last_rd_addr == cpu_tb.u0.de_reg)
		      $display("Loaded A from Mem 'CORRECT'");
		    else begin
		       $display("Loaded A from Mem INCORRECT, %X %X %X %X", data_in_check[15:8], cpu_tb.u0.accumulator_reg, cpu_tb.u0.de_reg, last_rd_addr);
		       errors++;
		    end
		 end
		 2: begin
		    if (cpu_tb.u0.accumulator_reg == data_in_check[15:8] && last_rd_addr == pre_op_HL)
		      $display("Loaded A from Mem 'CORRECT'");
		    else begin
		       $display("Loaded A from Mem INCORRECT, %X %X %X %X", data_in_check[15:8], cpu_tb.u0.accumulator_reg, pre_op_HL, last_rd_addr);
		       errors++;
		    end
		 end
		 3 : begin
		 end
	       endcase
	       check_a_mem = 0;
	    end // if (check_a_mem != 0)

	    if (check_inc_dec_a[1] == 1) begin
	       if (check_inc_dec_a[0] == 1) begin
		  check_flags(8'h0, 8'hE0);
		  if (pre_op_A+1 == cpu_tb.u0.accumulator_reg)
		    $display("INC A CORRECT");
		  else begin
		     $display("INC A INCORRECT");
		     errors++;
		  end
	       end else begin
		  check_flags(8'hc0, 8'hE0);
		  if (pre_op_A-1 == cpu_tb.u0.accumulator_reg)
		    $display("DEC A CORRECT");
		  else begin
		     $display("DEC A INCORRECT");
		     errors++;
		  end
	       end // else: !if(check_inc_dec_c[0] == 1)
	       check_inc_dec_a = 0;
	    end // if (check_inc_dec_c[1] == 1)

	    if (load_a_fast != 0) begin
	       if (load_a_fast == 1) begin // d8
		  if ((cpu_tb.u0.accumulator_reg == data_in_check[15:8]) && (last_rd_addr == {8'hff, data_in_check[7:0]}))
		    $display("LOAD A d8 CORRECT");
		  else begin
		     $display("LOAD A d8 INCORRECT, %X %X  %X %X", cpu_tb.u0.accumulator_reg, data_in_check[15:8], last_rd_addr, {8'hff, data_in_check[7:0]});
		     errors++;
		  end
	       end else if (load_a_fast == 2) begin // (C)
		  if ((cpu_tb.u0.accumulator_reg == data_in_check[15:8]) && (last_rd_addr == {8'hff, cpu_tb.u0.c_reg}))
		      $display("LOAD A (C) CORRECT");
		  else begin
		     $display("LOAD A (C) INCORRECT");
		     errors++;
		  end
	       end
	       load_a_fast = 0;
	    end // if (load_a_fast != 0)

	    if (load_a_high == 1) begin // (C)
	       load_a_high = 0;
	       if ((cpu_tb.u0.accumulator_reg == data_in_check[15:8]) && (last_rd_addr == 16'hfafa))
		 $display("LOAD A (a16) CORRECT");
	       else begin
		  $display("LOAD A (a16) INCORRECT %X %X %X %X", cpu_tb.u0.accumulator_reg, data_in_check[15:8], last_rd_addr, 16'hfafa);
		  errors++;
	       end
	    end
	    	    
	    // *********************************************************
	    // Check B
	    // *********************************************************
	    if (check_inc_B == 1) begin
	       check_inc_B = 0;
	       check_flags(8'h0, 8'hE0);
	       if ((pre_op_BC[15:8]+1 == cpu_tb.u0.b_reg) && flags_result == 0)
		 $display("Incremented B CORRECT");
	       else begin
		  $display("Incremented B INCORRECT");
		  errors++;
	       end
	    end

	    if (check_dec_B == 1) begin
	       check_dec_B = 0;
	       check_flags(8'h40, 8'hE0);
	       if ((pre_op_BC[15:8]-1 == cpu_tb.u0.b_reg) && flags_result == 0)
		 $display("Decremented B CORRECT");
	       else begin
		  $display("Decremented B INCORRECT");
		  errors++;
	       end
	    end

	    if (check_b == 1) begin
	       check_b = 0;
	       if (cpu_tb.u0.b_reg == data_in_check[15:8]) begin
		  $display("Loaded reg B CORRECT");
	       end else begin
		  $display("Loaded reg B INCORRECT, expected %X, received %X", data_in_check[15:8], cpu_tb.u0.b_reg);
		  errors++;
	       end
	    end

	    // *********************************************************
	    // Check C
	    // *********************************************************
	    if (check_inc_dec_c[1] == 1) begin
	       if (check_inc_dec_c[0] == 1) begin
		  check_flags(8'h0, 8'hE0);
		  if (pre_op_C+1 == cpu_tb.u0.c_reg)
		    $display("INC C CORRECT");
		  else begin
		     $display("INC C INCORRECT");
		     errors++;
		  end
	       end else begin
		  check_flags(8'h40, 8'hE0);
		  if (pre_op_C-1 == cpu_tb.u0.c_reg)
		    $display("DEC C CORRECT");
		  else begin
		     $display("DEC C INCORRECT");
		     errors++;
		  end
	       end // else: !if(check_inc_dec_c[0] == 1)
	       check_inc_dec_c = 0;
	    end // if (check_inc_dec_c[1] == 1)

	    if (check_c == 1) begin
	       check_c = 0;
	       if (cpu_tb.u0.c_reg == data_in_check[15:8]) begin
		  $display("Load C from Program CORRECT");
	       end else begin
		  $display("Load C from Program INCORRECT, %X %X", data_in_check[15:8], cpu_tb.u0.c_reg);
		  errors++;
	       end 
	    end	

	    // *********************************************************
	    // Check D
	    // *********************************************************
	    if (check_inc_dec_d[1] == 1) begin
	       if (check_inc_dec_d[0] == 1) begin
		  check_flags(8'h0, 8'hE0);
		  if (pre_op_D+1 == cpu_tb.u0.d_reg)
		    $display("INC D CORRECT");
		  else begin
		     $display("INC D INCORRECT");
		     errors++;
		  end
	       end else begin
		  check_flags(8'h40, 8'hE0);
		  if (pre_op_D-1 == cpu_tb.u0.d_reg)
		    $display("DEC D CORRECT");
		  else begin
		     $display("DEC D INCORRECT");
		     errors++;
		  end
	       end // else: !if(check_inc_dec_c[0] == 1)
	       check_inc_dec_d = 0;
	    end // if (check_inc_dec_c[1] == 1)

	    if (check_d == 1) begin
	       check_d = 0;
	       if (cpu_tb.u0.d_reg == data_in_check[15:8]) begin
		  $display("Load D from Program CORRECT");
	       end else begin
		  $display("Load D from Program INCORRECT, %X %X", data_in_check[15:8], cpu_tb.u0.d_reg);
		  errors++;
	       end 
	    end	

	    // *********************************************************
	    // Check E
	    // *********************************************************
	    if (check_inc_dec_e[1] == 1) begin
	       if (check_inc_dec_e[0] == 1) begin
		  check_flags(8'h0, 8'hE0);
		  if (pre_op_E+1 == cpu_tb.u0.e_reg)
		    $display("INC E CORRECT");
		  else begin
		     $display("INC E INCORRECT");
		     errors++;
		  end
	       end else begin
		  check_flags(8'h40, 8'hE0);
		  if (pre_op_E-1 == cpu_tb.u0.e_reg)
		    $display("DEC E CORRECT");
		  else begin
		     $display("DEC E INCORRECT");
		     errors++;
		  end
	       end // else: !if(check_inc_dec_c[0] == 1)
	       check_inc_dec_e = 0;
	    end // if (check_inc_dec_c[1] == 1)

	    if (check_e == 1) begin
	       check_e = 0;
	       if (cpu_tb.u0.e_reg == data_in_check[15:8]) begin
		  $display("Load E from Program CORRECT");
	       end else begin
		  $display("Load E from Program INCORRECT, %X %X", data_in_check[15:8], cpu_tb.u0.e_reg);
		  errors++;
	       end 
	    end

	    // *********************************************************
	    // Check H
	    // *********************************************************
	    if (check_inc_dec_h[1] == 1) begin
	       if (check_inc_dec_h[0] == 1) begin
		  check_flags(8'h0, 8'hE0);
		  if (pre_op_H+1 == cpu_tb.u0.h_reg)
		    $display("INC H CORRECT");
		  else begin
		     $display("INC H INCORRECT");
		     errors++;
		  end
	       end else begin
		  check_flags(8'h40, 8'hE0);
		  if (pre_op_H-1 == cpu_tb.u0.h_reg)
		    $display("DEC H CORRECT");
		  else begin
		     $display("DEC H INCORRECT");
		     errors++;
		  end
	       end // else: !if(check_inc_dec_c[0] == 1)
	       check_inc_dec_h = 0;
	    end // if (check_inc_dec_c[1] == 1)

	    if (check_h == 1) begin
	       check_h = 0;
	       if (cpu_tb.u0.h_reg == data_in_check[15:8]) begin
		  $display("Load H from Program CORRECT");
	       end else begin
		  $display("Load H from Program INCORRECT, %X %X", data_in_check[15:8], cpu_tb.u0.h_reg);
		  errors++;
	       end 
	    end

	    // *********************************************************
	    // Check L
	    // *********************************************************
	    if (check_inc_dec_l[1] == 1) begin
	       if (check_inc_dec_l[0] == 1) begin
		  check_flags(8'h0, 8'hE0);
		  if (pre_op_L+1 == cpu_tb.u0.l_reg)
		    $display("INC L CORRECT");
		  else begin
		     $display("INC L INCORRECT, %X %X", pre_op_L+1, cpu_tb.u0.l_reg);
		     errors++;
		  end
	       end else begin
		  check_flags(8'h40, 8'hE0);
		  if (pre_op_L-1 == cpu_tb.u0.l_reg)
		    $display("DEC L CORRECT");
		  else begin
		     $display("DEC L INCORRECT, %X %X", pre_op_L-1, cpu_tb.u0.l_reg);
		     errors++;
		  end
	       end // else: !if(check_inc_dec_c[0] == 1)
	       check_inc_dec_l = 0;
	    end // if (check_inc_dec_c[1] == 1)

	    if (check_l == 1) begin
	       check_l = 0;
	       if (cpu_tb.u0.l_reg == data_in_check[15:8]) begin
		  $display("Load L from Program CORRECT");
	       end else begin
		  $display("Load L from Program INCORRECT, %X %X", data_in_check[15:8], cpu_tb.u0.l_reg);
		  errors++;
	       end 
	    end
	    
	    // *********************************************************
	    // Check nots
	    // *********************************************************
	    if (check_not_reg != 0) begin
	       case(check_not_reg)
		 7 : begin
		    check_flags(8'h60, 8'h60);
		    if (~pre_op_A == cpu_tb.u0.accumulator_reg)
		      $display("Not A CORRECT");
		    else begin
		       $display("Not A INCORRECT %X %X", ~pre_op_A, cpu_tb.u0.accumulator_reg);
		       errors++;
		    end
		 end
	       endcase // case (check_not_reg)
	       check_not_reg = 0;
	    end

	    // *********************************************************
	    // Check Jumps
	    // *********************************************************
	    if (jump_add == 1) begin
	       jump_add = 0;
	       // Need to subtract one from PC for check here since the check happens after PC is incremented again
	       if (cpu_tb.u0.u_control.program_counter-1 == pre_op_PC + data_in_check[15:8]+1 ) begin
		  $display("JR e executed CORRECT");
	       end else begin
		  $display("JR e executed INCORRECT, %X, %X, %X", pre_op_PC, data_in_check[15:8]+1, cpu_tb.u0.u_control.program_counter-1); 
		  errors++;
	       end
	    end

	    if (jump_a16 == 1) begin
	       jump_a16 = 0;
	       if (cpu_tb.u0.u_control.program_counter-1 == data_in_check)
		 $display("JP CORRECT");
	       else begin
		  $display("JP INCORRECT, %X %X", cpu_tb.u0.u_control.program_counter-1, data_in_check);
		  errors++;
	       end
	    end

	    if (jump_hl == 1) begin
	       jump_hl = 0;
	       if (cpu_tb.u0.u_control.program_counter-1 == pre_op_HL)
		 $display("JP CORRECT");
	       else begin
		  $display("JP INCORRECT, %X %X", cpu_tb.u0.u_control.program_counter-1, pre_op_HL);
		  errors++;
	       end
	    end

	    if (check_inc_PC == 1) begin
	       check_inc_PC = 0;
	       if (cpu_tb.u0.u_control.program_counter == pre_op_PC+2) begin
		  $display("Jump was skipped CORRECT");
	       end else begin
		  $display("Jump was skipped INCORRECT, %X %X", cpu_tb.u0.u_control.program_counter, pre_op_PC+2);
		  errors++;
	       end
	    end

	    if (check_reset != 0) begin
	       if (cpu_tb.u0.u_alu.stack_data == pre_op_PC ) begin
		  $display("Loaded SP CORRECT");
	       end else begin
		  $display("Loaded SP INCORRECT, %X %X", cpu_tb.u0.u_alu.stack_data, pre_op_PC);
		  errors++;
	       end
	       case (check_reset[2:0])
		 0 : begin
		    if (cpu_tb.u0.u_control.program_counter-1 == 0)
		      $display("RESET CORRECT");
		    else begin
		       $display("RESET INCORRECT %X %X", cpu_tb.u0.u_control.program_counter-1, 0);
		       errors++;
		    end
		 end
		 1 : begin
		    if (cpu_tb.u0.u_control.program_counter-1 == 8)
		      $display("RESET CORRECT");
		    else begin
		       $display("RESET INCORRECT %X %X", cpu_tb.u0.u_control.program_counter-1, 8);
		       errors++;
		    end
		 end
		 2 : begin
		    if (cpu_tb.u0.u_control.program_counter-1 == 16)
		      $display("RESET CORRECT");
		    else begin
		       $display("RESET INCORRECT %X %X", cpu_tb.u0.u_control.program_counter-1, 16);
		       errors++;
		    end
		 end
		 3 :begin
		    if (cpu_tb.u0.u_control.program_counter-1 == 24)
		      $display("RESET CORRECT");
		    else begin
		       $display("RESET INCORRECT %X %X", cpu_tb.u0.u_control.program_counter-1, 24);
		       errors++;
		    end
		 end
		 4 : begin
		    if (cpu_tb.u0.u_control.program_counter-1 == 32)
		      $display("RESET CORRECT");
		    else begin
		       $display("RESET INCORRECT %X %X", cpu_tb.u0.u_control.program_counter-1, 32);
		       errors++;
		    end
		 end
		 5 : begin
		    if (cpu_tb.u0.u_control.program_counter-1 == 40)
		      $display("RESET CORRECT");
		    else begin
		       $display("RESET INCORRECT %X %X", cpu_tb.u0.u_control.program_counter-1, 40);
		       errors++;
		    end
		 end
		 6 : begin
		    if (cpu_tb.u0.u_control.program_counter-1 == 48)
		      $display("RESET CORRECT");
		    else begin
		       $display("RESET INCORRECT %X %X", cpu_tb.u0.u_control.program_counter-1, 48);
		       errors++;
		    end
		 end
		 7 : begin
		    if (cpu_tb.u0.u_control.program_counter-1 == 56)
		      $display("RESET CORRECT");
		    else begin
		       $display("RESET INCORRECT %X %X", cpu_tb.u0.u_control.program_counter-1, 56);
		       errors++;
		    end
		 end
	       endcase // case (check_reset[2:0])
	       check_reset = 0;
	    end // if (check_reset != 0)

	    if (call_a16 == 1) begin
	       call_a16 = 0;
	       if ( (cpu_tb.u0.u_alu.stack_data == pre_op_PC+2) && (cpu_tb.u0.u_alu.stack_pointer == pre_op_SP-2) && (cpu_tb.u0.u_control.program_counter == data_in_check+1)) begin
		  $display("Call CORRECT");
	       end else begin
		  $display("Call INCORRECT %X %X %X %X %X %X", cpu_tb.u0.u_alu.stack_data, pre_op_PC+1, cpu_tb.u0.u_alu.stack_pointer, pre_op_SP-2, cpu_tb.u0.u_control.program_counter, data_in_check+2);
		  errors++;
	       end
	    end

	    if (check_ret == 1) begin
	       check_ret = 0;
	       if ( (cpu_tb.u0.u_alu.stack_pointer == pre_op_SP+2) && (cpu_tb.u0.u_control.program_counter == pre_op_PC+1)) begin
		  $display("RET CORRECT");
	       end else begin
		  $display("RET INCORRECT %X %X %X %X", cpu_tb.u0.u_alu.stack_pointer, pre_op_SP+2, cpu_tb.u0.u_control.program_counter, pre_op_PC+1);
		  errors++;
	       end
	    end

	    if (check_push == 1) begin
	       check_push = 0;
	       if ((cpu_tb.u0.u_alu.stack_data == cpu_tb.u0.bc_reg) && (pre_op_SP-2 == cpu_tb.u0.u_alu.stack_pointer)) begin
		  $display("PUSH CORRECT");
	       end else begin
		  $display("PUSH INCORRECT, %X %X %X %X", cpu_tb.u0.u_alu.stack_data, cpu_tb.u0.bc_reg, pre_op_SP-2, cpu_tb.u0.u_alu.stack_pointer);
		  errors++;
	       end
	    end

	    if (check_pop == 1) begin
	       check_pop = 0;
	       if ((pre_op_HL == cpu_tb.u0.hl_reg) && (pre_op_SP+2 == cpu_tb.u0.u_alu.stack_pointer)) begin
		  $display("POP CORRECT");
	       end else begin
		  $display("POP INCORRECT, %X %X %X %X", pre_op_HL, cpu_tb.u0.hl_reg, pre_op_SP+2, cpu_tb.u0.u_alu.stack_pointer);
		  errors++;
	       end
	    end
	    
	    // *********************************************************
	    // Check Flags
	    // *********************************************************
	    if (check_scf == 1) begin
	       check_scf = 0;
	       check_flags(8'h10, 8'h70);
	    end

	    if (check_ccf == 1) begin
	       check_ccf = 0;
	       check_flags({3'h0, ~pre_op_flags[4], 4'h0}, 8'h70);
	    end

	    
	    // *********************************************************
	    // Check LD r, r
	    // *********************************************************
	    if (check_ld_command != 0) begin
	       case (check_ld_command[5:3])
		 0 : begin // B
		    case (check_ld_command[2:0])
		      0 : begin // B
			 if (cpu_tb.u0.b_reg == pre_op_B)
			   $display("LD B, B CORRECT");
			 else begin
			    $display("LD B, B INCORRECT %X %X", cpu_tb.u0.b_reg, pre_op_B);
			    errors++;
			 end
		      end
		      1 : begin // C
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.c_reg)
			   $display("LD B, C CORRECT");
			 else begin
			    $display("LD B, C INCORRECT %X %X", cpu_tb.u0.b_reg, cpu_tb.u0.c_reg);
			    errors++;
			 end
		      end
		      2 : begin // D
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.d_reg)
			   $display("LD B, D CORRECT");
			 else begin
			    $display("LD B, D INCORRECT %X %X", cpu_tb.u0.b_reg, cpu_tb.u0.d_reg);
			    errors++;
			 end
		      end
		      3 : begin // E
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.e_reg)
			   $display("LD B, D CORRECT");
			 else begin
			    $display("LD B, D INCORRECT %X %X", cpu_tb.u0.b_reg, cpu_tb.u0.d_reg);
			    errors++;
			 end
		      end
		      4 : begin // H
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.h_reg)
			   $display("LD B, h CORRECT");
			 else begin
			    $display("LD B, h INCORRECT %X %X", cpu_tb.u0.b_reg, cpu_tb.u0.h_reg);
			    errors++;
			 end
		      end
		      5 : begin // L
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.l_reg)
			   $display("LD B, l CORRECT");
			 else begin
			    $display("LD B, l INCORRECT %X %X", cpu_tb.u0.b_reg, cpu_tb.u0.l_reg);
			    errors++;
			 end
		      end
		      6 : begin // (HL)
			 if (cpu_tb.u0.b_reg == data_in_check[15:8] && last_rd_addr == cpu_tb.u0.hl_reg)
			   $display("LD B, (HL) CORRECT");
			 else begin
			    $display("LD B, (HL) INCORRECT %X %X %X %X", cpu_tb.u0.b_reg, data_in_check[15:8], last_rd_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		      7 : begin // A
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.accumulator_reg)
			   $display("LD B, a CORRECT");
			 else begin
			    $display("LD B, a INCORRECT %X %X", cpu_tb.u0.b_reg, cpu_tb.u0.accumulator_reg);
			    errors++;
			 end
		      end
		    endcase
		 end
		 1 : begin // C
		    case (check_ld_command[2:0])
		      0 : begin // B
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.c_reg)
			   $display("LD c, b CORRECT");
			 else begin
			    $display("LD c, b INCORRECT %X %X", cpu_tb.u0.c_reg, cpu_tb.u0.b_reg);
			    errors++;
			 end
		      end
		      1 : begin // C
			 if (cpu_tb.u0.c_reg == pre_op_C)
			   $display("LD c, c CORRECT");
			 else begin
			    $display("LD c, c INCORRECT %X %X", cpu_tb.u0.c_reg, pre_op_C);
			    errors++;
			 end
		      end
		      2 : begin // D
			 if (cpu_tb.u0.d_reg == cpu_tb.u0.c_reg)
			   $display("LD c, d CORRECT");
			 else begin
			    $display("LD c, d INCORRECT %X %X", cpu_tb.u0.c_reg, cpu_tb.u0.d_reg);
			    errors++;
			 end
		      end
		      3 : begin // E
			 if (cpu_tb.u0.e_reg == cpu_tb.u0.c_reg)
			   $display("LD c, e CORRECT");
			 else begin
			    $display("LD c, e INCORRECT %X %X", cpu_tb.u0.c_reg, cpu_tb.u0.e_reg);
			    errors++;
			 end
		      end
		      4 : begin // H
			 if (cpu_tb.u0.h_reg == cpu_tb.u0.c_reg)
			   $display("LD c, h CORRECT");
			 else begin
			    $display("LD c, h INCORRECT %X %X", cpu_tb.u0.c_reg, cpu_tb.u0.h_reg);
			    errors++;
			 end
		      end
		      5 : begin // L
			 if (cpu_tb.u0.l_reg == cpu_tb.u0.c_reg)
			   $display("LD c, l CORRECT");
			 else begin
			    $display("LD c, l INCORRECT %X %X", cpu_tb.u0.c_reg, cpu_tb.u0.l_reg);
			    errors++;
			 end
		      end
		      6 : begin // (HL)
			 if (data_in_check[15:8] == cpu_tb.u0.c_reg && last_rd_addr == cpu_tb.u0.hl_reg)
			   $display("LD c, (HL) CORRECT");
			 else begin
			    $display("LD c, (HL) INCORRECT %X %X %X %X", cpu_tb.u0.c_reg, data_in_check[15:8], last_rd_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		      7 : begin // A
			 if (cpu_tb.u0.accumulator_reg == cpu_tb.u0.c_reg)
			   $display("LD c, a CORRECT");
			 else begin
			    $display("LD c, a INCORRECT %X %X", cpu_tb.u0.c_reg, cpu_tb.u0.accumulator_reg);
			    errors++;
			 end
		      end
		    endcase
		 end
		 2 : begin // D
		    case (check_ld_command[2:0])
		      0 : begin // B
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.d_reg)
			   $display("LD d, b CORRECT");
			 else begin
			    $display("LD d, b INCORRECT %X %X", cpu_tb.u0.d_reg, cpu_tb.u0.b_reg);
			    errors++;
			 end
		      end
		      1 : begin // C
			 if (cpu_tb.u0.c_reg == cpu_tb.u0.d_reg)
			   $display("LD d, c CORRECT");
			 else begin
			    $display("LD d, c INCORRECT %X %X", cpu_tb.u0.d_reg, cpu_tb.u0.c_reg);
			    errors++;
			 end
		      end
		      2 : begin // D
			 if (cpu_tb.u0.d_reg == pre_op_D)
			   $display("LD d, d CORRECT");
			 else begin
			    $display("LD d, d INCORRECT %X %X", cpu_tb.u0.d_reg, pre_op_D);
			    errors++;
			 end
		      end
		      3 : begin // E
			 if (cpu_tb.u0.e_reg == cpu_tb.u0.d_reg)
			   $display("LD d, e CORRECT");
			 else begin
			    $display("LD d, e INCORRECT %X %X", cpu_tb.u0.d_reg, cpu_tb.u0.e_reg);
			    errors++;
			 end
		      end
		      4 : begin // H
			 if (cpu_tb.u0.h_reg == cpu_tb.u0.d_reg)
			   $display("LD d, h CORRECT");
			 else begin
			    $display("LD d, h INCORRECT %X %X", cpu_tb.u0.d_reg, cpu_tb.u0.h_reg);
			    errors++;
			 end
		      end
		      5 : begin // L
			 if (cpu_tb.u0.l_reg == cpu_tb.u0.d_reg)
			   $display("LD d, l CORRECT");
			 else begin
			    $display("LD d, l INCORRECT %X %X", cpu_tb.u0.d_reg, cpu_tb.u0.l_reg);
			    errors++;
			 end
		      end
		      6 : begin // (HL)
			 if (data_in_check[15:8] == cpu_tb.u0.d_reg && last_rd_addr == cpu_tb.u0.hl_reg)
			   $display("LD d, (HL) CORRECT");
			 else begin
			    $display("LD d, (HL) INCORRECT %X %X %X %X", cpu_tb.u0.d_reg, data_in_check[15:8], last_rd_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		      7 : begin // A
			 if (cpu_tb.u0.accumulator_reg == cpu_tb.u0.d_reg)
			   $display("LD d, a CORRECT");
			 else begin
			    $display("LD d, a INCORRECT %X %X", cpu_tb.u0.d_reg, cpu_tb.u0.accumulator_reg);
			    errors++;
			 end
		      end
		    endcase
		 end
		 3 : begin // E
		    case (check_ld_command[2:0])
		      0 : begin // B
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.e_reg)
			   $display("LD e, b CORRECT");
			 else begin
			    $display("LD e, b INCORRECT %X %X", cpu_tb.u0.e_reg, cpu_tb.u0.b_reg);
			    errors++;
			 end
		      end
		      1 : begin // C
			 if (cpu_tb.u0.c_reg == cpu_tb.u0.e_reg)
			   $display("LD e, c CORRECT");
			 else begin
			    $display("LD e, c INCORRECT %X %X", cpu_tb.u0.e_reg, cpu_tb.u0.c_reg);
			    errors++;
			 end
		      end
		      2 : begin // D
			 if (cpu_tb.u0.d_reg == cpu_tb.u0.e_reg)
			   $display("LD e, d CORRECT");
			 else begin
			    $display("LD e, d INCORRECT %X %X", cpu_tb.u0.e_reg, cpu_tb.u0.d_reg);
			    errors++;
			 end
		      end
		      3 : begin // E
			 if (cpu_tb.u0.e_reg == pre_op_E)
			   $display("LD e, e CORRECT");
			 else begin
			    $display("LD e, e INCORRECT %X %X", cpu_tb.u0.e_reg, pre_op_E);
			    errors++;
			 end
		      end
		      4 : begin // H
			 if (cpu_tb.u0.h_reg == cpu_tb.u0.e_reg)
			   $display("LD e, h CORRECT");
			 else begin
			    $display("LD e, h INCORRECT %X %X", cpu_tb.u0.e_reg, cpu_tb.u0.h_reg);
			    errors++;
			 end
		      end
		      5 : begin // L
			 if (cpu_tb.u0.l_reg == cpu_tb.u0.e_reg)
			   $display("LD e, l CORRECT");
			 else begin
			    $display("LD e, l INCORRECT %X %X", cpu_tb.u0.e_reg, cpu_tb.u0.l_reg);
			    errors++;
			 end
		      end
		      6 : begin // (HL)
			 if (cpu_tb.u0.e_reg == data_in_check[15:8] && last_rd_addr == cpu_tb.u0.hl_reg)
			   $display("LD e, (HL) CORRECT");
			 else begin
			    $display("LD e, (HL) INCORRECT %X %X %X %X", cpu_tb.u0.e_reg, data_in_check[15:8], last_rd_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		      7 : begin // A
			 if (cpu_tb.u0.accumulator_reg == cpu_tb.u0.e_reg)
			   $display("LD e, a CORRECT");
			 else begin
			    $display("LD e, a INCORRECT %X %X", cpu_tb.u0.e_reg, cpu_tb.u0.accumulator_reg);
			    errors++;
			 end
		      end
		    endcase
		 end
		 4 : begin // H
		    case (check_ld_command[2:0])
		      0 : begin // B
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.h_reg)
			   $display("LD h, b CORRECT");
			 else begin
			    $display("LD h, b INCORRECT %X %X", cpu_tb.u0.h_reg, cpu_tb.u0.b_reg);
			    errors++;
			 end
		      end
		      1 : begin // C
			 if (cpu_tb.u0.c_reg == cpu_tb.u0.h_reg)
			   $display("LD h, c CORRECT");
			 else begin
			    $display("LD h, c INCORRECT %X %X", cpu_tb.u0.h_reg, cpu_tb.u0.c_reg);
			    errors++;
			 end
		      end
		      2 : begin // D
			 if (cpu_tb.u0.d_reg == cpu_tb.u0.h_reg)
			   $display("LD h, d CORRECT");
			 else begin
			    $display("LD h, d INCORRECT %X %X", cpu_tb.u0.h_reg, cpu_tb.u0.d_reg);
			    errors++;
			 end
		      end
		      3 : begin // E
			 if (cpu_tb.u0.e_reg == cpu_tb.u0.h_reg)
			   $display("LD h, e CORRECT");
			 else begin
			    $display("LD h, e INCORRECT %X %X", cpu_tb.u0.h_reg, cpu_tb.u0.e_reg);
			    errors++;
			 end
		      end
		      4 : begin // H
			 if (cpu_tb.u0.h_reg == pre_op_H)
			   $display("LD h, h CORRECT");
			 else begin
			    $display("LD h, h INCORRECT %X %X", cpu_tb.u0.h_reg, pre_op_H);
			    errors++;
			 end
		      end
		      5 : begin // L
			 if (cpu_tb.u0.l_reg == cpu_tb.u0.h_reg)
			   $display("LD h, l CORRECT");
			 else begin
			    $display("LD h, l INCORRECT %X %X", cpu_tb.u0.h_reg, cpu_tb.u0.l_reg);
			    errors++;
			 end
		      end
		      6 : begin // (HL)
			 if (cpu_tb.u0.h_reg == data_in_check[15:8] && last_rd_addr == pre_op_HL)
			   $display("LD h, (HL) CORRECT");
			 else begin
			    $display("LD h, (HL) INCORRECT %X %X %X %X", cpu_tb.u0.h_reg, data_in_check[15:8], last_rd_addr, pre_op_HL);
			    errors++;
			 end
		      end
		      7 : begin // A
			 if (cpu_tb.u0.accumulator_reg == cpu_tb.u0.h_reg)
			   $display("LD h, a CORRECT");
			 else begin
			    $display("LD h, a INCORRECT %X %X", cpu_tb.u0.h_reg, cpu_tb.u0.accumulator_reg);
			    errors++;
			 end
		      end
		    endcase
		 end
		 5 : begin // L
		    case (check_ld_command[2:0])
		      0 : begin // B
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.l_reg)
			   $display("LD l, b CORRECT");
			 else begin
			    $display("LD l, b INCORRECT %X %X", cpu_tb.u0.l_reg, cpu_tb.u0.b_reg);
			    errors++;
			 end
		      end
		      1 : begin // C
			 if (cpu_tb.u0.c_reg == cpu_tb.u0.l_reg)
			   $display("LD l, c CORRECT");
			 else begin
			    $display("LD l, c INCORRECT %X %X", cpu_tb.u0.l_reg, cpu_tb.u0.c_reg);
			    errors++;
			 end
		      end
		      2 : begin // D
			 if (cpu_tb.u0.d_reg == cpu_tb.u0.l_reg)
			   $display("LD l, d CORRECT");
			 else begin
			    $display("LD l, d INCORRECT %X %X", cpu_tb.u0.l_reg, cpu_tb.u0.d_reg);
			    errors++;
			 end
		      end
		      3 : begin // E
			 if (cpu_tb.u0.e_reg == cpu_tb.u0.l_reg)
			   $display("LD l, e CORRECT");
			 else begin
			    $display("LD l, e INCORRECT %X %X", cpu_tb.u0.l_reg, cpu_tb.u0.e_reg);
			    errors++;
			 end
		      end
		      4 : begin // H
			 if (cpu_tb.u0.h_reg == cpu_tb.u0.l_reg)
			   $display("LD l, h CORRECT");
			 else begin
			    $display("LD l, h INCORRECT %X %X", cpu_tb.u0.l_reg, cpu_tb.u0.h_reg);
			    errors++;
			 end
		      end
		      5 : begin // L
			 if (cpu_tb.u0.l_reg == pre_op_L)
			   $display("LD l, l CORRECT");
			 else begin
			    $display("LD l, l INCORRECT %X %X", cpu_tb.u0.l_reg, pre_op_L);
			    errors++;
			 end
		      end
		      6 : begin // (HL)
			 if (cpu_tb.u0.l_reg == data_in_check[15:8] && last_rd_addr == pre_op_HL)
			   $display("LD l, (HL) CORRECT");
			 else begin
			    $display("LD l, (HL) INCORRECT %X %X %X %X", cpu_tb.u0.l_reg, data_in_check[15:8], last_rd_addr, pre_op_HL);
			    errors++;
			 end
		      end
		      7 : begin // A
			 if (cpu_tb.u0.accumulator_reg == cpu_tb.u0.l_reg)
			   $display("LD l, a CORRECT");
			 else begin
			    $display("LD l, a INCORRECT %X %X", cpu_tb.u0.l_reg, cpu_tb.u0.accumulator_reg);
			    errors++;
			 end
		      end
		    endcase
		 end
		 6 : begin // (HL) 
		    case (check_ld_command[2:0])
		      0 : begin // B
			 if (cpu_tb.u0.b_reg == trans.data_o && last_wr_addr == cpu_tb.u0.hl_reg)
			   $display("LD (HL), b CORRECT");
			 else begin
			    $display("LD (HL), b INCORRECT %X %X %X %X", cpu_tb.u0.b_reg, trans.data_o, last_wr_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		      1 : begin // C
			 if (cpu_tb.u0.c_reg == trans.data_o && last_wr_addr == cpu_tb.u0.hl_reg)
			   $display("LD (HL), c CORRECT");
			 else begin
			    $display("LD (HL), c INCORRECT %X %X %X %X", cpu_tb.u0.c_reg, trans.data_o, last_wr_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		      2 : begin // D
			 if (cpu_tb.u0.d_reg == trans.data_o && last_wr_addr == cpu_tb.u0.hl_reg)
			   $display("LD (HL), d CORRECT");
			 else begin
			    $display("LD (HL), d INCORRECT %X %X %X %X", cpu_tb.u0.d_reg, trans.data_o, last_wr_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		      3 : begin // E
			 if (cpu_tb.u0.e_reg == trans.data_o && last_wr_addr == cpu_tb.u0.hl_reg)
			   $display("LD (HL), e CORRECT");
			 else begin
			    $display("LD (HL), e INCORRECT %X %X %X %X", cpu_tb.u0.e_reg, trans.data_o, last_wr_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		      4 : begin // H
			 if (cpu_tb.u0.h_reg == trans.data_o && last_wr_addr == cpu_tb.u0.hl_reg)
			   $display("LD (HL), h CORRECT");
			 else begin
			    $display("LD (HL), h INCORRECT %X %X %X %X", cpu_tb.u0.h_reg, trans.data_o, last_wr_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		      5 : begin // L
			 if (cpu_tb.u0.l_reg == trans.data_o && last_wr_addr == cpu_tb.u0.hl_reg)
			   $display("LD (HL), L CORRECT");
			 else begin
			    $display("LD (HL), L INCORRECT %X %X %X %X", cpu_tb.u0.l_reg, trans.data_o, last_wr_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		      6 : begin // (HL)
			 break; // No op covers this (HALT op)
		      end
		      7 : begin // A
			 if (cpu_tb.u0.accumulator_reg == trans.data_o && last_wr_addr == cpu_tb.u0.hl_reg)
			   $display("LD (HL), a CORRECT");
			 else begin
			    $display("LD (HL), a INCORRECT %X %X %X %X", cpu_tb.u0.accumulator_reg, trans.data_o, last_wr_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		    endcase
		 end
		 7 : begin // A
		    case (check_ld_command[2:0])
		      0 : begin // B
			 if (cpu_tb.u0.b_reg == cpu_tb.u0.accumulator_reg)
			   $display("LD a, b CORRECT");
			 else begin
			    $display("LD a, b INCORRECT %X %X", cpu_tb.u0.accumulator_reg, cpu_tb.u0.b_reg);
			    errors++;
			 end
		      end
		      1 : begin // C
			 if (cpu_tb.u0.c_reg == cpu_tb.u0.accumulator_reg)
			   $display("LD a, c CORRECT");
			 else begin
			    $display("LD a, c INCORRECT %X %X", cpu_tb.u0.accumulator_reg, cpu_tb.u0.c_reg);
			    errors++;
			 end
		      end
		      2 : begin // D
			 if (cpu_tb.u0.d_reg == cpu_tb.u0.accumulator_reg)
			   $display("LD a, d CORRECT");
			 else begin
			    $display("LD a, d INCORRECT %X %X", cpu_tb.u0.accumulator_reg, cpu_tb.u0.d_reg);
			    errors++;
			 end
		      end
		      3 : begin // E
			 if (cpu_tb.u0.e_reg == cpu_tb.u0.accumulator_reg)
			   $display("LD a, e CORRECT");
			 else begin
			    $display("LD a, e INCORRECT %X %X", cpu_tb.u0.accumulator_reg, cpu_tb.u0.e_reg);
			    errors++;
			 end
		      end
		      4 : begin // H
			 if (cpu_tb.u0.h_reg == cpu_tb.u0.accumulator_reg)
			   $display("LD a, h CORRECT");
			 else begin
			    $display("LD a, h INCORRECT %X %X", cpu_tb.u0.accumulator_reg, cpu_tb.u0.h_reg);
			    errors++;
			 end
		      end
		      5 : begin // L
			 if (cpu_tb.u0.l_reg == cpu_tb.u0.accumulator_reg)
			   $display("LD a, b CORRECT");
			 else begin
			    $display("LD a, b INCORRECT %X %X", cpu_tb.u0.accumulator_reg, cpu_tb.u0.l_reg);
			    errors++;
			 end
		      end
		      6 : begin // (HL)
			 if (cpu_tb.u0.accumulator_reg == data_in_check[15:8] && last_rd_addr == cpu_tb.u0.hl_reg)
			   $display("LD a, (HL) CORRECT");
			 else begin
			    $display("LD a, (HL) INCORRECT %X %X %X %X", cpu_tb.u0.accumulator_reg, data_in_check[15:8], last_rd_addr, cpu_tb.u0.hl_reg);
			    errors++;
			 end
		      end
		      7 : begin // A
			 if (cpu_tb.u0.accumulator_reg == pre_op_A)
			   $display("LD a, a CORRECT");
			 else begin
			    $display("LD a, a INCORRECT %X %X", cpu_tb.u0.accumulator_reg, pre_op_A);
			    errors++;
			 end
		      end
		    endcase
		 end
	       endcase // case (check_ld_command[5:3])
	       check_ld_command = 0;
	    end

	    // *****************************************************************
	    // ADD, ADC, SUB, SBC, AND, XOR, OR, CP
	    // *****************************************************************
	    // TODO check flags
	    if (operator != 0) begin
	       case (operator[2:0])
		 0 : begin // ADD
		    if (cpu_tb.u0.accumulator_reg == (pre_op_A + operand))
		      $display("Addition CORRECT");
		    else begin
		       $display("Addition INCORRECT, %X %X %X %X", cpu_tb.u0.accumulator_reg, pre_op_A + operand, pre_op_A , operand);
		       errors++;
		    end
		 end
		 1 : begin // ADC
		    if (cpu_tb.u0.accumulator_reg == (pre_op_A + operand + pre_op_flags[4]))
		      $display("Addition + carry CORRECT");
		    else begin
		       $display("Addition + carry INCORRECT, %X %X %X %X", cpu_tb.u0.accumulator_reg, pre_op_A + operand + pre_op_flags[4], pre_op_A , operand);
		       errors++;
		    end
		 end
		 2 : begin // SUB
		    if (cpu_tb.u0.accumulator_reg == (pre_op_A - operand))
		      $display("Subtraction CORRECT");
		    else begin
		       $display("Subtraction INCORRECT, %X %X %X %X", cpu_tb.u0.accumulator_reg, pre_op_A - operand, pre_op_A , operand);
		       errors++;
		    end
		 end
		 3 : begin // SBC
		    if (cpu_tb.u0.accumulator_reg == (pre_op_A - operand - pre_op_flags[4]))
		      $display("Subtraction - carry CORRECT");
		    else begin
		       $display("Subtraction - carry INCORRECT, %X %X %X %X", cpu_tb.u0.accumulator_reg, pre_op_A - operand - pre_op_flags[4], pre_op_A , operand);
		       errors++;
		    end
		 end
		 4 : begin // AND
		    if (cpu_tb.u0.accumulator_reg == (pre_op_A & operand))
		      $display("And CORRECT");
		    else begin
		       $display("And INCORRECT, %X %X %X %X", cpu_tb.u0.accumulator_reg, pre_op_A & operand, pre_op_A , operand);
		       errors++;
		    end
		 end
		 5 : begin // XOR
		    if (cpu_tb.u0.accumulator_reg == (pre_op_A ^ operand))
		      $display("xor CORRECT");
		    else begin
		       $display("xor INCORRECT, %X %X %X %X", cpu_tb.u0.accumulator_reg, pre_op_A ^ operand, pre_op_A , operand);
		       errors++;
		    end
		 end
		 6 : begin // OR
		    if (cpu_tb.u0.accumulator_reg == (pre_op_A | operand))
		      $display("or CORRECT");
		    else begin
		       $display("or INCORRECT, %X %X %X %X", cpu_tb.u0.accumulator_reg, pre_op_A | operand, pre_op_A , operand);
		       errors++;
		    end
		 end
		 7 : begin // CP
		    // TODO
		    $display("TODO");
		 end
	       endcase
	       operator = 0;
	    end // if (operator != 0)
	    
	    // ********************************************************************************
	    // Check prefix commands
	    // ********************************************************************************
	    if (check_prefix != 0) begin
	       case (check_prefix)
		 1 : begin
		    if ({pre_op_B[6:0], pre_op_B[7]} == cpu_tb.u0.b_reg)
		      $display("RLC CORRECT");
		    else begin
		       $display("RLC INCORRECT, %X %X", {pre_op_B[6:0], pre_op_B[7]}, cpu_tb.u0.b_reg);
		       errors++;
		    end
		 end
		 2 : begin
		    if ({pre_op_C[6:0], pre_op_flags[4]} == cpu_tb.u0.c_reg)
		      $display("RL CORRECT");
		    else begin
		       $display("RL INCORRECT, %X %X", {pre_op_C[6:0], pre_op_flags[4]}, cpu_tb.u0.c_reg);
		       errors++;
		    end
		 end
		 3 : begin
		    if ({pre_op_D[6:0], 1'b0} == cpu_tb.u0.d_reg)
		      $display("SLA CORRECT");
		    else begin
		       $display("SLA INCORRECT %X %X", {pre_op_D[6:0], 1'b0}, cpu_tb.u0.d_reg);
		       errors++;
		    end
		 end
		 4 : begin
		    if ({pre_op_E[3:0], pre_op_E[7:4]} == cpu_tb.u0.e_reg)
		      $display("SWAP CORRECT");
		    else begin
		       $display("SWAP INCORRECT %X %X", {pre_op_E[3:0], pre_op_E[7:4]}, cpu_tb.u0.e_reg);
		       errors++;
		    end
		 end
		 5 : begin
		    if (cpu_tb.u0.flags_reg[7] == !cpu_tb.u0.h_reg[0])
		      $display("BIT CORRECT");
		    else begin
		       $display("BIT INCORRECT %X %X", cpu_tb.u0.flags_reg[7], !cpu_tb.u0.h_reg[0]);
		       errors++;
		    end
		 end
		 6 : begin
		    if (cpu_tb.u0.flags_reg[7] == !cpu_tb.u0.l_reg[2])
		      $display("BIT CORRECT");
		    else begin
		       $display("BIT INCORRECT %X %X", cpu_tb.u0.flags_reg[7], !cpu_tb.u0.l_reg[2]);
		       errors++;
		    end
		 end
		 7 : begin
		    if (cpu_tb.u0.flags_reg[7] == !trans.data_i[4])
		      $display("BIT CORRECT");
		    else begin
		       $display("BIT INCORRECT %X %X", cpu_tb.u0.flags_reg[7], !trans.data_i[4]);
		       errors++;
		    end
		 end
		 8 : begin
		    if (cpu_tb.u0.flags_reg[7] == !cpu_tb.u0.accumulator_reg[6])
		      $display("BIT CORRECT");
		    else begin
		       $display("BIT INCORRECT %X %X", cpu_tb.u0.flags_reg[7], !cpu_tb.u0.accumulator_reg[6]);
		       errors++;
		    end
		 end
		 9 : begin
		    if ({pre_op_B[7:2], 1'b0, pre_op_B[0]} == cpu_tb.u0.b_reg)
		      $display("RES CORRECT");
		    else begin
		       $display("RES INCORRECT %X %X", {pre_op_B[7:2], 1'b0, pre_op_B[0]}, cpu_tb.u0.b_reg);
		       errors++;
		    end
		 end
		 10 : begin
		    if ({pre_op_C[7:4], 1'b0, pre_op_C[2:0]} == cpu_tb.u0.c_reg)
		      $display("RES CORRECT");
		    else begin
		       $display("RES INCORRECT %X %X", {pre_op_C[7:4], 1'b0, pre_op_C[2:0]}, cpu_tb.u0.c_reg);
		       errors++;
		    end
		 end
		 11 : begin
		    if ({pre_op_D[7:6], 1'b0, pre_op_D[4:0]} == cpu_tb.u0.d_reg)
		      $display("RES CORRECT");
		    else begin
		       $display("RES INCORRECT %X %X", {pre_op_D[7:6], 1'b0, pre_op_D[4:0]}, cpu_tb.u0.d_reg);
		       errors++;
		    end
		 end
		 12 : begin
		    if ({1'b0, pre_op_E[6:0]} == cpu_tb.u0.e_reg)
		      $display("RES CORRECT");
		    else begin
		       $display("RES INCORRECT %X %X", {1'b0, pre_op_E[6:0]}, cpu_tb.u0.e_reg);
		       errors++;
		    end
		 end
		 13 : begin
		    if ({pre_op_H[7:2], 1'b1, pre_op_H[0]} == cpu_tb.u0.h_reg)
		      $display("SET CORRECT");
		    else begin
		       $display("SET INCORRECT %X %X",{pre_op_H[7:2], 1'b1, pre_op_H[0]}, cpu_tb.u0.h_reg);
		       errors++;
		    end
		 end
		 14 : begin
		    if ({pre_op_L[7:4], 1'b1, pre_op_L[2:0]} == cpu_tb.u0.l_reg)
		      $display("SET CORRECT");
		    else begin
		       $display("SET INCORRECT, %X %X", {pre_op_L[7:4], 1'b1, pre_op_L[2:0]}, cpu_tb.u0.l_reg);
		       errors++;
		    end
		 end
		 15 : begin
		    if ({data_in_check[15:14], 1'b1, data_in_check[12:8]} == trans.data_o)
		      $display("SET CORRECT");
		    else begin
		       $display("SET INCORRECT %X %X",{data_in_check[15:14], 1'b1, data_in_check[12:8]}, trans.data_o);
		       errors++;
		    end
		 end
		 16 : begin
		    if ({1'b1, pre_op_A[6:0]} == cpu_tb.u0.accumulator_reg)
		      $display("SET CORRECT");
		    else begin
		       $display("SET INCORRECT %X %X", {1'b1, pre_op_A[6:0]}, cpu_tb.u0.accumulator_reg);
		       errors++;
		    end
		 end
	       endcase // case check_prefix
	       check_prefix = 0;
	       prefix = 0;
	    end
	    
	    // ********************************************************************************
	    // Large array of checks goes here, need to find a way to check all commands so probably just going to be a large case statment
	    // *******************************************************************************
	    if (prefix == 0) begin
	       case(trans.data_i)
		 8'h1 : begin
		    $display("Next OP : LD BC, d16");
		    check_BC = 1;
		 end
		 8'h2 : begin
		    $display("Next OP : LD (BC), A");
		    pre_op_BC = cpu_tb.u0.bc_reg;
		    pre_op_A  = cpu_tb.u0.accumulator_reg;
		    check_mem = 4;
		 end
		 8'h3 : begin
		    $display("Next OP: Inc BC");
		    check_inc_BC = 1;
		    pre_op_BC = cpu_tb.u0.bc_reg;
		 end
		 8'h4 : begin
		    $display("Next OP: Inc B");
		    check_inc_B = 1;
		    pre_op_BC = cpu_tb.u0.bc_reg;
		 end
		 8'h5 : begin
		    $display("Next OP: Dec B");
		    check_dec_B = 1;
		    pre_op_BC = cpu_tb.u0.bc_reg;
		 end
		 8'h6 : begin
		    $display("Next OP: LD B,d8");
		    check_b = 1;
		 end
		 8'h7 : begin
		    $display("Next OP: RLCA");
		    check_rlca = 1;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h8 : begin
		    $display("Next OP: LD (a16), SP");
		    check_large_mem = 1;
		 end
		 8'h9 : begin
		    $display("Next OP: ADD HL, BC");
		    check_16_bit_add = 4'h8;
		    pre_op_HL = cpu_tb.u0.hl_reg;
		    pre_op_BC = cpu_tb.u0.bc_reg;
		 end
		 8'ha : begin
		    $display("Next OP: LD A, (BC)");
		    check_a_mem = 4;
		 end
		 8'hb : begin
		    $display("Next OP: DEC BC");
		    check_dec_BC = 1;
		    pre_op_BC = cpu_tb.u0.bc_reg;
		 end
		 8'hc : begin
		    $display("Next OP: INC C");
		    check_inc_dec_c = 2'b11;
		    pre_op_C = cpu_tb.u0.c_reg;
		 end
		 8'hd : begin
		    $display("Next OP: DEC C");
		    check_inc_dec_c = 2'b10;
		    pre_op_C = cpu_tb.u0.c_reg;
		 end
		 8'he : begin
		    $display("Next OP: LD C, d8");
		    check_c = 1;
		 end
		 8'hf : begin
		    $display("Next OP: RRCA");
		    check_rrca = 1;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h10 : begin
		    // TODO STOP OP
		    $display("Next OP: STOP");
		 end
		 8'h11 : begin
		    $display("Next OP: LD DE, d16");
		    check_DE = 1;
		 end
		 8'h12 : begin
		    $display("Next OP: LD (DE), A");
		    pre_op_DE = cpu_tb.u0.de_reg;
		    pre_op_A  = cpu_tb.u0.accumulator_reg;
		    check_mem = 5;
		 end
		 8'h13 : begin
		    $display("Next OP: INC DE");
		    check_inc_DE = 1;
		    pre_op_DE = cpu_tb.u0.de_reg;
		 end
		 8'h14 : begin
		    $display("Next OP: INC D");
		    check_inc_dec_d = 2'b11;
		    pre_op_D = cpu_tb.u0.d_reg;
		 end
		 8'h15 : begin
		    $display("Next OP: DEC D");
		    check_inc_dec_d = 2'b10;
		    pre_op_D = cpu_tb.u0.d_reg;
		 end
		 8'h16 : begin
		    $display("Next OP: LD D, d8");
		    check_d = 1;
		 end
		 8'h17 : begin
		    $display("Next OP: RLA");
		    check_rla = 1;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h18 : begin
		    $display("Next OP: JR r8");
		    jump_add = 1;
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;
		 end
		 8'h19 : begin
		    $display("Next OP: ADD HL, DE");
		    check_16_bit_add = 4'h9;
		    pre_op_HL = cpu_tb.u0.hl_reg;
		    pre_op_DE = cpu_tb.u0.de_reg;
		 end
		 8'h1a : begin
		    $display("Next OP: LD A, (DE)");
		    check_a_mem = 5;
		 end
		 8'h1b : begin
		    $display("Next OP: DEC DE");
		    check_dec_DE = 1;
		    pre_op_DE = cpu_tb.u0.de_reg;
		 end
		 8'h1c : begin
		    $display("Next OP: INC E");
		    check_inc_dec_e = 2'b11;
		    pre_op_E = cpu_tb.u0.e_reg;
		 end
		 8'h1d : begin
		    $display("Next OP: DEC E");
		    check_inc_dec_e = 2'b10;
		    pre_op_E = cpu_tb.u0.e_reg;
		 end
		 8'h1e : begin
		    $display("Next OP: LD E d8");
		    check_e = 1;
		 end
		 8'h1f : begin
		    $display("Next OP: RRA");
		    check_rra = 1;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h20 : begin
		    $display("Next OP: JR NZ, r8");
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;
		    if (cpu_tb.u0.u_alu.zero_flag == 0) begin
		       jump_add = 1;
		    end else begin
		       check_inc_PC = 1;
		    end
		 end
		 8'h21 : begin
		    $display("Next OP: LD HL, d16");
		    check_HL = 1;
		 end
		 8'h22 : begin
		    $display("Next OP: LD (HL+), A");
		    pre_op_HL = cpu_tb.u0.hl_reg;
		    pre_op_A  = cpu_tb.u0.accumulator_reg;
		    check_mem = 6;
		    check_inc_HL = 1;
		 end
		 8'h23 : begin
		    $display("Next OP: INC HL");
		    check_inc_HL = 1;
		    pre_op_HL = cpu_tb.u0.hl_reg;
		 end
		 8'h24 : begin
		    $display("Next OP: INC H");
		    check_inc_dec_h = 2'b11;
		    pre_op_H = cpu_tb.u0.h_reg;
		 end
		 8'h25 : begin
		    $display("Next OP: DEC H");
		    check_inc_dec_h = 2'b10;
		    pre_op_H = cpu_tb.u0.h_reg;
		 end
		 8'h26 : begin
		    $display("Next OP: LD H, d8");
		    check_h = 1;
		 end
		 8'h27 : begin
		    $display("Next OP: DAA");
		    // TODO
		 end
		 8'h28 : begin
		    $display("Next OP: JR Z, r8");
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;
		    if (cpu_tb.u0.u_alu.zero_flag == 1) begin
		       jump_add = 1;
		    end else begin
		       check_inc_PC = 1;
		    end
		 end
		 8'h29 : begin
		    $display("Next OP: ADD HL, HL");
		    check_16_bit_add = 4'ha;
		    pre_op_HL = cpu_tb.u0.hl_reg;
		 end
		 8'h2a : begin
		    $display("Next OP: LD A, (HL+)");
		    check_a_mem = 6;
		    pre_op_HL = cpu_tb.u0.hl_reg;
		    check_inc_HL = 1;
		 end
		 8'h2b : begin
		    $display("Next OP: DEC HL");
		    check_dec_HL = 1;
		    pre_op_HL = cpu_tb.u0.hl_reg;
		 end
		 8'h2c : begin
		    $display("Next OP: INC L");
		    check_inc_dec_l = 2'b11;
		    pre_op_L = cpu_tb.u0.l_reg;
		 end
		 8'h2d : begin
		    $display("Next OP: DEC L");
		    check_inc_dec_l = 2'b10;
		    pre_op_L = cpu_tb.u0.l_reg;
		 end
		 8'h2e : begin
		    $display("Next OP: LD L d8");
		    check_l = 1;
		 end
		 8'h2f : begin
		    $display("Next OP: CPL");
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    check_not_reg = 7;
		 end
		 8'h30 : begin
		    $display("Next OP: JR NC, r8");
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;
		    if (cpu_tb.u0.u_alu.carry_flag == 0) begin
		       jump_add = 1;
		    end else begin
		       check_inc_PC = 1;
		    end
		 end
		 8'h31 : begin
		    $display("Next OP: LD SP, d16");
		    check_SP = 1;
		 end
		 8'h32 : begin
		    $display("Next OP: LD (HL-), A");
		    pre_op_HL = cpu_tb.u0.hl_reg;
		    pre_op_A  = cpu_tb.u0.accumulator_reg;
		    check_mem = 6;
		    check_dec_HL = 1;
		 end
		 8'h33 : begin
		    $display("Next OP: INC SP");
		    check_inc_SP = 1;
		    pre_op_SP = cpu_tb.u0.u_alu.stack_pointer;
		 end
		 8'h34 : begin
		    $display("Next OP: INC (HL)");
		    pre_op_HL = cpu_tb.u0.hl_reg;
		    check_mem = 7;
		    check_inc_dec_mem = 2;
		 end
		 8'h35 : begin
		    $display("Next OP: DEC (HL)");
		    pre_op_HL = cpu_tb.u0.hl_reg;
		    check_mem = 7;
		    check_inc_dec_mem = 3;
		 end
		 8'h36 : begin
		    $display("Next OP: LD (HL), d8");
		    pre_op_HL = cpu_tb.u0.hl_reg;
		    check_mem = 7;
		    check_inc_dec_mem = 0;
		 end
		 8'h37 : begin
		    $display("Next OP: SCF");
		    check_scf = 1;
		 end
		 8'h38 : begin
		    $display("Next OP: JR C, r8");
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;
		    if (cpu_tb.u0.u_alu.carry_flag == 1) begin
		       jump_add = 1;
		    end else begin
		       check_inc_PC = 1;
		    end
		 end
		 8'h39 : begin
		    $display("Next OP: ADD HL, SP");
		    check_16_bit_add = 4'hb;
		    pre_op_HL = cpu_tb.u0.hl_reg;
		    pre_op_SP = cpu_tb.u0.u_alu.stack_pointer;
		 end
		 8'h3a : begin
		    $display("Next OP: LD A, (HL-)");
		    check_A = 1;
		    reg_used = 7;
		    pre_op_HL = cpu_tb.u0.hl_reg;
		    check_dec_HL = 1;
		 end
		 8'h3b : begin
		    $display("Next OP: DEC SP");
		    check_dec_SP = 1;
		    pre_op_SP = cpu_tb.u0.u_alu.stack_pointer;
		 end
		 8'h3c : begin
		    $display("Next OP: INC A");
		    check_inc_dec_a = 2'b11;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h3d : begin
		    $display("Next OP: DEC A");
		    check_inc_dec_a = 2'b10;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h3e : begin
		    $display("Next OP: LD A d8");
		    check_A = 1;
		    reg_used = 7;
		 end
		 8'h3f : begin
		    $display("Next OP: CCF");
		    check_ccf = 1;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h40 : begin
		    $display("Next OP: LD B,B");
		    check_ld_command = 6'h40;
		    pre_op_B = cpu_tb.u0.b_reg;
		 end
		 8'h41 : begin
		    $display("Next OP: LD B,C");
		    check_ld_command = 6'h41;
		 end
		 8'h42 : begin
		    $display("Next OP: LD B,D");
		    check_ld_command = 6'h42;
		 end
		 8'h43 : begin
		    $display("Next OP: LD B,E");
		    check_ld_command = 6'h43;
		 end
		 8'h44 : begin
		    $display("Next OP: LD B,H");
		    check_ld_command = 6'h44;
		 end
		 8'h45 : begin
		    $display("Next OP: LD B,L");
		    check_ld_command = 6'h45;
		 end
		 8'h46 : begin
		    $display("Next OP: LD B,(HL)");
		    check_ld_command = 6'h46;
		 end
		 8'h47 : begin
		    $display("Next OP: LD B,A");
		    check_ld_command = 6'h47;
		 end
		 8'h48 : begin
		    $display("Next OP: LD C,B");
		    check_ld_command = 6'h48;
		 end
		 8'h49 : begin
		    $display("Next OP: LD C,C");
		    check_ld_command = 6'h49;
		    pre_op_C = cpu_tb.u0.c_reg;
		 end
		 8'h4a : begin
		    $display("Next OP: LD C,D");
		    check_ld_command = 6'h4a;
		 end
		 8'h4b : begin
		    $display("Next OP: LD C,E");
		    check_ld_command = 6'h4b;
		 end
		 8'h4c : begin
		    $display("Next OP: LD C,H");
		    check_ld_command = 6'h4c;
		 end
		 8'h4d : begin
		    $display("Next OP: LD C,L");
		    check_ld_command = 6'h4d;
		 end
		 8'h4e : begin
		    $display("Next OP: LD C,(HL)");
		    check_ld_command = 6'h4e;
		 end
		 8'h4f : begin
		    $display("Next OP: LD C,A");
		    check_ld_command = 6'h4f;
		 end
		 8'h50 : begin
		    $display("Next OP: LD D,B");
		    check_ld_command = 6'h50;
		 end
		 8'h51 : begin
		    $display("Next OP: LD D,C");
		    check_ld_command = 6'h51;
		 end
		 8'h52 : begin
		    $display("Next OP: LD D,D");
		    check_ld_command = 6'h52;
		    pre_op_D = cpu_tb.u0.d_reg;
		 end
		 8'h53 : begin
		    $display("Next OP: LD D,E");
		    check_ld_command = 6'h53;
		 end
		 8'h54 : begin
		    $display("Next OP: LD D,H");
		    check_ld_command = 6'h54;
		 end
		 8'h55 : begin
		    $display("Next OP: LD D,L");
		    check_ld_command = 6'h55;
		 end
		 8'h56 : begin
		    $display("Next OP: LD D,(HL)");
		    check_ld_command = 6'h56;
		 end
		 8'h57 : begin
		    $display("Next OP: LD D,A");
		    check_ld_command = 6'h57;
		 end
		 8'h58 : begin
		    $display("Next OP: LD E,B");
		    check_ld_command = 6'h58;
		 end
		 8'h59 : begin
		    $display("Next OP: LD E,C");
		    check_ld_command = 6'h59;
		 end
		 8'h5a : begin
		    $display("Next OP: LD E,D");
		    check_ld_command = 6'h5a;
		 end
		 8'h5b : begin
		    $display("Next OP: LD E,E");
		    check_ld_command = 6'h5b;
		    pre_op_E = cpu_tb.u0.e_reg;
		 end
		 8'h5c : begin
		    $display("Next OP: LD E,H");
		    check_ld_command = 6'h5c;
		 end
		 8'h5d : begin
		    $display("Next OP: LD E,L");
		    check_ld_command = 6'h5d;
		 end
		 8'h5e : begin
		    $display("Next OP: LD E,(HL)");
		    check_ld_command = 6'h5e;
		 end
		 8'h5f : begin
		    $display("Next OP: LD E,A");
		    check_ld_command = 6'h5f;
		 end
		 8'h60 : begin
		    $display("Next OP: LD H,B");
		    check_ld_command = 6'h60;
		 end
		 8'h61 : begin
		    $display("Next OP: LD H,C");
		    check_ld_command = 6'h61;
		 end
		 8'h62 : begin
		    $display("Next OP: LD H,D");
		    check_ld_command = 6'h62;
		 end
		 8'h63 : begin
		    $display("Next OP: LD H,E");
		    check_ld_command = 6'h63;
		 end
		 8'h64 : begin
		    $display("Next OP: LD H,H");
		    check_ld_command = 6'h64;
		    pre_op_H = cpu_tb.u0.h_reg;
		 end
		 8'h65 : begin
		    $display("Next OP: LD H,L");
		    check_ld_command = 6'h65;
		 end
		 8'h66 : begin
		    $display("Next OP: LD H,(HL)");
		    check_ld_command = 6'h66;
		    pre_op_HL = cpu_tb.u0.hl_reg;
		 end
		 8'h67 : begin
		    $display("Next OP: LD H,A");
		    check_ld_command = 6'h67;
		 end
		 8'h68 : begin
		    $display("Next OP: LD L,B");
		    check_ld_command = 6'h68;
		 end
		 8'h69 : begin
		    $display("Next OP: LD L,C");
		    check_ld_command = 6'h69;
		 end
		 8'h6a : begin
		    $display("Next OP: LD L,D");
		    check_ld_command = 6'h6a;
		 end
		 8'h6b : begin
		    $display("Next OP: LD L,E");
		    check_ld_command = 6'h6b;
		 end
		 8'h6c : begin
		    $display("Next OP: LD L,H");
		    check_ld_command = 6'h6c;
		 end
		 8'h6d : begin
		    $display("Next OP: LD L,L");
		    check_ld_command = 6'h6d;
		    pre_op_L = cpu_tb.u0.l_reg;
		 end
		 8'h6e : begin
		    $display("Next OP: LD L,(HL)");
		    check_ld_command = 6'h6e;
		    pre_op_HL = cpu_tb.u0.hl_reg;
		 end
		 8'h6f : begin
		    $display("Next OP: LD L,A");
		    check_ld_command = 6'h6f;
		 end
		 8'h70 : begin
		    $display("Next OP: LD (HL),B");
		    check_ld_command = 6'h70;
		 end
		 8'h71 : begin
		    $display("Next OP: LD (HL),C");
		    check_ld_command = 6'h71;
		 end
		 8'h72 : begin
		    $display("Next OP: LD (HL),D");
		    check_ld_command = 6'h72;
		 end
		 8'h73 : begin
		    $display("Next OP: LD (HL),E");
		    check_ld_command = 6'h73;
		 end
		 8'h74 : begin
		    $display("Next OP: LD (HL),H");
		    check_ld_command = 6'h74;
		 end
		 8'h75 : begin
		    $display("Next OP: LD (HL),L");
		    check_ld_command = 6'h75;
		 end
		 8'h76 : begin
		    $display("Next OP: HALT");
		    // TODO
		 end
		 8'h77 : begin
		    $display("Next OP: LD (HL),A");
		    check_ld_command = 6'h77;
		 end
		 8'h78 : begin
		    $display("Next OP: LD A,B");
		    check_ld_command = 6'h78;
		 end
		 8'h79 : begin
		    $display("Next OP: LD A,C");
		    check_ld_command = 6'h79;
		 end
		 8'h7a : begin
		    $display("Next OP: LD A,D");
		    check_ld_command = 6'h7a;
		 end
		 8'h7b : begin
		    $display("Next OP: LD A,E");
		    check_ld_command = 6'h7b;
		 end
		 8'h7c : begin
		    $display("Next OP: LD A,H");
		    check_ld_command = 6'h7c;
		 end
		 8'h7d : begin
		    $display("Next OP: LD A,L");
		    check_ld_command = 6'h7d;
		 end
		 8'h7e : begin
		    $display("Next OP: LD A,(HL)");
		    check_ld_command = 6'h7e;
		 end
		 8'h7f : begin
		    $display("Next OP: LD A,A");
		    check_ld_command = 6'h7f;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h80 : begin
		    $display("Next OP: ADD A,B");
		    operator = 8;
		    operand  = cpu_tb.u0.b_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h81 : begin
		    $display("Next OP: ADD A,C");
		    operator = 8;
		    operand  = cpu_tb.u0.c_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h82 : begin
		    $display("Next OP: ADD A,D");
		    operator = 8;
		    operand  = cpu_tb.u0.d_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h83 : begin
		    $display("Next OP: ADD A,E");
		    operator = 8;
		    operand  = cpu_tb.u0.e_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h84 : begin
		    $display("Next OP: ADD A,H");
		    operator = 8;
		    operand  = cpu_tb.u0.h_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h85 : begin
		    $display("Next OP: ADD A,L");
		    operator = 8;
		    operand  = cpu_tb.u0.l_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h86 : begin
		    $display("Next OP: ADD A,(HL)");
		    operator = 8;
		    operand  = 8'h86;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h87 : begin
		    $display("Next OP: ADD A,A");
		    operator = 8;
		    operand  = cpu_tb.u0.accumulator_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h88 : begin
		    $display("Next OP: ADC A,B");
		    operator = 9;
		    operand  = cpu_tb.u0.b_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h89 : begin
		    $display("Next OP: ADC A,C");
		    operator = 9;
		    operand  = cpu_tb.u0.c_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h8a : begin
		    $display("Next OP: ADC A,D");
		    operator = 9;
		    operand  = cpu_tb.u0.d_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h8b : begin
		    $display("Next OP: ADC A,E");
		    operator = 9;
		    operand  = cpu_tb.u0.e_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h8c : begin
		    $display("Next OP: ADC A,H");
		    operator = 9;
		    operand  = cpu_tb.u0.h_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h8d : begin
		    $display("Next OP: ADC A,L");
		    operator = 9;
		    operand  = cpu_tb.u0.l_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h8e : begin
		    $display("Next OP: ADC A,(HL)");
		    operator = 9;
		    operand  = 8'h8e;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h8f : begin
		    $display("Next OP: ADC A,A");
		    operator = 9;
		    operand  = cpu_tb.u0.accumulator_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h90 : begin
		    $display("Next OP: SUB B");
		    operator = 10;
		    operand  = cpu_tb.u0.b_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h91 : begin
		    $display("Next OP: SUB C");
		    operator = 10;
		    operand  = cpu_tb.u0.c_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h92 : begin
		    $display("Next OP: SUB D");
		    operator = 10;
		    operand  = cpu_tb.u0.d_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h93 : begin
		    $display("Next OP: SUB E");
		    operator = 10;
		    operand  = cpu_tb.u0.e_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h94 : begin
		    $display("Next OP: SUB H");
		    operator = 10;
		    operand  = cpu_tb.u0.h_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h95 : begin
		    $display("Next OP: SUB L");
		    operator = 10;
		    operand  = cpu_tb.u0.l_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h96 : begin
		    $display("Next OP: SUB (HL)");
		    operator = 10;
		    operand  = 8'h96;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h97 : begin
		    $display("Next OP: SUB A");
		    operator = 10;
		    operand  = cpu_tb.u0.accumulator_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h98 : begin
		    $display("Next OP: SBC B");
		    operator = 11;
		    operand  = cpu_tb.u0.b_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h99 : begin
		    $display("Next OP: SBC C");
		    operator = 11;
		    operand  = cpu_tb.u0.c_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h9a : begin
		    $display("Next OP: SBC D");
		    operator = 11;
		    operand  = cpu_tb.u0.d_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h9b : begin
		    $display("Next OP: SBC E");
		    operator = 11;
		    operand  = cpu_tb.u0.e_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h9c : begin
		    $display("Next OP: SBC H");
		    operator = 11;
		    operand  = cpu_tb.u0.h_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h9d : begin
		    $display("Next OP: SBC L");
		    operator = 11;
		    operand  = cpu_tb.u0.l_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h9e : begin
		    $display("Next OP: SBC (HL)");
		    operator = 11;
		    operand  = 8'h9e;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h9f : begin
		    $display("Next OP: SBC A");
		    operator = 11;
		    operand  = cpu_tb.u0.accumulator_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'ha0 : begin
		    $display("Next OP: AND B");
		    operator = 12;
		    operand  = cpu_tb.u0.b_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'ha1 : begin
		    $display("Next OP: AND C");
		    operator = 12;
		    operand  = cpu_tb.u0.c_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'ha2 : begin
		    $display("Next OP: AND D");
		    operator = 12;
		    operand  = cpu_tb.u0.d_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'ha3 : begin
		    $display("Next OP: AND E");
		    operator = 12;
		    operand  = cpu_tb.u0.e_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'ha4 : begin
		    $display("Next OP: AND H");
		    operator = 12;
		    operand  = cpu_tb.u0.h_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'ha5 : begin
		    $display("Next OP: AND L");
		    operator = 12;
		    operand  = cpu_tb.u0.l_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'ha6 : begin
		    $display("Next OP: AND (HL)");
		    operator = 12;
		    operand  = 8'ha6;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'ha7 : begin
		    $display("Next OP: AND A");
		    operator = 12;
		    operand  = cpu_tb.u0.accumulator_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'ha8 : begin
		    $display("Next OP: XOR B");
		    operator = 13;
		    operand  = cpu_tb.u0.b_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'ha9 : begin
		    $display("Next OP: XOR C");
		    operator = 13;
		    operand  = cpu_tb.u0.c_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'haa : begin
		    $display("Next OP: XOR D");
		    operator = 13;
		    operand  = cpu_tb.u0.d_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hab : begin
		    $display("Next OP: XOR E");
		    operator = 13;
		    operand  = cpu_tb.u0.e_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hac : begin
		    $display("Next OP: XOR H");
		    operator = 13;
		    operand  = cpu_tb.u0.h_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'had : begin
		    $display("Next OP: XOR L");
		    operator = 13;
		    operand  = cpu_tb.u0.l_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hae : begin
		    $display("Next OP: XOR (HL)");
		    operator = 13;
		    operand  = 8'hae;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'haf : begin
		    $display("Next OP: XOR A");
		    operator = 13;
		    operand  = cpu_tb.u0.accumulator_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hb0 : begin
		    $display("Next OP: OR B");
		    operator = 14;
		    operand  = cpu_tb.u0.b_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hb1 : begin
		    $display("Next OP: OR C");
		    operator = 14;
		    operand  = cpu_tb.u0.c_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hb2 : begin
		    $display("Next OP: OR D");
		    operator = 14;
		    operand  = cpu_tb.u0.d_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hb3 : begin
		    $display("Next OP: OR E");
		    operator = 14;
		    operand  = cpu_tb.u0.e_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hb4 : begin
		    $display("Next OP: OR H");
		    operator = 14;
		    operand  = cpu_tb.u0.h_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hb5 : begin
		    $display("Next OP: OR L");
		    operator = 14;
		    operand  = cpu_tb.u0.l_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hb6 : begin
		    $display("Next OP: OR (HL)");
		    operator = 14;
		    operand  = 8'hb6;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hb7 : begin
		    $display("Next OP: OR A");
		    operator = 14;
		    operand  = cpu_tb.u0.accumulator_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hb8 : begin
		    $display("Next OP: CP B");
		    operator = 15;
		    operand  = cpu_tb.u0.b_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hb9 : begin
		    $display("Next OP: CP C");
		    operator = 15;
		    operand  = cpu_tb.u0.c_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hba : begin
		    $display("Next OP: CP D");
		    operator = 15;
		    operand  = cpu_tb.u0.d_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hbb : begin
		    $display("Next OP: CP E");
		    operator = 15;
		    operand  = cpu_tb.u0.e_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hbc : begin
		    $display("Next OP: CP H");
		    operator = 15;
		    operand  = cpu_tb.u0.h_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hbd : begin
		    $display("Next OP: CP L");
		    operator = 15;
		    operand  = cpu_tb.u0.l_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hbe : begin
		    $display("Next OP: CP (HL)");
		    operator = 15;
		    operand  = 8'hbe;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hbf : begin
		    $display("Next OP: CP A");
		    operator = 15;
		    operand  = cpu_tb.u0.accumulator_reg;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'hc0 : begin
		    $display("Ret NZ");
		 end
		 8'hc1 : begin
		    $display("POP BC");
		 end
		 8'hc2 : begin
		    $display("JP NZ, a16");
		    if (cpu_tb.u0.u_alu.zero_flag == 0) begin
		       jump_a16 = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter;
		    end else begin
		       check_inc_PC = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter+1;
		    end
		 end
		 8'hc3 : begin
		    $display("JP a16");
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;
		    jump_a16 = 1;
		 end
		 8'hc4 : begin
		    $display("Call NZ, a16");
		 end
		 8'hc5 : begin
		    $display("PUSH BC");
		    check_push = 1;
		    pre_op_SP = cpu_tb.u0.u_alu.stack_pointer;
		 end
		 8'hc6 : begin
		    $display("ADD A, d8");
		 end
		 8'hc7 : begin
		    $display("RST 00");
		    check_reset = 8;
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;
		 end
		 8'hc8 : begin
		    $display("RET Z");
		 end
		 8'hc9 : begin
		    $display("RET");
		    pre_op_PC = cpu_tb.u0.u_alu.stack_data;
		    pre_op_SP = cpu_tb.u0.u_alu.stack_pointer;
		    check_ret = 1;
		 end
		 8'hca : begin
		    $display("JP Z,a16");
		    if (cpu_tb.u0.u_alu.zero_flag == 1) begin
		       jump_a16 = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter;
		    end else begin
		       check_inc_PC = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter+1;
		    end
		 end
		 8'hcb : begin
		    $display("Prefix");
		    prefix = 1;
		 end
		 8'hcc : begin
		    $display("Call Z, a16");
		 end
		 8'hcd : begin
		    $display("Call a16");
		    call_a16 = 1;
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;
		    pre_op_SP = cpu_tb.u0.u_alu.stack_pointer;
		 end
		 8'hce : begin
		    $display("ADC a d8");
		 end
		 8'hcf : begin
		    $display("RST 08");
		    check_reset = 9;
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;		    
		 end
		 8'hd0 : begin
		    $display("RET NC");
		    if (cpu_tb.u0.u_alu.carry_flag == 0) begin
		       check_ret = 1;
		       pre_op_PC = cpu_tb.u0.u_alu.stack_data;
		       pre_op_SP = cpu_tb.u0.u_alu.stack_pointer;
		    end else begin
		       check_inc_PC = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter-1;
		    end
		 end
		 8'hd1 : begin
		    $display("POP DE");
		 end
		 8'hd2 : begin
		    $display("JP NC a16");
		    if (cpu_tb.u0.u_alu.carry_flag == 0) begin
		       jump_a16 = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter;
		    end else begin
		       check_inc_PC = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter+1;
		    end
		 end
		 8'hd4 : begin
		    $display("Call nc a16");
		    if (cpu_tb.u0.u_alu.carry_flag == 0) begin
		       call_a16 = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter;
		       pre_op_SP = cpu_tb.u0.u_alu.stack_pointer;
		    end else begin
		       check_inc_PC = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter+1;
		    end
		 end
		 8'hd5 : begin
		    $display("PUSH DE");
		 end
		 8'hd6 : begin
		    $display("Sub d8");
		 end
		 8'hd7 : begin
		    $display("RST 10");
		    check_reset = 10;
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;		    
		 end
		 8'hd8 : begin
		    $display("RET C");
		    if (cpu_tb.u0.u_alu.carry_flag == 1) begin
		       check_ret = 1;
		       pre_op_PC = cpu_tb.u0.u_alu.stack_data;
		       pre_op_SP = cpu_tb.u0.u_alu.stack_pointer;
		    end else begin
		       check_inc_PC = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter+1;
		    end
		 end
		 8'hd9 : begin
		    $display("RETI");
		 end
		 8'hda : begin
		    $display("JP C a16");
		    if (cpu_tb.u0.u_alu.carry_flag == 1) begin
		       jump_a16 = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter;
		    end else begin
		       check_inc_PC = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter+1;
		    end
		 end
		 8'hdc : begin
		    $display("Call c, a16");
		    if (cpu_tb.u0.u_alu.carry_flag == 1) begin
		       call_a16 = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter;
		       pre_op_SP = cpu_tb.u0.u_alu.stack_pointer;
		    end else begin
		       check_inc_PC = 1;
		       pre_op_PC = cpu_tb.u0.u_control.program_counter+1;
		    end
		 end
		 8'hde : begin
		    $display("sbc a d8");
		 end
		 8'hdf : begin
		    $display("rst 18");
		    check_reset = 11;
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;		    
		 end
		 8'he0 : begin
		    $display("LDH (a8) a");
		    load_mem_fast = 1;
		 end
		 8'he1 : begin
		    $display("POP HL");
		    check_pop = 1;
		    pre_op_SP = cpu_tb.u0.u_alu.stack_pointer;
		    pre_op_HL = cpu_tb.u0.u_alu.stack_data;
		 end
		 8'he2 : begin
		    $display("LD (C) a");
		    check_mem_fast = 1;
		 end
		 8'he5 : begin
		    $display("PUSH HL");
		 end
		 8'he6 : begin
		    $display("and d8");
		 end
		 8'he7 : begin
		    $display("RST 20");
		    check_reset = 12;
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;		    
		 end
		 8'he8 : begin
		    $display("Add sp, r8");
		 end
		 8'he9 : begin
		    $display("JP (HL)");
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;
		    pre_op_HL = cpu_tb.u0.hl_reg;
	            jump_hl = 1;
		 end
		 8'hea : begin
		    $display("ld (a16) a");
		    load_mem_high = 1;
		 end
		 8'hee : begin
		    $display("xor d8");
		 end
		 8'hef : begin
		    $display("rst 28");
		    check_reset = 13;
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;		    
		 end
		 8'hf0 : begin
		    $display("ldh a (d8)");
		    load_a_fast = 1;
		 end
		 8'hf1 : begin
		    $display("POP AF");
		 end
		 8'hf2 : begin
		    $display("ld a (c)");
		    load_a_fast = 2;
		 end
		 8'hf3 : begin
		    $display("DI");
		 end
		 8'hf5 : begin
		    $display("PUSH af");
		 end
		 8'hf6 : begin
		    $display("or d8");
		 end
		 8'hf7 : begin
		    $display("rst 30");
		    check_reset = 14;
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;		    
		 end
		 8'hf8 : begin
		    $display("ld hl,sp+r8");
		    check_hl_sp = 1;
		 end
		 8'hf9 : begin
		    $display("ld sp, hl");
		    check_sp_hl = 1;
		 end
		 8'hfa : begin
		    $display("ld a a16");
		    load_a_high = 1;
		 end
		 8'hfb : begin
		    $display("ei");
		 end
		 8'hfe : begin
		    $display("cp d8");
		 end
		 8'hff : begin
		    $display("rst 38");
		    check_reset = 15;
		    pre_op_PC = cpu_tb.u0.u_control.program_counter;
		 end
	       endcase
	    end else begin // if (prefix == 0)
	       case (trans.data_i)
		 8'h0 : begin
		    $display("RLC");
		    check_prefix = 1;
		    pre_op_B = cpu_tb.u0.b_reg;
		 end
		 8'h11 : begin
		    $display("RL");
		    check_prefix = 2;
		    pre_op_C = cpu_tb.u0.c_reg;
		    pre_op_flags = cpu_tb.u0.flags_reg;
		 end
		 8'h22 : begin
		    $display("SLA");
		    check_prefix = 3;
		    pre_op_D = cpu_tb.u0.d_reg;
		 end
		 8'h33 : begin
		    $display("SWAP");
		    check_prefix = 4;
		    pre_op_E = cpu_tb.u0.e_reg;
		 end
		 8'h44 : begin
		    $display("BIT");
		    check_prefix = 5;
		    pre_op_H = cpu_tb.u0.h_reg;
		 end
		 8'h55 : begin
		    $display("BIT");
		    check_prefix = 6;
		    pre_op_L = cpu_tb.u0.l_reg;
		 end
		 8'h66 : begin
		    $display("BIT");
		    check_prefix = 7;
		 end
		 8'h77 : begin
		    $display("BIT");
		    check_prefix = 8;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
		 8'h88 : begin
		    $display("RES");
		    check_prefix = 9;
		    pre_op_B = cpu_tb.u0.b_reg;
		 end
		 8'h99 : begin
		    $display("RES");
		    check_prefix = 10;
		    pre_op_C = cpu_tb.u0.c_reg;
		 end
		 8'haa : begin
		    $display("RES");
		    check_prefix = 11;
		    pre_op_D = cpu_tb.u0.d_reg;
		 end
		 8'hbb : begin
		    $display("RES");
		    check_prefix = 12;
		    pre_op_E = cpu_tb.u0.e_reg;
		 end
		 8'hcc : begin
		    $display("SET");
		    check_prefix = 13;
		    pre_op_H = cpu_tb.u0.h_reg;
		 end
		 8'hdd : begin
		    $display("SET");
		    check_prefix = 14;
		    pre_op_L = cpu_tb.u0.l_reg;
		 end
		 8'hee : begin
		    $display("SET");
		    check_prefix = 15;
		 end
		 8'hff : begin
		    $display("SET");
		    check_prefix = 16;
		    pre_op_A = cpu_tb.u0.accumulator_reg;
		 end
	       endcase
	    end // else: !if(prefix == 0)
	    	    
	    // *******************************************************
	    // Check Timing
	    // *******************************************************
	    instru_timer_2 = ($time - instru_timer_1)/1000;
	    instru_timer_1 = $time;
	    
	    if (skip_first == 1) begin
	    
	       // Timing check results
	       if (instru_timer_2 == trans.dur)
		 $display("Duration of instruction CORRECT: %t @ %t", instru_timer_2, $time);
	       else begin
		  $display("Duration of instruction INCORRECT; Expected %t; Lasted %t @ ", trans.dur, instru_timer_2, $time);
		  errors++;
	       end
	    end else
	      skip_first = 1;
	    $display("----------------------");
	 end // if (trans.m1 == 0)
	 
	 last_m1 = trans.m1;
      end
   endtask // main
endclass // scoreboard
