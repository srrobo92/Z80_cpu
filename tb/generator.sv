class generator;

   transaction trans;

   mailbox gen2driv;

   event   ended;

   int 	   fd, no_transactions, num_data_bytes;

   string  test_set;

   function new(mailbox gen2driv, string test_set);

      this.gen2driv = gen2driv;
      this.test_set = test_set;
      
   endfunction // new

   task main();
      fd = $fopen(this.test_set, "rb");
      trans = new();
      if ( fd ) begin
	while ($fscanf(fd, "%x,%b,%b,%b,%b,%d,%d,%b", trans.data_i, trans.wait_n, trans.interrupt, trans.nmi, trans.busreq, trans.dur, num_data_bytes, trans.op_or_data) == 8) begin
	   //trans.display("[Generator]");
	   gen2driv.put(trans);
	   no_transactions++;
	end
      end else begin
	 $display("File %s was not opened", this.test_set);
	 $fclose(fd);
         $finish;
      end // else: !if( fd )
      $fclose(fd);
      -> ended;
   endtask // main

endclass // generator
