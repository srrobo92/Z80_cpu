// Samples interface, captures into transaction and send to scorboard

class monitor;

   // Virtual interface
   virtual cpu_if vif;

   // mailbox handle
   mailbox mon2scb;


   // constructor
   function new(virtual cpu_if vif, mailbox mon2scb);
      this.vif = vif;
      this.mon2scb = mon2scb;
   endfunction // new

   task main();
      forever begin
	 transaction trans;
	 trans = new();
	 @(posedge vif.clk);
	 // Load vif to trans
  
	 trans.m1 = vif.m1;
	 trans.data_i = vif.data_i;
	 trans.data_o = vif.data_o;
	 trans.wait_n = vif.wait_n;
	 trans.interrupt = vif.interrupt;
	 trans.nmi = vif.nmi;
	 trans.mreq = vif.mreq;
	 trans.rd = vif.rd;
	 trans.wr = vif.wr;
	 trans.halt = vif.halt;
	 trans.busreq = vif.busreq;
	 trans.busack = vif.busack;
	 trans.addr_bus = vif.addr_bus;
	 trans.dur = vif.dur;
	 mon2scb.put(trans);
	 //trans.display("[Monitor]");
      end
   endtask // main
endclass // monitor
