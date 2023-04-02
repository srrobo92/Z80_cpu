// CPU Interface
interface cpu_if (input bit clk);
   logic m1;
   logic mreq;
   logic rd;
   logic wr;
   logic rfsh;
   logic wait_n;
   logic halt;
   logic interrupt;
   logic nmi;
   logic reset;
   logic busreq;
   logic busack;
   logic [15:0] addr_bus;
   logic [7:0] 	data_i;
   logic [7:0] 	data_o;
   time 	dur;
   logic 	op_or_data;
endinterface // cpu_if
