-------------------------------------------------------------------------------
-- Title      : CPU Control Module
-- Project    : Z80 CPU
-------------------------------------------------------------------------------
-- File       : cpu_control.vhd
-- Author     : Steve Robichaud
-- Company    : Self
-- Created    : 2022-08-14
-- Last update: 2022-09-05
-- Platform   : 
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Control module for the CPU, controls the decoding and
-- sequencing, also controls the control bus out of the chip
--
-- NOTES : Z80 has a fetch/execute overlapping system meaning  it can fetch
-- next instruction while first instruction is executed.
-- Also ops can be 1 to 4 bytes long.
--
-- Going to start with Game boy instructions as that's the end goal may come
-- back and add in IX, IY, ect after, though doubtful
--
-- Taken from Z80 User Manual (can be found in docs):
-- Notation     Description
-- r            Register (A, B, C, D, E, H, L)
-- (HL)         Mem location (At HL in notation)
-- n            Byte
-- nn           Two Bytes
-- d            Decimal (-128 to 127) So a byte
-- b            Bit
-- e            One byte signed in range -126 to 129 (Another byte offset by
--              +2) not sure how to use this
-- cc           Flag status
-- qq           BC, DE, HL or AF reg pair
-- ss, dd       BC, DE, HL or SP reg pair
-- pp           BC, DE, IX or SP reg pair
-- rr           BC, DE, IY or SP reg pair
-- s            r, n, (HL)
-- m            r, (HL)
-------------------------------------------------------------------------------
-- Copyright (c) 2022 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2022-08-14  1.0      srrob   Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity cpu_control is

  port (
    ---------------------------------------------------------------------------
    -- Clock and reset
    ---------------------------------------------------------------------------
    clk            : in  std_logic;
    reset          : in  std_logic;     -- active low
    ---------------------------------------------------------------------------
    -- Data and Address bus
    ---------------------------------------------------------------------------
    data_bus       : in  std_logic_vector(7 downto 0);
    address_bus    : out std_logic_vector(15 downto 0);
    ---------------------------------------------------------------------------
    -- Registers in
    ---------------------------------------------------------------------------
    register_bc    : in  std_logic_vector(15 downto 0);
    register_de    : in  std_logic_vector(15 downto 0);
    register_hl    : in  std_logic_vector(15 downto 0);
    stack_data     : in  std_logic_vector(15 downto 0);
    register_c     : in  std_logic_vector(7 downto 0);
    flags_reg      : in  std_logic_vector(7 downto 0);
    ---------------------------------------------------------------------------
    -- Registers out
    pc_out         : out std_logic_vector(15 downto 0);
    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    -- Control and status
    ---------------------------------------------------------------------------
    halt           : out std_logic;
    wait_n         : in  std_logic;
    int            : in  std_logic;
    nmi            : in  std_logic;
    bus_req        : in  std_logic;
    bus_ack        : out std_logic;
    mreq           : out std_logic;
    rd             : out std_logic;
    wr             : out std_logic;
    m1_o           : out std_logic;
    rfsh           : out std_logic;
    ---------------------------------------------------------------------------
    -- Internal control logic
    -- Used to update internal registers and control ALU
    ---------------------------------------------------------------------------
    update_reg     : out std_logic_vector(2 downto 0);
    increment_reg  : out std_logic_vector(2 downto 0);
    decrement_reg  : out std_logic_vector(2 downto 0);
    inc_8b_reg     : out std_logic_vector(3 downto 0);
    dec_8b_reg     : out std_logic_vector(3 downto 0);
    bit_reg        : out std_logic_vector(2 downto 0);
    bit_cmd        : out std_logic_vector(4 downto 0);
    use_cmd        : out std_logic;
    inc_db         : out std_logic;
    dec_db         : out std_logic;
    add_reg        : out std_logic_vector(3 downto 0);
    add_hl         : out std_logic_vector(2 downto 0);
    hl_pl_sp       : out std_logic;
    add_db         : out std_logic;
    use_cf         : out std_logic;
    sub_reg        : out std_logic_vector(3 downto 0);
    sub_db         : out std_logic;
    and_reg        : out std_logic_vector(3 downto 0);
    and_db         : out std_logic;
    or_reg         : out std_logic_vector(3 downto 0);
    or_db          : out std_logic;
    xor_reg        : out std_logic_vector(3 downto 0);
    xor_db         : out std_logic;
    load_regs      : out std_logic_vector(6 downto 0);
    db_to_regs     : out std_logic_vector(3 downto 0);
    reg_to_db      : out std_logic_vector(3 downto 0);
    reg_16_to_db   : out std_logic_vector(2 downto 0);
    push_reg       : out std_logic_vector(2 downto 0);
    pop_reg        : out std_logic_vector(2 downto 0);
    pc_to_sp       : out std_logic;
    pc_to_sp_p3    : out std_logic;
    sp_to_pc       : out std_logic;
    sp_add         : out std_logic;
    flag_only      : out std_logic;
    preform_daa    : out std_logic;
    not_acc        : out std_logic;
    not_cf         : out std_logic;
    set_cf         : out std_logic;
    enable_inter   : out std_logic;
    disable_inter  : out std_logic;
    rotate_l_a     : out std_logic;
    thru_cf        : out std_logic;
    rotate_r_a     : out std_logic;
    hl_ld_sp       : out std_logic;
    exchange_de_hl : out std_logic;
    exchange_af    : out std_logic;
    exchange_prime : out std_logic
    );

end entity cpu_control;

architecture rtl of cpu_control is

  -----------------------------------------------------------------------------
  -- Types
  type sequence_state is (M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12,
                          M13, M14, M15, M16, M17, M18, M19, M20, M21, M22);
  type time_steps is (T1, T2, T3, T4);
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -- Signals
  signal current_state, next_state         : sequence_state               := M1;
  signal current_time_step, next_time_step : time_steps                   := T1;
  signal program_counter                   : std_logic_vector(15 downto 0);
  signal instruction_register              : std_logic_vector(7 downto 0);
  signal interrupt_register                : std_logic_vector(7 downto 0) := x"00";
  signal refresh_register                  : std_logic_vector(7 downto 0);
  signal read_bytes_req                    : unsigned(1 downto 0)         := "00";
  signal write_bytes_req                   : unsigned(1 downto 0)         := "00";
  signal delay_loops                       : unsigned(1 downto 0)         := "00";
  signal ab_reg_sel                        : std_logic_vector(1 downto 0);
  signal Inc_PC                            : std_logic;
  signal Load_PC                           : std_logic;
  signal Load_DB                           : std_logic;
  signal Load_C                            : std_logic;
  signal Load_Reg                          : std_logic;
  signal l_halt                            : std_logic                    := '0';
  signal l_stop                            : std_logic                    := '0';
  signal jump_pc                           : std_logic;
  signal Load_Add                          : std_logic;
  signal Inc_Add                           : std_logic;
  signal PC_HL                             : std_logic;
  signal reset_val                         : std_logic_vector(2 downto 0);
  signal pc_rst                            : std_logic                    := '0';
  signal sp_to_pc_l                        : std_logic;
  signal multi_command                     : std_logic                    := '0';
  signal bus_ack_l                         : std_logic;
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -- Aliases
  alias zero_flag                          : std_logic is flags_reg(7);
  alias carry_flag                         : std_logic is flags_reg(4);
  -----------------------------------------------------------------------------

begin

  -- Send out halt
  halt   <= not(l_halt);

  sp_to_pc <= sp_to_pc_l;

  use_cmd <= multi_command;
  
  process(clk, reset)
  begin
    if reset = '0' then
      current_state     <= M1;
      current_time_step <= T4;
      bus_ack_l         <= '1';
      bus_ack           <= '1';
    elsif rising_edge(clk) then
      current_state     <= next_state;
      if current_time_step = T4 and bus_req = '0' then
        bus_ack   <= '0';
        bus_ack_l <= '0';
      elsif bus_req = '0' and bus_ack_l = '0' then
        bus_ack <= '0';
      elsif bus_req = '1' then
        current_time_step <= next_time_step;
        bus_ack   <= '1';
        bus_ack_l <= '1';
      end if;
    end if;
  end process;

  u_sequence : process (current_state, current_time_step, wait_n, flags_reg, instruction_register)
  begin

    ---------------------------------------------------------------------------
    -- Update time step
    ---------------------------------------------------------------------------
    case current_time_step is
      when T1 =>
        next_time_step <= T2;
      when T2 =>
        if wait_n = '1' then
          next_time_step <= T3;
        end if;
      when T3 =>
        next_time_step <= T4;
      when T4 =>
        next_time_step <= T1;
      when others => null;
    end case;
    ---------------------------------------------------------------------------
    -- MAIN STATE MACHINE
    ---------------------------------------------------------------------------
    load_regs      <= (others => '0');
    reg_to_db      <= (others => '0');
    reg_16_to_db   <= (others => '0');
    push_reg       <= (others => '0');
    pop_reg        <= (others => '0');
    add_hl         <= (others => '0');
    pc_to_sp       <= '0';
    pc_to_sp_p3    <= '0';
    sp_to_pc_l     <= '0';
    m1_o           <= '1';
    rfsh           <= '1';
    Load_PC        <= '0';
    Load_DB        <= '0';
    Load_C         <= '0';
    pc_rst         <= '0';
    PC_HL          <= '0';
    hl_pl_sp       <= '0';
    Load_Reg       <= '0';
    sp_add         <= '0';
    inc_db         <= '0';
    dec_db         <= '0';
    increment_reg  <= "000";
    decrement_reg  <= "000";
    inc_8b_reg     <= x"0";
    dec_8b_reg     <= x"0";
    add_reg        <= x"0";
    add_db         <= '0';
    sub_reg        <= x"0";
    sub_db         <= '0';
    and_reg        <= x"0";
    and_db         <= '0';
    or_reg         <= x"0";
    or_db          <= '0';
    xor_reg        <= x"0";
    xor_db         <= '0';
    preform_daa    <= '0';
    not_acc        <= '0';
    not_cf         <= '0';
    set_cf         <= '0';
    disable_inter  <= '0';
    enable_inter   <= '0';
    rotate_l_a     <= '0';
    thru_cf        <= '0';
    rotate_r_a     <= '0';
    Load_Add       <= '0';
    Inc_Add        <= '0';
    hl_ld_sp       <= '0';
    exchange_de_hl <= '0';
    exchange_af    <= '0';
    exchange_prime <= '0';
    case current_state is
      when M1 =>                        -- FETCH
        ---------------------------------------------------------------------------
        -- Reset signals to defaults
        ---------------------------------------------------------------------------    
        update_reg <= "000";
        db_to_regs <= (others => '0');
        ab_reg_sel <= "00";
        use_cf     <= '0';
        flag_only  <= '0';
        Inc_PC     <= '0';
        jump_pc    <= '0';
        if current_time_step = T1 then
          Inc_PC <= '1';
          m1_o   <= '0';
        elsif current_time_step = T2 then
          m1_o <= '0';
        elsif current_time_step = T3 then
          rfsh <= '0';
        else
          rfsh <= '0';
          -------------------------------------------------------------------
          -- DECODE INSTRUCTION
          -------------------------------------------------------------------
          if multi_command = '0' then
            case instruction_register is

              -- NOP
              when x"00" =>
                Load_PC <= '1';

              -- Load dd, nn
              when x"01" | x"11" | x"21" | x"31" =>
                next_state     <= M2;
                read_bytes_req <= "01";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                update_reg     <= instruction_register(5 downto 4) & '1';

              -- Load (BC), A; Load (DE), A
              when x"02" | x"12" =>
                next_state      <= M3;
                write_bytes_req <= "00";
                Load_Reg        <= '1';
                ab_reg_sel      <= instruction_register(5 downto 4);

              -- INC ss
              when x"03" | x"13" | x"23" | x"33" =>
                next_state    <= M15;
                delay_loops   <= "00";
                increment_reg <= instruction_register(5 downto 4) & '1';

              -- INC r
              when x"04" | x"0C" | x"14" | x"1C" | x"24" | x"2C" | x"3C" =>
                inc_8b_reg <= instruction_register(5 downto 3) & '1';

              -- INC (HL)
              when x"34" =>
                next_state     <= M5;
                read_bytes_req <= "00";
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";

              -- DEC ss
              when x"0B" | x"1B" | x"2B" | x"3B" =>
                next_state    <= M15;
                delay_loops   <= "00";
                decrement_reg <= instruction_register(5 downto 4) & '1';

              -- DEC r
              when x"05" | x"0D" | x"15" | x"1D" | x"25" | x"2D" | x"3D" =>
                dec_8b_reg <= instruction_register(5 downto 3) & '1';

              -- DEC (HL)
              when x"35" =>
                next_state     <= M6;
                read_bytes_req <= "00";
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";

              -- Load r,r'
              when x"40" | x"41" | x"42" | x"43" | x"44" | x"45" | x"47" |
                x"48" | x"49" | x"4A" | x"4B" | x"4C" | x"4D" | x"4F" |
                x"50" | x"51" | x"52" | x"53" | x"54" | x"55" | x"57" |
                x"58" | x"59" | x"5A" | x"5B" | x"5C" | x"5D" | x"5F" |
                x"60" | x"61" | x"62" | x"63" | x"64" | x"65" | x"67" |
                x"68" | x"69" | x"6A" | x"6B" | x"6C" | x"6D" | x"6F" |
                x"78" | x"79" | x"7A" | x"7B" | x"7C" | x"7D" | x"7F" =>
                load_regs <= instruction_register(5 downto 0) & '1';

              -- Load r,n
              when x"06" | x"0E" | x"16" | x"1E" | x"26" | x"2E" | x"3E" =>
                next_state     <= M2;
                read_bytes_req <= "00";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                db_to_regs     <= instruction_register(5 downto 3) & '1';

              -- Load r, (HL)
              when x"46" | x"4E" | x"56" | x"5E" | x"66" | x"6E" | x"7E" =>
                next_state     <= M2;
                read_bytes_req <= "00";
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";
                db_to_regs     <= instruction_register(5 downto 3) & '1';

              -- Load (HL), r
              when x"70" | x"71" | x"72" | x"73" | x"74" | x"75" | x"77" =>
                next_state      <= M3;
                write_bytes_req <= "00";
                Load_Reg        <= '1';
                ab_reg_sel      <= "10";
                reg_to_db       <= instruction_register(2 downto 0) & '1';

              -- Load (HL), n
              when x"36" =>
                next_state     <= M12;
                read_bytes_req <= "00";
                Inc_PC         <= '1';
                Load_PC        <= '1';

              -- Load A, (BC); Load A, (DE)
              when x"0A" | x"1A" =>
                next_state     <= M2;
                read_bytes_req <= "00";
                Load_Reg       <= '1';
                ab_reg_sel     <= instruction_register(5 downto 4);
                db_to_regs     <= x"F";

              -- (GB) LD A, (HL-); (Z80) Load A, (nn)
              when x"3A" =>
                next_state     <= M2;
                read_bytes_req <= "00";
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";
                db_to_regs     <= x"F";
                decrement_reg  <= "101";

              -- (GB) LD (HL-), A; (Z80) Load (nn), A
              when x"32" =>
                next_state      <= M3;
                write_bytes_req <= "00";
                Load_Reg        <= '1';
                ab_reg_sel      <= "10";
                reg_to_db       <= x"F";
                decrement_reg   <= "101";

              -- (GB) LD (HL+), A; (Z80) Load (nn), HL
              when x"22" =>
                next_state      <= M3;
                write_bytes_req <= "00";
                Load_Reg        <= '1';
                ab_reg_sel      <= "10";
                reg_to_db       <= x"F";
                increment_reg   <= "101";
                
              -- (GB) LD A, (HL+); (Z80) Load HL, (nn)
              when x"2A" =>
                next_state     <= M2;
                read_bytes_req <= "00";
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";
                db_to_regs     <= x"F";
                increment_reg  <= "101";

              -- Load SP, HL
              when x"F9" =>
                next_state  <= M15;
                delay_loops <= "00";
                hl_ld_sp    <= '1';

              -- Push qq
              when x"C5" | x"D5" | x"E5" | x"F5" =>
                next_state  <= M15;
                delay_loops <= "10";
                push_reg    <= instruction_register(5 downto 4) & '1';

              --Pop qq
              when x"C1" | x"D1" | x"E1" | x"F1" =>
                next_state  <= M15;
                delay_loops <= "01";
                pop_reg     <= instruction_register(5 downto 4) & '1';

              -- Exchange DE, HL
              when x"EB" =>
                exchange_de_hl <= '1';

              -- (GB) LD (nn), SP; (Z80)Exchange AF, AF'
              when x"08" =>
                --exchange_af <= '1';
                next_state     <= M13;
                read_bytes_req <= "01";
                Inc_PC         <= '1';
                Load_PC        <= '1';

              -- Exchange (Swap prime and not prime)
              when x"D9" =>
                exchange_prime <= '1';

              -- Exchange (SP), HL
              when x"E3" =>
                null;

              -- Add A, r
              when x"80" | x"81" | x"82" | x"83" | x"84" | x"85" | x"87" =>
                add_reg <= instruction_register(2 downto 0) & '1';

              -- Add A, n
              when x"C6" =>
                next_state     <= M7;
                Inc_PC         <= '1';
                Load_PC        <= '1';
                read_bytes_req <= "00";

              -- Add A, (HL)
              when x"86" =>
                next_state     <= M7;
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";
                read_bytes_req <= "00";

              -- Add with Carry A, r
              when x"88" | x"89" | x"8A" | x"8B" | x"8C" | x"8D" | x"8F" =>
                add_reg <= instruction_register(2 downto 0) & '1';
                use_cf  <= '1';

              -- ADC A, n
              when x"CE" =>
                next_state     <= M7;
                Inc_PC         <= '1';
                Load_PC        <= '1';
                use_cf         <= '1';
                read_bytes_req <= "00";

              -- ADC A, (HL)
              when x"8E" =>
                next_state     <= M7;
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";
                use_cf         <= '1';
                read_bytes_req <= "00";

              -- ADD HL, ss
              when x"09" | x"19" | x"29" | x"39" =>
                next_state  <= M15;
                delay_loops <= "00";
                add_hl      <= instruction_register(5 downto 4) & '1';

              -- Sub A, r
              when x"90" | x"91" | x"92" | x"93" | x"94" | x"95" | x"97" =>
                sub_reg <= instruction_register(2 downto 0) & '1';

              -- Sub A, n
              when x"D6" =>
                next_state     <= M8;
                Inc_PC         <= '1';
                Load_PC        <= '1';
                read_bytes_req <= "00";

              -- Sub A, (HL)
              when x"96" =>
                next_state     <= M8;
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";
                read_bytes_req <= "00";

              -- SBC A, r
              when x"98" | x"99" | x"9A" | x"9B" | x"9C" | x"9D" | x"9F" =>
                sub_reg <= instruction_register(2 downto 0) & '1';
                use_cf  <= '1';

              -- SBC A, n
              when x"DE" =>
                next_state     <= M8;
                Inc_PC         <= '1';
                Load_PC        <= '1';
                use_cf         <= '1';
                read_bytes_req <= "00";

              -- SBC A, (HL)
              when x"9E" =>
                next_state     <= M8;
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";
                use_cf         <= '1';
                read_bytes_req <= "00";

              -- AND r
              when x"A0" | x"A1" | x"A2" | x"A3" | x"A4" | x"A5" | x"A7" =>
                and_reg <= instruction_register(2 downto 0) & '1';

              -- And n
              when x"E6" =>
                next_state     <= M9;
                Inc_PC         <= '1';
                Load_PC        <= '1';
                read_bytes_req <= "00";

              -- And (HL)
              when x"A6" =>
                next_state     <= M9;
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";
                read_bytes_req <= "00";

              -- OR r
              when x"B0" | x"B1" | x"B2" | x"B3" | x"B4" | x"B5" | x"B7" =>
                or_reg <= instruction_register(2 downto 0) & '1';

              -- OR n
              when x"F6" =>
                next_state     <= M10;
                Inc_PC         <= '1';
                Load_PC        <= '1';
                read_bytes_req <= "00";

              -- Or (HL)
              when x"B6" =>
                next_state     <= M10;
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";
                read_bytes_req <= "00";

              -- XOR r
              when x"A8" | x"A9" | x"AA" | x"AB" | x"AC" | x"AD"| x"AF" =>
                xor_reg <= instruction_register(2 downto 0) & '1';

              -- XOR n
              when x"EE" =>
                next_state     <= M11;
                Inc_PC         <= '1';
                Load_PC        <= '1';
                read_bytes_req <= "00";

              -- XOR (HL)
              when x"AE" =>
                next_state     <= M11;
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";
                read_bytes_req <= "00";

              --CP r
              when x"B8" | x"B9" | x"BA" | x"BB" | x"BC" | x"BD" | x"BF" =>
                sub_reg   <= instruction_register(2 downto 0) & '1';
                flag_only <= '1';

              -- CP n
              when x"FE" =>
                next_state     <= M8;
                flag_only      <= '1';
                Inc_PC         <= '1';
                Load_PC        <= '1';
                read_bytes_req <= "00";

              -- CP (HL)
              when x"BE" =>
                next_state     <= M8;
                flag_only      <= '1';
                Load_Reg       <= '1';
                ab_reg_sel     <= "10";
                read_bytes_req <= "00";

              -- DAA
              when x"27" =>
                preform_daa <= '1';

              -- Not A
              when x"2F" =>
                not_acc <= '1';

              -- NOT Carry flag
              when x"3F" =>
                not_cf <= '1';

              -- Set Carry
              when x"37" =>
                set_cf <= '1';

              -- Halt
              when x"76" =>
                l_halt <= '1';

              -- DI (diables maskable interrupt; sets IFF1 & 2)
              when x"F3" =>
                disable_inter <= '1';

              -- EI (Enable maskable interrupt)
              when x"FB" =>
                enable_inter <= '1';

              -- RLCA
              when x"07" =>
                rotate_l_a <= '1';

              -- RLA
              when x"17" =>
                rotate_l_a <= '1';
                thru_cf    <= '1';

              -- RRCA
              when x"0F" =>
                rotate_r_a <= '1';

              -- RRA
              when x"1F" =>
                rotate_r_a <= '1';
                thru_cf    <= '1';

              -- JP nn
              when x"C3" =>
                next_state     <= M2;
                read_bytes_req <= "01";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                jump_pc        <= '1';

              -- JP cc, nn
              when x"C2" | x"CA" | x"D2" | x"DA" =>
                next_state     <= M2;
                read_bytes_req <= "01";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                if instruction_register(5 downto 3) = "000" and zero_flag = '0' then
                  jump_pc <= '1';
                elsif instruction_register(5 downto 3) = "001" and zero_flag = '1' then
                  jump_pc <= '1';
                elsif instruction_register(5 downto 3) = "010" and carry_flag = '0' then
                  jump_pc <= '1';
                elsif instruction_register(5 downto 3) = "011" and carry_flag = '1' then
                  jump_pc <= '1';
                end if;

              -- JR e
              when x"18" =>
                next_state     <= M2;
                read_bytes_req <= "00";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                jump_pc        <= '1';

              -- JR C, e
              when x"38" =>
                next_state     <= M2;
                read_bytes_req <= "00";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                if carry_flag = '1' then
                  jump_pc        <= '1';
                  delay_loops    <= "01";
                end if;

              -- JR NC, e
              when x"30" =>
                next_state     <= M2;
                read_bytes_req <= "00";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                if carry_flag = '0' then
                  jump_pc        <= '1';
                  delay_loops    <= "01";
                end if;

              -- JR Z, e
              when x"28" =>
                next_state     <= M2;
                read_bytes_req <= "00";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                if zero_flag = '1' then
                  jump_pc        <= '1';
                  delay_loops    <= "01";
                end if;

              -- JR NZ, e
              when x"20" =>
                next_state     <= M2;
                read_bytes_req <= "00";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                if zero_flag = '0' then
                  jump_pc        <= '1';
                  delay_loops    <= "01";
                end if;

              -- JP (HL)
              when x"E9" =>
                PC_HL <= '1';
                

              -- (GB) STOP; (Z80) DJNZ, e
              when x"10" =>
                l_stop <= '1';

              -- CALL nn
              when x"CD" =>
                next_state     <= M2;
                read_bytes_req <= "01";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                jump_pc        <= '1';
                pc_to_sp_p3    <= '1';
                

              -- CALL cc, nn
              when x"C4" | x"CC" | x"D4" | x"DC" => 
                next_state     <= M2;
                read_bytes_req <= "01";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                if instruction_register(5 downto 3) = "000" and zero_flag = '0' then
                  jump_pc     <= '1';
                  pc_to_sp_p3 <= '1';
                  delay_loops <= "10";
                elsif instruction_register(5 downto 3) = "001" and zero_flag = '1' then
                  jump_pc     <= '1';
                  pc_to_sp_p3 <= '1';
                  delay_loops <= "10";
                elsif instruction_register(5 downto 3) = "010" and carry_flag = '0' then
                  jump_pc     <= '1';
                  pc_to_sp_p3 <= '1';
                  delay_loops <= "10";
                elsif instruction_register(5 downto 3) = "011" and carry_flag = '1' then
                  jump_pc     <= '1';
                  pc_to_sp_p3 <= '1';
                  delay_loops <= "10";
                end if;

              -- RET
              when x"C9" =>
                next_state  <= M15;
                delay_loops <= "01";
                sp_to_pc_l  <= '1';

              -- RET cc
              when x"C0" | x"C8" | x"D0" | x"D8" =>
                next_state  <= M15;
                if instruction_register(5 downto 3) = "000" and zero_flag = '0' then
                  delay_loops <= "11";
                  sp_to_pc_l  <= '1';
                elsif instruction_register(5 downto 3) = "001" and zero_flag = '1' then
                  delay_loops <= "11";
                  sp_to_pc_l  <= '1';
                elsif instruction_register(5 downto 3) = "010" and carry_flag = '0' then
                  delay_loops <= "11";
                  sp_to_pc_l  <= '1';
                elsif instruction_register(5 downto 3) = "011" and carry_flag = '1' then
                  delay_loops <= "11";
                  sp_to_pc_l  <= '1';
                else
                  delay_loops <= "00";
                end if;

              -- RST p
              when x"C7" | x"CF" | x"D7" | x"DF" | x"E7" | x"EF" | x"F7" | x"FF" =>
                next_state  <= M15;
                delay_loops <= "01";
                pc_rst      <= '1';
                reset_val   <= instruction_register(5 downto 3);
                pc_to_sp    <= '1';

              -- LDH (n), A (GB only)
              when x"E0" =>
                next_state     <= M16;
                read_bytes_req <= "00";
                Load_PC        <= '1';

              -- LD (C), A (GB only)
              when x"E2" =>
                next_state      <= M3;
                write_bytes_req <= "00";
                Load_C          <= '1';
                reg_to_db       <= x"F";

              -- ADD SP, e
              when x"E8" =>
                next_state     <= M19;
                read_bytes_req <= "00";
                Load_PC        <= '1';
                delay_loops    <= "10";

              -- LD (nn), A
              when x"EA" =>
                next_state     <= M17;
                read_bytes_req <= "01";
                Inc_PC         <= '1';
                Load_PC        <= '1';

              -- LDH A, (n)
              when x"F0" =>
                next_state     <= M18;
                read_bytes_req <= "01";
                Load_PC        <= '1';

              -- LD A, (C)
              when x"F2" =>
                next_state     <= M2;
                read_bytes_req <= "00";
                Load_C         <= '1';
                db_to_regs     <= x"F";

              -- LD HL, SP+e
              when x"F8" =>
                next_state     <= M20;
                read_bytes_req <= "00";
                Load_PC        <= '1';
                delay_loops    <= "01";

              -- LD A, (nn)
              when x"FA" =>
                next_state     <= M18;
                read_bytes_req <= "10";
                Inc_PC         <= '1';
                Load_PC        <= '1';
                db_to_regs     <= x"F";

              -----------------------------------------------------------------
              -- Multi-Byte OPS
              -----------------------------------------------------------------
              when x"CB" =>
                multi_command <= '1';

              when others => null;
            end case;
          else
            multi_command <= '0';
            bit_reg       <= instruction_register(2 downto 0);
            bit_cmd       <= instruction_register(7 downto 3);
            if instruction_register(2 downto 0) = "110" then
              next_state     <= M4;
              read_bytes_req <= "00";
              Load_Reg       <= '1';
              ab_reg_sel     <= "10";
            end if;
          end if;
        end if;
      -----------------------------------------------------------------------
      -- READ BYTES
      -----------------------------------------------------------------------
      when M2 | M4 | M5 | M6 | M7 | M8 | M9 | M10 | M11 | M12 | M13 | M16 | M17 | M18 | M19 | M20 =>
        if current_time_step = T4 then
          if not(current_state = M4 or current_state = M5 or current_state = M6 or
                 current_state = M12 or current_state = M13 or current_state = M16 or
                 current_state = M17 or current_state = M18) then
            Load_PC <= '1';
          elsif current_state = M13 then
            if read_bytes_req = "01" then
              Load_Add <= '1';
              Inc_PC   <= '0';
            else
              Load_Add <= '1';
            end if;
          elsif current_state = M16 or current_state = M18 then
            Load_DB <= '1';
            if read_bytes_req = "01" then
              Inc_PC <= '0';
            end if;
          elsif current_state = M17 then
            if read_bytes_req = "01" then
              Load_PC  <= '1';
              Load_Add <= '1';
            else
              Load_Add <= '1';
            end if;
          else
            Load_Reg <= '1';
          end if;
          if read_bytes_req = "00" then
            if not(current_state = M4 or current_state = M5 or current_state = M6 or
                   current_state = M12 or current_state = M13 or current_state = M16 or
                   current_state = M17) then
              if delay_loops = "00" then
                next_state <= M1;
              else
                delay_loops <= delay_loops - "1";
                next_state  <= M15;
                Inc_PC      <= '0';
              end if;
              
              case current_state is
                when M7     => add_db     <= '1';
                when M8     => sub_db     <= '1';
                when M9     => and_db     <= '1';
                when M10    => or_db      <= '1';
                when M11    => xor_db     <= '1';
                when M19    => sp_add     <= '1';
                when M20    => hl_pl_sp   <= '1';
                when others => null;
              end case;
            elsif current_state = M4 or current_state = M12 then
              next_state      <= M3;
              write_bytes_req <= "00";
              ab_reg_sel      <= "10";
            elsif current_state = M13 then
              next_state      <= M14;
              write_bytes_req <= "01";
              reg_16_to_db    <= "111";
            elsif current_state = M16 or current_state = M17 then
              next_state      <= M3;
              write_bytes_req <= "00";
              reg_to_db       <= x"F";
              Inc_PC          <= '0';
            else
              next_state      <= M3;
              write_bytes_req <= "00";
              if current_state = M5 then
                inc_db <= '1';
              elsif current_state = M6 then
                dec_db <= '1';
              end if;
            end if;
          else
            read_bytes_req <= read_bytes_req - "1";
          end if;
        end if;
        
      -----------------------------------------------------------------------
      -- WRITE BYTES
      -----------------------------------------------------------------------
      when M3 | M14 =>
        if current_time_step = T4 then
          if current_state = M3 then
            Load_PC <= '1';
          else
            Inc_Add      <= '1';
            reg_16_to_db <= "111";
          end if;

          if write_bytes_req = "00" then
            if current_state = M14 then
              next_state <= M15;
            else
              next_state <= M1;  
            end if;
          else
            write_bytes_req <= write_bytes_req - "1";
          end if;
        end if;
      -------------------------------------------------------------------------
      -- Extra clock cycles?
      -- Not sure why increment 16 bit registers take two extra clocks
      -------------------------------------------------------------------------
      when M15 =>
        if current_time_step = T4 then
          if delay_loops = "00" then
            next_state <= M1;
          else
            delay_loops <= delay_loops - "1";
          end if;
        end if;
      when others => null;
    end case;
  end process;

  -----------------------------------------------------------------------------
  -- Program counter
  --
  -- Falling edge of the first clock in the fetch state, increments the PC.
  -----------------------------------------------------------------------------
  u_pc : process(clk, reset)
    variable temp_pc : std_logic_vector(15 downto 0);
    variable update  : std_logic;
    variable toggle  : std_logic;
  begin
    if reset = '0' then
      program_counter <= (others => '0');
      temp_pc         := (others => '0');
      pc_out          <= (others => '0');
      update          := '0';
      toggle          := '0';
    elsif falling_edge(clk) then
      pc_out <= program_counter;
      if (Inc_PC = '1' and current_time_step = T1) and l_halt = '0' and l_stop = '0' then
        program_counter <= std_logic_vector(unsigned(program_counter) + "1");
      elsif update = '1' then
        program_counter <= temp_pc;
      elsif PC_HL = '1' then
        program_counter <= register_hl;
      elsif sp_to_pc_l = '1' then
        program_counter <= stack_data;
      elsif pc_rst = '1' then
        case reset_val is
          when "000" => program_counter <= x"0000";
          when "001" => program_counter <= x"0008";
          when "010" => program_counter <= x"0010";
          when "011" => program_counter <= x"0018";
          when "100" => program_counter <= x"0020";
          when "101" => program_counter <= x"0028";
          when "110" => program_counter <= x"0030";
          when "111" => program_counter <= x"0038";
          when others => null;
        end case;
      end if;
      if jump_pc = '1' then
        if current_state = M2 and current_time_step = T3 then
          if instruction_register = x"18" or instruction_register = x"38" or instruction_register = x"30" or
          instruction_register = x"28" or instruction_register = x"20" then
            update  := '1';
            temp_pc := std_logic_vector(resize((signed('0' & program_counter) + signed(data_bus) - x"2"), 16));
          else
            if toggle = '0' then
              temp_pc(7 downto 0) := data_bus;
            else
              temp_pc(15 downto 8) := data_bus;
              update               := '1';
            end if;
            toggle := not(toggle);
          end if;
        end if;
      end if;
      update := '0';
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Address bus
  -----------------------------------------------------------------------------
  u_ab : process(clk, reset)
    variable temp_ab : std_logic_vector(15 downto 0);
    variable update  : std_logic;
    variable toggle  : std_logic;
  begin
    if reset = '0' then
      address_bus <= (others => '0');
      temp_ab     := (others => '0');
      update      := '0';
      toggle      := '0';
    elsif rising_edge(clk) then
      if Load_PC = '1' then
        address_bus <= program_counter;
      elsif update = '1' then
        address_bus <= temp_ab;
      elsif current_state = M1 and current_time_step = T2 then
        address_bus <= interrupt_register & refresh_register;
      elsif Load_DB = '1' then
        address_bus <= x"FF" & data_bus;
      elsif Load_C = '1' then
        address_bus <= x"FF" & register_c;
      elsif Load_Reg = '1' then
        if ab_reg_sel = "00" then
          address_bus <= register_bc;
        elsif ab_reg_sel = "01" then
          address_bus <= register_de;
        elsif ab_reg_sel = "10" then
          address_bus <= register_hl;
        end if;
      end if;
      update := '0';
      if Load_Add = '1' then
        if toggle = '0' then
          temp_ab(7 downto 0) := data_bus;
        else
          temp_ab(15 downto 8) := data_bus;
          update               := '1';
        end if;
        toggle := not(toggle);
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Instruction register
  -----------------------------------------------------------------------------
  u_ir : process(clk, reset)
  begin
    if reset = '0' then
      instruction_register <= (others => '0');
    elsif falling_edge(clk) then
      if current_state = M1 and current_time_step = T2 then
        if l_halt = '0' and l_stop = '0' then
          instruction_register <= data_bus;
        else
          instruction_register <= x"00";  -- NOP
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- MREQ logic
  -----------------------------------------------------------------------------
  u_mreq : process(clk, reset)
  begin
    if reset = '0' then
      mreq <= '1';
    elsif rising_edge(clk) then
      if (current_state = M1 or current_state = M2) and current_time_step = T1 then
        mreq <= '0';
      elsif current_state = M1 and current_time_step = T3 then
        mreq <= '0';
      elsif current_state = M3 and current_time_step = T2 then
        mreq <= '0';
      else
        mreq <= '1';
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- RD logic
  -----------------------------------------------------------------------------
  u_rd : process(clk, reset)
  begin
    if reset = '0' then
      rd <= '1';
    elsif rising_edge(clk) then
      if (current_state = M1 or current_state = M2)and current_time_step = T1 then
        rd <= '0';
      else
        rd <= '1';
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- WR logic
  -----------------------------------------------------------------------------
  u_wr : process(clk, reset)
  begin
    if reset = '0' then
      wr <= '1';
    elsif rising_edge(clk) then
      if (current_state = M3 or current_state = M14) and current_time_step = T2 then
        wr <= '0';
      else
        wr <= '1';
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Refresh register
  -----------------------------------------------------------------------------
  u_rfsh : process(clk, reset)
  begin
    if reset = '0' then
      refresh_register <= (others => '0');
    elsif falling_edge(clk) then
      if current_state = M1 and current_time_step = T3 then
        refresh_register <= std_logic_vector(unsigned(refresh_register) + "1");
      end if;
    end if;
  end process;

end architecture rtl;
