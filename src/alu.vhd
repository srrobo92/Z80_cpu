-------------------------------------------------------------------------------
-- Title      : ALU
-- Project    : 
-------------------------------------------------------------------------------
-- File       : alu.vhd
-- Author     : Steve Robichaud
-- Company    : Self
-- Created    : 2022-09-03
-- Last update: 2022-09-06
-- Platform   : 
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Arithmatic Logic Unit for Z80 CPU
-- Granted I'm putting more in than just the ALU, also contains all the stack
-- pointer logic, and general register logic for that matter.
-------------------------------------------------------------------------------
-- Copyright (c) 2022 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2022-09-03  1.0      srrob   Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity alu is

  port (
    ---------------------------------------------------------------------------
    -- Clock and Reset
    ---------------------------------------------------------------------------
    clk             : in  std_logic;
    reset           : in  std_logic;
    ---------------------------------------------------------------------------
    -- Top level control/data
    ---------------------------------------------------------------------------
    rd              : in  std_logic;
    data_bus_i      : in  std_logic_vector(7 downto 0);
    data_bus_o      : out std_logic_vector(7 downto 0);
    ---------------------------------------------------------------------------
    -- Registers out
    ---------------------------------------------------------------------------
    flags_reg       : out std_logic_vector(7 downto 0);
    b_reg           : out std_logic_vector(7 downto 0);
    c_reg           : out std_logic_vector(7 downto 0);
    d_reg           : out std_logic_vector(7 downto 0);
    e_reg           : out std_logic_vector(7 downto 0);
    accumulator_reg : out std_logic_vector(7 downto 0);
    h_reg           : out std_logic_vector(7 downto 0);
    l_reg           : out std_logic_vector(7 downto 0);
    stack_data      : out std_logic_vector(15 downto 0);
    ---------------------------------------------------------------------------
    -- Control lines in
    ---------------------------------------------------------------------------
    pc_in           : in  std_logic_vector(15 downto 0);
    update_reg      : in  std_logic_vector(2 downto 0);
    increment_reg   : in  std_logic_vector(2 downto 0);
    decrement_reg   : in  std_logic_vector(2 downto 0);
    inc_8b_reg      : in  std_logic_vector(3 downto 0);
    dec_8b_reg      : in  std_logic_vector(3 downto 0);
    bit_reg         : in  std_logic_vector(2 downto 0);
    bit_cmd         : in  std_logic_vector(4 downto 0);
    use_cmd         : in  std_logic;
    inc_db          : in  std_logic;
    dec_db          : in  std_logic;
    add_reg         : in  std_logic_vector(3 downto 0);
    add_hl          : in  std_logic_vector(2 downto 0);
    hl_pl_sp        : in  std_logic;
    add_db          : in  std_logic;
    use_cf          : in  std_logic;
    sub_reg         : in  std_logic_vector(3 downto 0);
    sub_db          : in  std_logic;
    and_reg         : in  std_logic_vector(3 downto 0);
    and_db          : in  std_logic;
    or_reg          : in  std_logic_vector(3 downto 0);
    or_db           : in  std_logic;
    xor_reg         : in  std_logic_vector(3 downto 0);
    xor_db          : in  std_logic;
    load_regs       : in  std_logic_vector(6 downto 0);
    db_to_regs      : in  std_logic_vector(3 downto 0);
    reg_to_db       : in  std_logic_vector(3 downto 0);
    reg_16_to_db    : in  std_logic_vector(2 downto 0);
    push_reg        : in  std_logic_vector(2 downto 0);
    pop_reg         : in  std_logic_vector(2 downto 0);
    pc_to_sp        : in  std_logic;
    pc_to_sp_p3     : in  std_logic;
    sp_to_pc        : in  std_logic;
    sp_add          : in  std_logic;
    flag_only       : in  std_logic;
    preform_daa     : in  std_logic;
    not_acc         : in  std_logic;
    not_cf          : in  std_logic;
    set_cf          : in  std_logic;
    rotate_l_a      : in  std_logic;
    thru_cf         : in  std_logic;
    rotate_r_a      : in  std_logic;
    hl_ld_sp        : in  std_logic
    );

end entity alu;

architecture rtl of alu is

  -----------------------------------------------------------------------------
  -- Types
  type mem is array (65535 downto 0) of std_logic_vector(7 downto 0);
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -- Signals
  signal accum_local       : std_logic_vector(7 downto 0);
  signal flags_reg_local   : std_logic_vector(7 downto 0)  := (others => '0');
  signal reg_bc            : std_logic_vector(15 downto 0);
  signal reg_de            : std_logic_vector(15 downto 0);
  signal reg_hl            : std_logic_vector(15 downto 0);
  signal stack_pointer     : unsigned(15 downto 0);
  signal stack_pointer_mem : mem                           := (others => (others => '0'));
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -- Aliases
  alias carry_flag         : std_logic is flags_reg_local(4);
  alias half_carry_flag    : std_logic is flags_reg_local(5);
  alias sub_flag           : std_logic is flags_reg_local(6);
  alias zero_flag          : std_logic is flags_reg_local(7);
  alias reg_b_local        : std_logic_vector(7 downto 0) is reg_bc(15 downto 8);
  alias reg_c_local        : std_logic_vector(7 downto 0) is reg_bc(7 downto 0);
  alias reg_d_local        : std_logic_vector(7 downto 0) is reg_de(15 downto 8);
  alias reg_e_local        : std_logic_vector(7 downto 0) is reg_de(7 downto 0);
  alias reg_h_local        : std_logic_vector(7 downto 0) is reg_hl(15 downto 8);
  alias reg_l_local        : std_logic_vector(7 downto 0) is reg_hl(7 downto 0);
  -----------------------------------------------------------------------------

begin

  u_alu : process(clk, reset)
    variable toggle               : std_logic := '0';
    variable reg_results          : unsigned(8 downto 0);
    variable half_carry_results   : unsigned(4 downto 0);
    variable reg16_results        : unsigned(16 downto 0);
    variable half_carry16_results : unsigned(12 downto 0);
  begin
    if reset = '0' then
      accumulator_reg    <= (others => '0');
      b_reg              <= (others => '0');
      c_reg              <= (others => '0');
      d_reg              <= (others => '0');
      e_reg              <= (others => '0');
      h_reg              <= (others => '0');
      l_reg              <= (others => '0');
      reg_bc             <= (others => '0');
      reg_de             <= (others => '0');
      reg_hl             <= (others => '0');
      stack_pointer      <= x"FFFE";
      stack_pointer_mem  <= (others => (others => '0'));
      toggle             := '0';
      reg_results        := (others => '0');
      half_carry_results := (others => '0');
      flags_reg          <= (others => '0');
      flags_reg_local    <= (others => '0');
    elsif rising_edge(clk) then
      -- UPDATE TOP LEVEL REGISTERS
      accumulator_reg <= accum_local;
      flags_reg       <= flags_reg_local;
      b_reg           <= reg_b_local;
      c_reg           <= reg_c_local;
      d_reg           <= reg_d_local;
      e_reg           <= reg_e_local;
      h_reg           <= reg_h_local;
      l_reg           <= reg_l_local;

      -------------------------------------------------------------------------
      -- Stack
      -------------------------------------------------------------------------
      stack_data <= stack_pointer_mem(to_integer(stack_pointer+"1")) & stack_pointer_mem(to_integer(stack_pointer));

      -- Pop PC
      if sp_to_pc = '1' then
        stack_pointer <= stack_pointer + "10";
      end if;

      -- Push PC
      if pc_to_sp = '1' then
        stack_pointer <= stack_pointer - "10";
        --
        stack_pointer_mem(to_integer(stack_pointer))     <= pc_in(15 downto 8);
        stack_pointer_mem(to_integer(stack_pointer-"1")) <= pc_in(7 downto 0);
      end if;

      -- Push PC + 3
      -- Used when PC needs to read multiple bytes from program first
      if pc_to_sp_p3 = '1' then
        reg16_results := unsigned('0' & pc_in) + "11";
        stack_pointer <= stack_pointer - "10";
        --
        stack_pointer_mem(to_integer(stack_pointer))     <= std_logic_vector(reg16_results(15 downto 8));
        stack_pointer_mem(to_integer(stack_pointer-"1")) <= std_logic_vector(reg16_results(7 downto 0));
      end if;

      -- Push Register
      if push_reg(0) = '1' then
        stack_pointer <= stack_pointer - "10";
        --
        case push_reg(2 downto 1) is
          when "00" =>
            stack_pointer_mem(to_integer(stack_pointer))     <= reg_b_local;
            stack_pointer_mem(to_integer(stack_pointer-"1")) <= reg_c_local;
          when "01" => 
            stack_pointer_mem(to_integer(stack_pointer))     <= reg_d_local;
            stack_pointer_mem(to_integer(stack_pointer-"1")) <= reg_e_local;
          when "10" => 
            stack_pointer_mem(to_integer(stack_pointer))     <= reg_h_local;
            stack_pointer_mem(to_integer(stack_pointer-"1")) <= reg_l_local;
          when "11" => 
            stack_pointer_mem(to_integer(stack_pointer))     <= accum_local;
            stack_pointer_mem(to_integer(stack_pointer-"1")) <= flags_reg_local;
          when others => null;
        end case;
      end if;

      -- Pop Register
      if pop_reg(0) = '1' then
        stack_pointer <= stack_pointer + "10";
        --
        case pop_reg(2 downto 1) is
          when "00" =>
            reg_b_local <= stack_pointer_mem(to_integer(stack_pointer+"1"));
            reg_c_local <= stack_pointer_mem(to_integer(stack_pointer));
          when "01" => 
            reg_d_local <= stack_pointer_mem(to_integer(stack_pointer+"1"));
            reg_e_local <= stack_pointer_mem(to_integer(stack_pointer));
          when "10" => 
            reg_h_local <= stack_pointer_mem(to_integer(stack_pointer+"1"));
            reg_l_local <= stack_pointer_mem(to_integer(stack_pointer));
          when "11" => 
            accum_local     <= stack_pointer_mem(to_integer(stack_pointer+"1"));
            flags_reg_local <= stack_pointer_mem(to_integer(stack_pointer));
          when others => null;
        end case;
      end if;

      -------------------------------------------------------------------------
      -- Load immediates (16 bit)
      -------------------------------------------------------------------------
      if update_reg(0) = '1' then
        case update_reg(2 downto 1) is
          -- BC
          when "00" =>
            if rd = '0' then
              if toggle = '0' then
                reg_c_local <= data_bus_i;
              else
                reg_b_local <= data_bus_i;
              end if;
              toggle := not(toggle);
            end if;

          -- DE
          when "01" =>
            if rd = '0' then
              if toggle = '0' then
                reg_e_local <= data_bus_i;
              else
                reg_d_local <= data_bus_i;
              end if;
              toggle := not(toggle);
            end if;

          -- HL
          when "10" =>
            if rd = '0' then
              if toggle = '0' then
                reg_l_local <= data_bus_i;
              else
                reg_h_local <= data_bus_i;
              end if;
              toggle := not(toggle);
            end if;

          -- SP
          when "11" =>
            if rd = '0' then
              if toggle = '0' then
                stack_pointer(7 downto 0) <= unsigned(data_bus_i);
              else
                stack_pointer(15 downto 8) <= unsigned(data_bus_i);
              end if;
              toggle := not(toggle);
            end if;
          when others => null;
        end case;
      end if;

      -------------------------------------------------------------------------
      -- Increment 16 bit reg
      -------------------------------------------------------------------------
      if increment_reg(0) = '1' then
        case increment_reg(2 downto 1) is
          -- BC
          when "00"   => reg_bc        <= std_logic_vector(unsigned(reg_bc) + "1");
          -- DE
          when "01"   => reg_de        <= std_logic_vector(unsigned(reg_de) + "1");
          -- HL
          when "10"   => reg_hl        <= std_logic_vector(unsigned(reg_hl) + "1");
          -- SP
          when "11"   => stack_pointer <= stack_pointer + "1";
          when others => null;
        end case;
      end if;

      -------------------------------------------------------------------------
      -- Increment 8 bit reg
      -------------------------------------------------------------------------
      if inc_8b_reg(0) = '1' then
        sub_flag <= '0';
        case inc_8b_reg(3 downto 1) is
          -- B
          when "000" =>
            reg_results        := unsigned('0' & reg_b_local) + "1";
            reg_b_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_b_local(3 downto 0)) + "1";
            half_carry_flag    <= half_carry_results(4);
          -- C
          when "001" =>
            reg_results        := unsigned('0' & reg_c_local) + "1";
            reg_c_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_c_local(3 downto 0)) + "1";
            half_carry_flag    <= half_carry_results(4);
          -- D
          when "010" =>
            reg_results        := unsigned('0' & reg_d_local) + "1";
            reg_d_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_d_local(3 downto 0)) + "1";
            half_carry_flag    <= half_carry_results(4);
          -- E
          when "011" =>
            reg_results        := unsigned('0' & reg_e_local) + "1";
            reg_e_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_e_local(3 downto 0)) + "1";
            half_carry_flag    <= half_carry_results(4);
          -- H
          when "100" =>
            reg_results        := unsigned('0' & reg_h_local) + "1";
            reg_h_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_h_local(3 downto 0)) + "1";
            half_carry_flag    <= half_carry_results(4);
          -- L
          when "110" =>
            reg_results        := unsigned('0' & reg_l_local) + "1";
            reg_l_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_l_local(3 downto 0)) + "1";
            half_carry_flag    <= half_carry_results(4);
          -- A
          when "111" =>
            reg_results        := unsigned('0' & accum_local) + "1";
            accum_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) + "1";
            half_carry_flag    <= half_carry_results(4);
          when others => null;
        end case;
        zero_flag <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      -------------------------------------------------------------------------
      -- Decrement 16 bit registers
      -------------------------------------------------------------------------
      if decrement_reg(0) = '1' then
        case decrement_reg(2 downto 1) is
          -- BC
          when "00"   => reg_bc        <= std_logic_vector(unsigned(reg_bc) - "1");
          -- DE
          when "01"   => reg_de        <= std_logic_vector(unsigned(reg_de) - "1");
          -- HL
          when "10"   => reg_hl        <= std_logic_vector(unsigned(reg_hl) - "1");
          -- SP
          when "11"   => stack_pointer <= stack_pointer - "1";
          when others => null;
        end case;
      end if;

      -------------------------------------------------------------------------
      -- Decrement 8 bit reg
      -------------------------------------------------------------------------
      if dec_8b_reg(0) = '1' then
        sub_flag <= '1';
        case dec_8b_reg(3 downto 1) is
          -- B
          when "000" =>
            reg_results        := unsigned('0' & reg_b_local) - "1";
            reg_b_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_b_local(3 downto 0)) - "1";
            half_carry_flag    <= half_carry_results(4);
          -- C
          when "001" =>
            reg_results        := unsigned('0' & reg_c_local) - "1";
            reg_c_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_c_local(3 downto 0)) - "1";
            half_carry_flag    <= half_carry_results(4);
          -- D
          when "010" =>
            reg_results        := unsigned('0' & reg_d_local) - "1";
            reg_d_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_d_local(3 downto 0)) - "1";
            half_carry_flag    <= half_carry_results(4);
          -- E
          when "011" =>
            reg_results        := unsigned('0' & reg_e_local) - "1";
            reg_e_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_e_local(3 downto 0)) - "1";
            half_carry_flag    <= half_carry_results(4);
          -- H
          when "100" =>
            reg_results        := unsigned('0' & reg_h_local) - "1";
            reg_h_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_h_local(3 downto 0)) - "1";
            half_carry_flag    <= half_carry_results(4);
          -- L
          when "110" =>
            reg_results        := unsigned('0' & reg_l_local) - "1";
            reg_l_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & reg_l_local(3 downto 0)) - "1";
            half_carry_flag    <= half_carry_results(4);
          -- A
          when "111" =>
            reg_results        := unsigned('0' & accum_local) - "1";
            accum_local        <= std_logic_vector(reg_results(7 downto 0));
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) - "1";
            half_carry_flag    <= half_carry_results(4);
          when others => null;
        end case;
        zero_flag <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      -------------------------------------------------------------------------
      -- Load Registers
      -------------------------------------------------------------------------
      if load_regs(0) = '1' then
        case load_regs(3 downto 1) is
          when "000" =>                 -- Load B
            case load_regs(6 downto 4) is
              when "000"  => null;      -- Load b into b does nothing
              when "001"  => reg_b_local <= reg_c_local;
              when "010"  => reg_b_local <= reg_d_local;
              when "011"  => reg_b_local <= reg_e_local;
              when "100"  => reg_b_local <= reg_h_local;
              when "101"  => reg_b_local <= reg_l_local;
              when "111"  => reg_b_local <= accum_local;
              when others => null;
            end case;
          when "001" =>                 -- Load C
            case load_regs(6 downto 4) is
              when "000"  => reg_c_local <= reg_b_local;
              when "001"  => null;      -- Load c to c
              when "010"  => reg_c_local <= reg_d_local;
              when "011"  => reg_c_local <= reg_e_local;
              when "100"  => reg_c_local <= reg_h_local;
              when "101"  => reg_c_local <= reg_l_local;
              when "111"  => reg_c_local <= accum_local;
              when others => null;
            end case;
          when "010" =>                 -- Load D
            case load_regs(6 downto 4) is
              when "000"  => reg_d_local <= reg_b_local;
              when "001"  => reg_d_local <= reg_c_local;
              when "010"  => null;
              when "011"  => reg_d_local <= reg_e_local;
              when "100"  => reg_d_local <= reg_h_local;
              when "101"  => reg_d_local <= reg_l_local;
              when "111"  => reg_d_local <= accum_local;
              when others => null;
            end case;
          when "011" =>                 -- Load E
            case load_regs(6 downto 4) is
              when "000"  => reg_e_local <= reg_b_local;
              when "001"  => reg_e_local <= reg_c_local;
              when "010"  => reg_e_local <= reg_d_local;
              when "011"  => null;
              when "100"  => reg_e_local <= reg_h_local;
              when "101"  => reg_e_local <= reg_l_local;
              when "111"  => reg_e_local <= accum_local;
              when others => null;
            end case;
          when "100" =>                 -- Load H
            case load_regs(6 downto 4) is
              when "000"  => reg_h_local <= reg_b_local;
              when "001"  => reg_h_local <= reg_c_local;
              when "010"  => reg_h_local <= reg_d_local;
              when "011"  => reg_h_local <= reg_e_local;
              when "100"  => null;
              when "101"  => reg_h_local <= reg_l_local;
              when "111"  => reg_h_local <= accum_local;
              when others => null;
            end case;
          when "101" =>                 -- Load L
            case load_regs(6 downto 4) is
              when "000"  => reg_l_local <= reg_b_local;
              when "001"  => reg_l_local <= reg_c_local;
              when "010"  => reg_l_local <= reg_d_local;
              when "011"  => reg_l_local <= reg_e_local;
              when "100"  => reg_l_local <= reg_h_local;
              when "101"  => null;
              when "111"  => reg_l_local <= accum_local;
              when others => null;
            end case;
          when "111" =>
            case load_regs(6 downto 4) is
              when "000"  => accum_local <= reg_b_local;
              when "001"  => accum_local <= reg_c_local;
              when "010"  => accum_local <= reg_d_local;
              when "011"  => accum_local <= reg_e_local;
              when "100"  => accum_local <= reg_h_local;
              when "101"  => accum_local <= reg_l_local;
              when "111"  => null;
              when others => null;
            end case;
          when others => null;
        end case;
      end if;

      -------------------------------------------------------------------------
      -- Load 8 bit immediates
      -------------------------------------------------------------------------
      if db_to_regs(0) = '1' then
        if rd = '0' then
          case db_to_regs(3 downto 1) is
            when "000"  => reg_b_local <= data_bus_i;
            when "001"  => reg_c_local <= data_bus_i;
            when "010"  => reg_d_local <= data_bus_i;
            when "011"  => reg_e_local <= data_bus_i;
            when "100"  => reg_h_local <= data_bus_i;
            when "101"  => reg_l_local <= data_bus_i;
            when "111"  => accum_local <= data_bus_i;
            when others => null;
          end case;
        end if;
      end if;

      -------------------------------------------------------------------------
      -- Register to data bus (8 bit)
      -------------------------------------------------------------------------
      if reg_to_db(0) = '1' then
        case reg_to_db(3 downto 1) is
          when "000"  => data_bus_o <= reg_b_local;
          when "001"  => data_bus_o <= reg_c_local;
          when "010"  => data_bus_o <= reg_d_local;
          when "011"  => data_bus_o <= reg_e_local;
          when "100"  => data_bus_o <= reg_h_local;
          when "101"  => data_bus_o <= reg_l_local;
          when "111"  => data_bus_o <= accum_local;
          when others => null;
        end case;
      end if;

      -------------------------------------------------------------------------
      -- Load SP with HL
      -------------------------------------------------------------------------
      if hl_ld_sp = '1' then
        stack_pointer <= unsigned(reg_hl);
      end if;

      -------------------------------------------------------------------------
      -- Add registers
      -------------------------------------------------------------------------
      if add_reg(0) = '1' then
        sub_flag <= '0';
        case add_reg(3 downto 1) is
          when "000" =>
            reg_results        := unsigned('0' & accum_local) + unsigned('0' & reg_b_local) + (carry_flag and use_cf);
            accum_local        <= std_logic_vector(reg_results(7 downto 0));
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) + unsigned('0' & reg_b_local(3 downto 0)) + (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "001" =>
            reg_results        := unsigned('0' & accum_local) + unsigned('0' & reg_c_local) + (carry_flag and use_cf);
            accum_local        <= std_logic_vector(reg_results(7 downto 0));
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) + unsigned('0' & reg_c_local(3 downto 0)) + (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "010" =>
            reg_results        := unsigned('0' & accum_local) + unsigned('0' & reg_d_local) + (carry_flag and use_cf);
            accum_local        <= std_logic_vector(reg_results(7 downto 0));
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) + unsigned('0' & reg_d_local(3 downto 0)) + (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "011" =>
            reg_results        := unsigned('0' & accum_local) + unsigned('0' & reg_e_local) + (carry_flag and use_cf);
            accum_local        <= std_logic_vector(reg_results(7 downto 0));
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) + unsigned('0' & reg_e_local(3 downto 0)) + (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "100" =>
            reg_results        := unsigned('0' & accum_local) + unsigned('0' & reg_h_local) + (carry_flag and use_cf);
            accum_local        <= std_logic_vector(reg_results(7 downto 0));
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) + unsigned('0' & reg_h_local(3 downto 0)) + (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "101" =>
            reg_results        := unsigned('0' & accum_local) + unsigned('0' & reg_l_local) + (carry_flag and use_cf);
            accum_local        <= std_logic_vector(reg_results(7 downto 0));
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) + unsigned('0' & reg_l_local(3 downto 0)) + (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "111" =>
            accum_local        <= std_logic_vector(unsigned(accum_local(6 downto 0) & '0') + (carry_flag and use_cf));
            carry_flag         <= accum_local(7);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) + unsigned('0' & accum_local(3 downto 0)) + (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when others => null;
        end case;
        zero_flag <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      if add_db = '1' then
        sub_flag           <= '0';
        reg_results        := unsigned('0' & accum_local) + unsigned('0' & data_bus_i) + (carry_flag and use_cf);
        accum_local        <= std_logic_vector(reg_results(7 downto 0));
        carry_flag         <= reg_results(8);
        half_carry_results := unsigned('0' & accum_local(3 downto 0)) + unsigned('0' & data_bus_i(3 downto 0)) + (carry_flag & use_cf);
        zero_flag          <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      if add_hl(0) = '1' then
        sub_flag <= '0';
        case add_hl(2 downto 1) is
          when "00" =>
            reg16_results        := unsigned('0' & reg_hl) + unsigned('0' & reg_bc);
            reg_hl               <= std_logic_vector(reg16_results(15 downto 0));
            carry_flag           <= reg16_results(16);
            half_carry16_results := unsigned('0' & reg_hl(11 downto 0)) + unsigned('0' & reg_bc(11 downto 0));
            half_carry_flag      <= half_carry16_results(12);
          when "01" =>
            reg16_results        := unsigned('0' & reg_hl) + unsigned('0' & reg_de);
            reg_hl               <= std_logic_vector(reg16_results(15 downto 0));
            carry_flag           <= reg16_results(16);
            half_carry16_results := unsigned('0' & reg_hl(11 downto 0)) + unsigned('0' & reg_de(11 downto 0));
            half_carry_flag      <= half_carry16_results(12);
          when "10" =>
            reg16_results        := unsigned('0' & reg_hl) + unsigned('0' & reg_hl);
            reg_hl               <= std_logic_vector(reg16_results(15 downto 0));
            carry_flag           <= reg16_results(16);
            half_carry16_results := unsigned('0' & reg_hl(11 downto 0)) + unsigned('0' & reg_hl(11 downto 0));
            half_carry_flag      <= half_carry16_results(12);
          when "11" =>
            reg16_results        := unsigned('0' & reg_hl) + '0' & stack_pointer;
            reg_hl               <= std_logic_vector(reg16_results(15 downto 0));
            carry_flag           <= reg16_results(16);
            half_carry16_results := unsigned('0' & reg_hl(11 downto 0)) + '0' & stack_pointer(11 downto 0);
            half_carry_flag      <= half_carry16_results(12);
          when others => null;
        end case;
      end if;

      if sp_add = '1' then
        reg16_results := unsigned(signed('0' & stack_pointer) + signed(data_bus_i));
        stack_pointer <= reg16_results(15 downto 0);
      end if;

      if hl_pl_sp = '1' then
        reg16_results := unsigned(signed('0' & stack_pointer) + signed(data_bus_i));
        reg_hl        <= std_logic_vector(reg16_results(15 downto 0));
      end if;

      -------------------------------------------------------------------------
      -- Subtraction
      -------------------------------------------------------------------------
      if sub_reg(0) = '1' then
        sub_flag <= '1';
        case sub_reg(3 downto 1) is
          when "000" =>
            reg_results := unsigned('0' & accum_local) - unsigned('0' & reg_b_local) - (carry_flag and use_cf);
            if flag_only = '0' then
              accum_local <= std_logic_vector(reg_results(7 downto 0));
            end if;
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) - unsigned('0' & reg_b_local(3 downto 0)) - (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "001" =>
            reg_results := unsigned('0' & accum_local) - unsigned('0' & reg_c_local) - (carry_flag and use_cf);
            if flag_only = '0' then
              accum_local <= std_logic_vector(reg_results(7 downto 0));
            end if;
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) - unsigned('0' & reg_c_local(3 downto 0)) - (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "010" =>
            reg_results := unsigned('0' & accum_local) - unsigned('0' & reg_d_local) - (carry_flag and use_cf);
            if flag_only = '0' then
              accum_local <= std_logic_vector(reg_results(7 downto 0));
            end if;
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) - unsigned('0' & reg_d_local(3 downto 0)) - (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "011" =>
            reg_results := unsigned('0' & accum_local) - unsigned('0' & reg_e_local) - (carry_flag and use_cf);
            if flag_only = '0' then
              accum_local <= std_logic_vector(reg_results(7 downto 0));
            end if;
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) - unsigned('0' & reg_e_local(3 downto 0)) - (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "100" =>
            reg_results := unsigned('0' & accum_local) - unsigned('0' & reg_h_local) - (carry_flag and use_cf);
            if flag_only = '0' then
              accum_local <= std_logic_vector(reg_results(7 downto 0));
            end if;
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) - unsigned('0' & reg_h_local(3 downto 0)) - (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "101" =>
            reg_results := unsigned('0' & accum_local) - unsigned('0' & reg_l_local) - (carry_flag and use_cf);
            if flag_only = '0' then
              accum_local <= std_logic_vector(reg_results(7 downto 0));
            end if;
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) - unsigned('0' & reg_l_local(3 downto 0)) - (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when "111" =>
            reg_results := unsigned('0' & accum_local) - unsigned('0' & accum_local) - (carry_flag and use_cf);
            if flag_only = '0' then
              accum_local <= std_logic_vector(reg_results(7 downto 0));
            end if;
            carry_flag         <= reg_results(8);
            half_carry_results := unsigned('0' & accum_local(3 downto 0)) - unsigned('0' & accum_local(3 downto 0)) - (carry_flag and use_cf);
            half_carry_flag    <= half_carry_results(4);
          when others => null;
        end case;
        zero_flag <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      if sub_db = '1' then
        sub_flag    <= '1';
        reg_results := unsigned('0' & accum_local) - unsigned('0' & data_bus_i) - (carry_flag and use_cf);
        if flag_only = '0' then
          accum_local <= std_logic_vector(reg_results(7 downto 0));
        end if;
        carry_flag         <= reg_results(8);
        half_carry_results := unsigned('0' & accum_local(3 downto 0)) - unsigned('0' & data_bus_i(3 downto 0)) - (carry_flag & use_cf);
        zero_flag          <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      -------------------------------------------------------------------------
      -- AND logic
      -------------------------------------------------------------------------
      if and_reg(0) = '1' then
        sub_flag        <= '0';
        half_carry_flag <= '1';
        carry_flag      <= '0';
        case and_reg(3 downto 1) is
          when "000" =>
            reg_results := unsigned('0' & (accum_local and reg_b_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "001" =>
            reg_results := unsigned('0' & (accum_local and reg_c_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "010" =>
            reg_results := unsigned('0' & (accum_local and reg_d_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "011" =>
            reg_results := unsigned('0' & (accum_local and reg_e_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "100" =>
            reg_results := unsigned('0' & (accum_local and reg_h_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "101" =>
            reg_results := unsigned('0' & (accum_local and reg_l_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "111" =>
            reg_results := unsigned('0' & (accum_local and accum_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when others => null;
        end case;
        zero_flag <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      if and_db = '1' then
        sub_flag        <= '0';
        half_carry_flag <= '1';
        carry_flag      <= '0';
        reg_results     := unsigned('0' & (accum_local and data_bus_i));
        accum_local     <= std_logic_vector(reg_results(7 downto 0));
        zero_flag       <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      -------------------------------------------------------------------------
      -- OR logic
      -------------------------------------------------------------------------
      if or_reg(0) = '1' then
        sub_flag        <= '0';
        half_carry_flag <= '0';
        carry_flag      <= '0';
        case or_reg(3 downto 1) is
          when "000" =>
            reg_results := unsigned('0' & (accum_local or reg_b_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "001" =>
            reg_results := unsigned('0' & (accum_local or reg_c_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "010" =>
            reg_results := unsigned('0' & (accum_local or reg_d_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "011" =>
            reg_results := unsigned('0' & (accum_local or reg_e_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "100" =>
            reg_results := unsigned('0' & (accum_local or reg_h_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "101" =>
            reg_results := unsigned('0' & (accum_local or reg_l_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "111" =>
            reg_results := unsigned('0' & (accum_local or accum_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when others => null;
        end case;
        zero_flag <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      if or_db = '1' then
        sub_flag        <= '0';
        half_carry_flag <= '1';
        carry_flag      <= '0';
        reg_results     := unsigned('0' & (accum_local or data_bus_i));
        accum_local     <= std_logic_vector(reg_results(7 downto 0));
        zero_flag       <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      -------------------------------------------------------------------------
      -- XOR logic
      -------------------------------------------------------------------------
      if xor_reg(0) = '1' then
        sub_flag        <= '0';
        half_carry_flag <= '0';
        carry_flag      <= '0';
        case xor_reg(3 downto 1) is
          when "000" =>
            reg_results := unsigned('0' & (accum_local xor reg_b_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "001" =>
            reg_results := unsigned('0' & (accum_local xor reg_c_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "010" =>
            reg_results := unsigned('0' & (accum_local xor reg_d_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "011" =>
            reg_results := unsigned('0' & (accum_local xor reg_e_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "100" =>
            reg_results := unsigned('0' & (accum_local xor reg_h_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "101" =>
            reg_results := unsigned('0' & (accum_local xor reg_l_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when "111" =>
            reg_results := unsigned('0' & (accum_local xor accum_local));
            accum_local <= std_logic_vector(reg_results(7 downto 0));
          when others => null;
        end case;
        zero_flag <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      if xor_db = '1' then
        sub_flag        <= '0';
        half_carry_flag <= '1';
        carry_flag      <= '0';
        reg_results     := unsigned('0' & (accum_local xor data_bus_i));
        accum_local     <= std_logic_vector(reg_results(7 downto 0));
        zero_flag       <= '1' when reg_results(7 downto 0) = x"00" else '0';
      end if;

      -------------------------------------------------------------------------
      -- Nots and sets
      -------------------------------------------------------------------------
      if not_acc = '1' then
        sub_flag        <= '1';
        half_carry_flag <= '1';
        accum_local     <= not(accum_local);
      end if;

      if not_cf = '1' then
        sub_flag        <= '0';
        half_carry_flag <= '0';
        carry_flag      <= not(carry_flag);
      end if;

      if set_cf = '1' then
        sub_flag        <= '0';
        half_carry_flag <= '0';
        carry_flag      <= '1';
      end if;

      -------------------------------------------------------------------------
      -- Rotates
      -------------------------------------------------------------------------
      if rotate_l_a = '1' then
        if thru_cf = '1' then
          carry_flag  <= accum_local(7);
          accum_local <= accum_local(6 downto 0) & carry_flag;
        else
          carry_flag  <= accum_local(7);
          accum_local <= accum_local(6 downto 0) & accum_local(7);
        end if;
      end if;

      if rotate_r_a = '1' then
        if thru_cf = '1' then
          carry_flag  <= accum_local(0);
          accum_local <= carry_flag & accum_local(7 downto 1);
        else
          carry_flag  <= accum_local(0);
          accum_local <= accum_local(0) & accum_local(7 downto 1);
        end if;
      end if;

      -------------------------------------------------------------------------
      -- Bit commands
      -------------------------------------------------------------------------
      if use_cmd = '1' then
        case bit_reg is
          when "000" =>                 -- B
            case bit_cmd is
              when "00000" =>           -- RLC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_b_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_b_local(7);
                reg_b_local <= reg_b_local(6 downto 0) & reg_b_local(7);
              when "00010" =>           -- RL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_b_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_b_local(7);
                reg_b_local <= reg_b_local(6 downto 0) & carry_flag;
              when "00001" =>           -- RRC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_b_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_b_local(0);
                reg_b_local <= reg_b_local(0) & reg_b_local(7 downto 1);
              when "00011" =>           -- RR
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_b_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_b_local(0);
                reg_b_local <= carry_flag & reg_b_local(7 downto 1);
              when "00100" =>           -- SLA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_b_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_b_local(7);
                reg_b_local <= reg_b_local(6 downto 0) & '0';
              when "00101" =>           -- SRA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_b_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_b_local(0);
                reg_b_local <= reg_b_local(7) & reg_b_local(7 downto 1);
              when "00110" =>           -- SWAP
                half_carry_flag <= '0';
                sub_flag        <= '0';
                carry_flag      <= '0';
                reg_b_local     <= reg_b_local(3 downto 0) & reg_b_local(7 downto 4);
                if reg_b_local = x"00" then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
              when "00111" =>           -- SRL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_b_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_b_local(0);
                reg_b_local <= '0' & reg_b_local(7 downto 1);
              when "01000" =>           -- BIT 0
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_b_local(0));
              when "01001" =>           -- BIT 1
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_b_local(1));
              when "01010" =>           -- BIT 2
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_b_local(2));
              when "01011" =>           -- BIT 3
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_b_local(3));
              when "01100" =>           -- BIT 4
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_b_local(4));
              when "01101" =>           -- BIT 5
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_b_local(5));
              when "01110" =>           -- BIT 6
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_b_local(6));
              when "01111" =>           -- BIT 7
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_b_local(7));
              when "10000" =>           -- RES 0
                reg_b_local(0) <= '0';
              when "10001" =>           -- RES 1
                reg_b_local(1) <= '0';
              when "10010" =>           -- RES 2
                reg_b_local(2) <= '0';
              when "10011" =>           -- RES 3
                reg_b_local(3) <= '0';
              when "10100" =>           -- RES 4
                reg_b_local(4) <= '0';
              when "10101" =>           -- RES 5
                reg_b_local(5) <= '0';
              when "10110" =>           -- RES 6
                reg_b_local(6) <= '0';
              when "10111" =>           -- RES 7
                reg_b_local(7) <= '0';
              when "11000" =>           -- SET 0
                reg_b_local(0) <= '1';
              when "11001" =>           -- SET 1
                reg_b_local(1) <= '1';
              when "11010" =>           -- SET 2
                reg_b_local(2) <= '1';
              when "11011" =>           -- SET 3
                reg_b_local(3) <= '1';
              when "11100" =>           -- SET 4
                reg_b_local(4) <= '1';
              when "11101" =>           -- SET 5
                reg_b_local(5) <= '1';
              when "11110" =>           -- SET 6
                reg_b_local(6) <= '1';
              when "11111" =>           -- SET 7
                reg_b_local(7) <= '1';
              when others => null;
            end case;
          when "001" =>                 -- C
            case bit_cmd is
              when "00000" =>           -- RLC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_c_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_c_local(7);
                reg_c_local <= reg_c_local(6 downto 0) & reg_c_local(7);
              when "00010" =>           -- RL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_c_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_c_local(7);
                reg_c_local <= reg_c_local(6 downto 0) & carry_flag;
              when "00001" =>           -- RRC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_c_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_c_local(0);
                reg_c_local <= reg_c_local(0) & reg_c_local(7 downto 1);
              when "00011" =>           -- RR
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_c_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_c_local(0);
                reg_c_local <= carry_flag & reg_c_local(7 downto 1);
              when "00100" =>           -- SLA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_c_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_c_local(7);
                reg_c_local <= reg_c_local(6 downto 0) & '0';
              when "00101" =>           -- SRA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_c_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_c_local(0);
                reg_c_local <= reg_c_local(7) & reg_c_local(7 downto 1);
              when "00110" =>           -- SWAP
                half_carry_flag <= '0';
                sub_flag        <= '0';
                carry_flag      <= '0';
                reg_c_local     <= reg_c_local(3 downto 0) & reg_c_local(7 downto 4);
                if reg_c_local = x"00" then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
              when "00111" =>           -- SRL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_c_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_c_local(0);
                reg_c_local <= '0' & reg_c_local(7 downto 1);
              when "01000" =>           -- BIT 0
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_c_local(0));
              when "01001" =>           -- BIT 1
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_c_local(1));
              when "01010" =>           -- BIT 2
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_c_local(2));
              when "01011" =>           -- BIT 3
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_c_local(3));
              when "01100" =>           -- BIT 4
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_c_local(4));
              when "01101" =>           -- BIT 5
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_c_local(5));
              when "01110" =>           -- BIT 6
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_c_local(6));
              when "01111" =>           -- BIT 7
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_c_local(7));
              when "10000" =>           -- RES 0
                reg_c_local(0) <= '0';
              when "10001" =>           -- RES 1
                reg_c_local(1) <= '0';
              when "10010" =>           -- RES 2
                reg_c_local(2) <= '0';
              when "10011" =>           -- RES 3
                reg_c_local(3) <= '0';
              when "10100" =>           -- RES 4
                reg_c_local(4) <= '0';
              when "10101" =>           -- RES 5
                reg_c_local(5) <= '0';
              when "10110" =>           -- RES 6
                reg_c_local(6) <= '0';
              when "10111" =>           -- RES 7
                reg_c_local(7) <= '0';
              when "11000" =>           -- SET 0
                reg_c_local(0) <= '1';
              when "11001" =>           -- SET 1
                reg_c_local(1) <= '1';
              when "11010" =>           -- SET 2
                reg_c_local(2) <= '1';
              when "11011" =>           -- SET 3
                reg_c_local(3) <= '1';
              when "11100" =>           -- SET 4
                reg_c_local(4) <= '1';
              when "11101" =>           -- SET 5
                reg_c_local(5) <= '1';
              when "11110" =>           -- SET 6
                reg_c_local(6) <= '1';
              when "11111" =>           -- SET 7
                reg_c_local(7) <= '1';
              when others => null;
            end case;
          when "010" =>                 -- D
            case bit_cmd is
              when "00000" =>           -- RLC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_d_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_d_local(7);
                reg_d_local <= reg_d_local(6 downto 0) & reg_d_local(7);
              when "00010" =>           -- RL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_d_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_d_local(7);
                reg_d_local <= reg_d_local(6 downto 0) & carry_flag;
              when "00001" =>           -- RRC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_d_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_d_local(0);
                reg_d_local <= reg_d_local(0) & reg_d_local(7 downto 1);
              when "00011" =>           -- RR
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_d_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_d_local(0);
                reg_d_local <= carry_flag & reg_d_local(7 downto 1);
              when "00100" =>           -- SLA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_d_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_d_local(7);
                reg_d_local <= reg_d_local(6 downto 0) & '0';
              when "00101" =>           -- SRA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_d_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_d_local(0);
                reg_d_local <= reg_d_local(7) & reg_d_local(7 downto 1);
              when "00110" =>           -- SWAP
                half_carry_flag <= '0';
                sub_flag        <= '0';
                carry_flag      <= '0';
                reg_d_local     <= reg_d_local(3 downto 0) & reg_d_local(7 downto 4);
                if reg_d_local = x"00" then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
              when "00111" =>           -- SRL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_d_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_d_local(0);
                reg_d_local <= '0' & reg_d_local(7 downto 1);
              when "01000" =>           -- BIT 0
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_d_local(0));
              when "01001" =>           -- BIT 1
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_d_local(1));
              when "01010" =>           -- BIT 2
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_d_local(2));
              when "01011" =>           -- BIT 3
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_d_local(3));
              when "01100" =>           -- BIT 4
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_d_local(4));
              when "01101" =>           -- BIT 5
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_d_local(5));
              when "01110" =>           -- BIT 6
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_d_local(6));
              when "01111" =>           -- BIT 7
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_d_local(7));
              when "10000" =>           -- RES 0
                reg_d_local(0) <= '0';
              when "10001" =>           -- RES 1
                reg_d_local(1) <= '0';
              when "10010" =>           -- RES 2
                reg_d_local(2) <= '0';
              when "10011" =>           -- RES 3
                reg_d_local(3) <= '0';
              when "10100" =>           -- RES 4
                reg_d_local(4) <= '0';
              when "10101" =>           -- RES 5
                reg_d_local(5) <= '0';
              when "10110" =>           -- RES 6
                reg_d_local(6) <= '0';
              when "10111" =>           -- RES 7
                reg_d_local(7) <= '0';
              when "11000" =>           -- SET 0
                reg_d_local(0) <= '1';
              when "11001" =>           -- SET 1
                reg_d_local(1) <= '1';
              when "11010" =>           -- SET 2
                reg_d_local(2) <= '1';
              when "11011" =>           -- SET 3
                reg_d_local(3) <= '1';
              when "11100" =>           -- SET 4
                reg_d_local(4) <= '1';
              when "11101" =>           -- SET 5
                reg_d_local(5) <= '1';
              when "11110" =>           -- SET 6
                reg_d_local(6) <= '1';
              when "11111" =>           -- SET 7
                reg_d_local(7) <= '1';
              when others => null;
            end case;
          when "011" =>                 -- E
            case bit_cmd is
              when "00000" =>           -- RLC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_e_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_e_local(7);
                reg_e_local <= reg_e_local(6 downto 0) & reg_e_local(7);
              when "00010" =>           -- RL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_e_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_e_local(7);
                reg_e_local <= reg_e_local(6 downto 0) & carry_flag;
              when "00001" =>           -- RRC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_e_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_e_local(0);
                reg_e_local <= reg_e_local(0) & reg_e_local(7 downto 1);
              when "00011" =>           -- RR
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_e_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_e_local(0);
                reg_e_local <= carry_flag & reg_e_local(7 downto 1);
              when "00100" =>           -- SLA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_e_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_e_local(7);
                reg_e_local <= reg_e_local(6 downto 0) & '0';
              when "00101" =>           -- SRA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_e_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_e_local(0);
                reg_e_local <= reg_e_local(7) & reg_e_local(7 downto 1);
              when "00110" =>           -- SWAP
                half_carry_flag <= '0';
                sub_flag        <= '0';
                carry_flag      <= '0';
                reg_e_local     <= reg_e_local(3 downto 0) & reg_e_local(7 downto 4);
                if reg_e_local = x"00" then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
              when "00111" =>           -- SRL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_e_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_e_local(0);
                reg_e_local <= '0' & reg_e_local(7 downto 1);
              when "01000" =>           -- BIT 0
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_e_local(0));
              when "01001" =>           -- BIT 1
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_e_local(1));
              when "01010" =>           -- BIT 2
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_e_local(2));
              when "01011" =>           -- BIT 3
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_e_local(3));
              when "01100" =>           -- BIT 4
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_e_local(4));
              when "01101" =>           -- BIT 5
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_e_local(5));
              when "01110" =>           -- BIT 6
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_e_local(6));
              when "01111" =>           -- BIT 7
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_e_local(7));
              when "10000" =>           -- RES 0
                reg_e_local(0) <= '0';
              when "10001" =>           -- RES 1
                reg_e_local(1) <= '0';
              when "10010" =>           -- RES 2
                reg_e_local(2) <= '0';
              when "10011" =>           -- RES 3
                reg_e_local(3) <= '0';
              when "10100" =>           -- RES 4
                reg_e_local(4) <= '0';
              when "10101" =>           -- RES 5
                reg_e_local(5) <= '0';
              when "10110" =>           -- RES 6
                reg_e_local(6) <= '0';
              when "10111" =>           -- RES 7
                reg_e_local(7) <= '0';
              when "11000" =>           -- SET 0
                reg_e_local(0) <= '1';
              when "11001" =>           -- SET 1
                reg_e_local(1) <= '1';
              when "11010" =>           -- SET 2
                reg_e_local(2) <= '1';
              when "11011" =>           -- SET 3
                reg_e_local(3) <= '1';
              when "11100" =>           -- SET 4
                reg_e_local(4) <= '1';
              when "11101" =>           -- SET 5
                reg_e_local(5) <= '1';
              when "11110" =>           -- SET 6
                reg_e_local(6) <= '1';
              when "11111" =>           -- SET 7
                reg_e_local(7) <= '1';
              when others => null;
            end case;
          when "100" =>                 -- H
            case bit_cmd is
              when "00000" =>           -- RLC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_h_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_h_local(7);
                reg_h_local <= reg_h_local(6 downto 0) & reg_h_local(7);
              when "00010" =>           -- RL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_h_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_h_local(7);
                reg_h_local <= reg_h_local(6 downto 0) & carry_flag;
              when "00001" =>           -- RRC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_h_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_h_local(0);
                reg_h_local <= reg_h_local(0) & reg_h_local(7 downto 1);
              when "00011" =>           -- RR
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_h_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_h_local(0);
                reg_h_local <= carry_flag & reg_h_local(7 downto 1);
              when "00100" =>           -- SLA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_h_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_h_local(7);
                reg_h_local <= reg_h_local(6 downto 0) & '0';
              when "00101" =>           -- SRA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_h_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_h_local(0);
                reg_h_local <= reg_h_local(7) & reg_h_local(7 downto 1);
              when "00110" =>           -- SWAP
                half_carry_flag <= '0';
                sub_flag        <= '0';
                carry_flag      <= '0';
                reg_h_local     <= reg_h_local(3 downto 0) & reg_h_local(7 downto 4);
                if reg_h_local = x"00" then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
              when "00111" =>           -- SRL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_h_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_h_local(0);
                reg_h_local <= '0' & reg_h_local(7 downto 1);
              when "01000" =>           -- BIT 0
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_h_local(0));
              when "01001" =>           -- BIT 1
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_h_local(1));
              when "01010" =>           -- BIT 2
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_h_local(2));
              when "01011" =>           -- BIT 3
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_h_local(3));
              when "01100" =>           -- BIT 4
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_h_local(4));
              when "01101" =>           -- BIT 5
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_h_local(5));
              when "01110" =>           -- BIT 6
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_h_local(6));
              when "01111" =>           -- BIT 7
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_h_local(7));
              when "10000" =>           -- RES 0
                reg_h_local(0) <= '0';
              when "10001" =>           -- RES 1
                reg_h_local(1) <= '0';
              when "10010" =>           -- RES 2
                reg_h_local(2) <= '0';
              when "10011" =>           -- RES 3
                reg_h_local(3) <= '0';
              when "10100" =>           -- RES 4
                reg_h_local(4) <= '0';
              when "10101" =>           -- RES 5
                reg_h_local(5) <= '0';
              when "10110" =>           -- RES 6
                reg_h_local(6) <= '0';
              when "10111" =>           -- RES 7
                reg_h_local(7) <= '0';
              when "11000" =>           -- SET 0
                reg_h_local(0) <= '1';
              when "11001" =>           -- SET 1
                reg_h_local(1) <= '1';
              when "11010" =>           -- SET 2
                reg_h_local(2) <= '1';
              when "11011" =>           -- SET 3
                reg_h_local(3) <= '1';
              when "11100" =>           -- SET 4
                reg_h_local(4) <= '1';
              when "11101" =>           -- SET 5
                reg_h_local(5) <= '1';
              when "11110" =>           -- SET 6
                reg_h_local(6) <= '1';
              when "11111" =>           -- SET 7
                reg_h_local(7) <= '1';
              when others => null;
            end case;
          when "101" =>                 -- L
            case bit_cmd is
              when "00000" =>           -- RLC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_l_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_l_local(7);
                reg_l_local <= reg_l_local(6 downto 0) & reg_l_local(7);
              when "00010" =>           -- RL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_l_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_l_local(7);
                reg_l_local <= reg_l_local(6 downto 0) & carry_flag;
              when "00001" =>           -- RRC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_l_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_l_local(0);
                reg_l_local <= reg_l_local(0) & reg_l_local(7 downto 1);
              when "00011" =>           -- RR
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_l_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_l_local(0);
                reg_l_local <= carry_flag & reg_l_local(7 downto 1);
              when "00100" =>           -- SLA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_l_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_l_local(7);
                reg_l_local <= reg_l_local(6 downto 0) & '0';
              when "00101" =>           -- SRA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_l_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_l_local(0);
                reg_l_local <= reg_l_local(7) & reg_l_local(7 downto 1);
              when "00110" =>           -- SWAP
                half_carry_flag <= '0';
                sub_flag        <= '0';
                carry_flag      <= '0';
                reg_l_local     <= reg_l_local(3 downto 0) & reg_l_local(7 downto 4);
                if reg_l_local = x"00" then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
              when "00111" =>           -- SRL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if reg_l_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= reg_l_local(0);
                reg_l_local <= '0' & reg_l_local(7 downto 1);
              when "01000" =>           -- BIT 0
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_l_local(0));
              when "01001" =>           -- BIT 1
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_l_local(1));
              when "01010" =>           -- BIT 2
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_l_local(2));
              when "01011" =>           -- BIT 3
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_l_local(3));
              when "01100" =>           -- BIT 4
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_l_local(4));
              when "01101" =>           -- BIT 5
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_l_local(5));
              when "01110" =>           -- BIT 6
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_l_local(6));
              when "01111" =>           -- BIT 7
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(reg_l_local(7));
              when "10000" =>           -- RES 0
                reg_l_local(0) <= '0';
              when "10001" =>           -- RES 1
                reg_l_local(1) <= '0';
              when "10010" =>           -- RES 2
                reg_l_local(2) <= '0';
              when "10011" =>           -- RES 3
                reg_l_local(3) <= '0';
              when "10100" =>           -- RES 4
                reg_l_local(4) <= '0';
              when "10101" =>           -- RES 5
                reg_l_local(5) <= '0';
              when "10110" =>           -- RES 6
                reg_l_local(6) <= '0';
              when "10111" =>           -- RES 7
                reg_l_local(7) <= '0';
              when "11000" =>           -- SET 0
                reg_l_local(0) <= '1';
              when "11001" =>           -- SET 1
                reg_l_local(1) <= '1';
              when "11010" =>           -- SET 2
                reg_l_local(2) <= '1';
              when "11011" =>           -- SET 3
                reg_l_local(3) <= '1';
              when "11100" =>           -- SET 4
                reg_l_local(4) <= '1';
              when "11101" =>           -- SET 5
                reg_l_local(5) <= '1';
              when "11110" =>           -- SET 6
                reg_l_local(6) <= '1';
              when "11111" =>           -- SET 7
                reg_l_local(7) <= '1';
              when others => null;
            end case;
          when "111" =>                 -- A
            case bit_cmd is
              when "00000" =>           -- RLC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if accum_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= accum_local(7);
                accum_local <= accum_local(6 downto 0) & accum_local(7);
              when "00010" =>           -- RL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if accum_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= accum_local(7);
                accum_local <= accum_local(6 downto 0) & carry_flag;
              when "00001" =>           -- RRC
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if accum_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= accum_local(0);
                accum_local <= accum_local(0) & accum_local(7 downto 1);
              when "00011" =>           -- RR
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if accum_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= accum_local(0);
                accum_local <= carry_flag & accum_local(7 downto 1);
              when "00100" =>           -- SLA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if accum_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= accum_local(7);
                accum_local <= accum_local(6 downto 0) & '0';
              when "00101" =>           -- SRA
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if accum_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= accum_local(0);
                accum_local <= accum_local(7) & accum_local(7 downto 1);
              when "00110" =>           -- SWAP
                half_carry_flag <= '0';
                sub_flag        <= '0';
                carry_flag      <= '0';
                accum_local     <= accum_local(3 downto 0) & accum_local(7 downto 4);
                if accum_local = x"00" then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
              when "00111" =>           -- SRL
                half_carry_flag <= '0';
                sub_flag        <= '0';
                if accum_local = x"00" and carry_flag = '0' then
                  zero_flag <= '1';
                else
                  zero_flag <= '0';
                end if;
                carry_flag  <= accum_local(0);
                accum_local <= '0' & accum_local(7 downto 1);
              when "01000" =>           -- BIT 0
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(accum_local(0));
              when "01001" =>           -- BIT 1
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(accum_local(1));
              when "01010" =>           -- BIT 2
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(accum_local(2));
              when "01011" =>           -- BIT 3
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(accum_local(3));
              when "01100" =>           -- BIT 4
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(accum_local(4));
              when "01101" =>           -- BIT 5
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(accum_local(5));
              when "01110" =>           -- BIT 6
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(accum_local(6));
              when "01111" =>           -- BIT 7
                sub_flag        <= '0';
                half_carry_flag <= '1';
                zero_flag       <= not(accum_local(7));
              when "10000" =>           -- RES 0
                accum_local(0) <= '0';
              when "10001" =>           -- RES 1
                accum_local(1) <= '0';
              when "10010" =>           -- RES 2
                accum_local(2) <= '0';
              when "10011" =>           -- RES 3
                accum_local(3) <= '0';
              when "10100" =>           -- RES 4
                accum_local(4) <= '0';
              when "10101" =>           -- RES 5
                accum_local(5) <= '0';
              when "10110" =>           -- RES 6
                accum_local(6) <= '0';
              when "10111" =>           -- RES 7
                accum_local(7) <= '0';
              when "11000" =>           -- SET 0
                accum_local(0) <= '1';
              when "11001" =>           -- SET 1
                accum_local(1) <= '1';
              when "11010" =>           -- SET 2
                accum_local(2) <= '1';
              when "11011" =>           -- SET 3
                accum_local(3) <= '1';
              when "11100" =>           -- SET 4
                accum_local(4) <= '1';
              when "11101" =>           -- SET 5
                accum_local(5) <= '1';
              when "11110" =>           -- SET 6
                accum_local(6) <= '1';
              when "11111" =>           -- SET 7
                accum_local(7) <= '1';
              when others => null;
            end case;
          when "110" =>                 -- (HL)
            if rd = '0' then
              case bit_cmd is
                when "00000" =>         -- RLC
                  half_carry_flag <= '0';
                  sub_flag        <= '0';
                  if data_bus_i = x"00" and carry_flag = '0' then
                    zero_flag <= '1';
                  else
                    zero_flag <= '0';
                  end if;
                  carry_flag  <= data_bus_i(7);
                  data_bus_o <= data_bus_i(6 downto 0) & data_bus_i(7);
                when "00010" =>         -- RL
                  half_carry_flag <= '0';
                  sub_flag        <= '0';
                  if data_bus_i = x"00" and carry_flag = '0' then
                    zero_flag <= '1';
                  else
                    zero_flag <= '0';
                  end if;
                  carry_flag  <= data_bus_i(7);
                  data_bus_o <= data_bus_i(6 downto 0) & carry_flag;
                when "00001" =>         -- RRC
                  half_carry_flag <= '0';
                  sub_flag        <= '0';
                  if data_bus_i = x"00" and carry_flag = '0' then
                    zero_flag <= '1';
                  else
                    zero_flag <= '0';
                  end if;
                  carry_flag  <= data_bus_i(0);
                  data_bus_o <= data_bus_i(0) & data_bus_i(7 downto 1);
                when "00011" =>         -- RR
                  half_carry_flag <= '0';
                  sub_flag        <= '0';
                  if data_bus_i = x"00" and carry_flag = '0' then
                    zero_flag <= '1';
                  else
                    zero_flag <= '0';
                  end if;
                  carry_flag  <= data_bus_i(0);
                  data_bus_o <= carry_flag & data_bus_i(7 downto 1);
                when "00100" =>         -- SLA
                  half_carry_flag <= '0';
                  sub_flag        <= '0';
                  if data_bus_i = x"00" and carry_flag = '0' then
                    zero_flag <= '1';
                  else
                    zero_flag <= '0';
                  end if;
                  carry_flag  <= data_bus_i(7);
                  data_bus_o <= data_bus_i(6 downto 0) & '0';
                when "00101" =>         -- SRA
                  half_carry_flag <= '0';
                  sub_flag        <= '0';
                  if data_bus_i = x"00" and carry_flag = '0' then
                    zero_flag <= '1';
                  else
                    zero_flag <= '0';
                  end if;
                  carry_flag  <= data_bus_i(0);
                  data_bus_o <= data_bus_i(7) & data_bus_i(7 downto 1);
                when "00110" =>         -- SWAP
                  half_carry_flag <= '0';
                  sub_flag        <= '0';
                  carry_flag      <= '0';
                  data_bus_o     <= data_bus_i(3 downto 0) & data_bus_i(7 downto 4);
                  if data_bus_i = x"00" then
                    zero_flag <= '1';
                  else
                    zero_flag <= '0';
                  end if;
                when "00111" =>         -- SRL
                  half_carry_flag <= '0';
                  sub_flag        <= '0';
                  if data_bus_i = x"00" and carry_flag = '0' then
                    zero_flag <= '1';
                  else
                    zero_flag <= '0';
                  end if;
                  carry_flag  <= data_bus_i(0);
                  data_bus_o <= '0' & data_bus_i(7 downto 1);
                when "01000" =>         -- BIT 0
                  sub_flag        <= '0';
                  half_carry_flag <= '1';
                  zero_flag       <= not(data_bus_i(0));
                when "01001" =>         -- BIT 1
                  sub_flag        <= '0';
                  half_carry_flag <= '1';
                  zero_flag       <= not(data_bus_i(1));
                when "01010" =>         -- BIT 2
                  sub_flag        <= '0';
                  half_carry_flag <= '1';
                  zero_flag       <= not(data_bus_i(2));
                when "01011" =>         -- BIT 3
                  sub_flag        <= '0';
                  half_carry_flag <= '1';
                  zero_flag       <= not(data_bus_i(3));
                when "01100" =>         -- BIT 4
                  sub_flag        <= '0';
                  half_carry_flag <= '1';
                  zero_flag       <= not(data_bus_i(4));
                when "01101" =>         -- BIT 5
                  sub_flag        <= '0';
                  half_carry_flag <= '1';
                  zero_flag       <= not(data_bus_i(5));
                when "01110" =>         -- BIT 6
                  sub_flag        <= '0';
                  half_carry_flag <= '1';
                  zero_flag       <= not(data_bus_i(6));
                when "01111" =>         -- BIT 7
                  sub_flag        <= '0';
                  half_carry_flag <= '1';
                  zero_flag       <= not(data_bus_i(7));
                when "10000" =>         -- RES 0
                  data_bus_o <= data_bus_i(7 downto 1) & '0';
                when "10001" =>         -- RES 1
                  data_bus_o <= data_bus_i(7 downto 2) & '0' & data_bus_i(0);
                when "10010" =>         -- RES 2
                  data_bus_o <= data_bus_i(7 downto 3) & '0' & data_bus_i(1 downto 0);
                when "10011" =>         -- RES 3
                  data_bus_o <= data_bus_i(7 downto 4) & '0' & data_bus_i(2 downto 0);
                when "10100" =>         -- RES 4
                  data_bus_o <= data_bus_i(7 downto 5) & '0' & data_bus_i(3 downto 0);
                when "10101" =>         -- RES 5
                  data_bus_o <= data_bus_i(7 downto 6) & '0' & data_bus_i(4 downto 0);
                when "10110" =>         -- RES 6
                  data_bus_o <= data_bus_i(7) & '0' & data_bus_i(5 downto 0);
                when "10111" =>         -- RES 7
                  data_bus_o <= '0' & data_bus_i(6 downto 0);
                when "11000" =>         -- SET 0
                  data_bus_o <= data_bus_i(7 downto 1) & '1';
                when "11001" =>         -- SET 1
                  data_bus_o <= data_bus_i(7 downto 2) & '1' & data_bus_i(0);
                when "11010" =>         -- SET 2
                  data_bus_o <= data_bus_i(7 downto 3) & '1' & data_bus_i(1 downto 0);
                when "11011" =>         -- SET 3
                  data_bus_o <= data_bus_i(7 downto 4) & '1' & data_bus_i(2 downto 0);
                when "11100" =>         -- SET 4
                  data_bus_o <= data_bus_i(7 downto 5) & '1' & data_bus_i(3 downto 0);
                when "11101" =>         -- SET 5
                  data_bus_o <= data_bus_i(7 downto 6) & '1' & data_bus_i(4 downto 0);
                when "11110" =>         -- SET 6
                  data_bus_o <= data_bus_i(7) & '1' & data_bus_i(5 downto 0);
                when "11111" =>         -- SET 7
                  data_bus_o <= '1' & data_bus_i(6 downto 0);
                when others => null;
              end case;
            end if;
          when others => null;
        end case;
      end if;
    end if;
  end process;

end architecture rtl;
