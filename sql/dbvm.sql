-- a simple virtual machine stored in a database

----------------------------------------------------------------
-- core tables
----------------------------------------------------------------
create table vm_ram  (id integer primary key, value); -- ip reg = -1
create table vm_data (id integer primary key, value);
create table vm_addr (id integer primary key, value);

----------------------------------------------------------------
-- views of the stack
----------------------------------------------------------------
create view vm_tos as
  select value from vm_data where id = (select max(id) from vm_data);

create view vm_toa as -- top of address stack
  select value from vm_addr where id = (select max(id) from vm_addr);

create view vm_nos as
  select value from vm_data where id = (select max(id) from vm_data) -1;

create view vm_stack as
  select ((select max(id) from vm_data) - id) as depth, value from vm_data;

create view vm_stacks as
  select 'v' as 'which',* from vm_stack union select '^',* from vm_addr;

create view vm_depth as
  select max(id) from vm_data as depth;

----------------------------------------------------------------
-- interpreter
----------------------------------------------------------------
create view vm as
  select 'insert opcodes here' as op, 'to run them' as arg ;

----------------------------------------------------------------
-- opcodes : literal values
----------------------------------------------------------------
create trigger op_nop instead of insert on vm when new.op in (0, 'nop')
  begin
     select 'nop' as op, '-' as arg; -- have to put something here
  end;

create trigger op_lit instead of insert on vm when new.op in (1,'lit')
  begin
    insert into vm_data (value) values (new.arg);
  end;

-- since sqlite doesn't have variables, we'll use the
-- address stack as a temporary holding place
create trigger op_tmp instead of insert on vm when new.op in (-1,'tmp')
  begin
    insert into vm_addr (value) values (new.arg);
  end;

-- a helper to clear both stacks:
create trigger op_reset instead of insert on vm when new.op in (-2,'reset')
  begin
    delete from vm_addr;
    delete from vm_data;
  end;

----------------------------------------------------------------
-- opcodes : stack manipulations
----------------------------------------------------------------
create trigger op_dup instead of insert on vm when new.op in (2,'dup')
  begin
    insert into vm_data (value) select value from vm_tos;
  end;

create trigger op_drop instead of insert on vm when new.op in ( 3, 'drop')
  begin
    delete from vm_data where id in vm_depth;
  end;

-- todo: check depth first
create trigger op_swap instead of insert on vm when new.op in ( 4, 'swap')
  begin
    -- make a copy of the nos
    insert into vm values ('tmp', (select value from vm_nos));
    -- copy tos to nos
    update vm_data set value = (select value from vm_tos)
      where id = (select max(id)-1 from vm_data);
    -- drop the tos and pop the copy
    insert into vm values 
      ('drop', null),
      ('pop', null); -- see below
  end;

create trigger op_push instead of insert on vm when new.op in ( 5, 'push')
  begin
    insert into vm_addr (value) select value from vm_tos;
    delete from vm_data where id in vm_depth;
  end;

create trigger op_pop instead of insert on vm when new.op in ( 6, 'pop')
  begin
    insert into vm_data (value) select value from vm_toa;
    delete from vm_addr where id = (select max(id) from vm_addr);
  end;

----------------------------------------------------------------
-- opcodes : memory
----------------------------------------------------------------
create trigger op_get instead of insert on vm when new.op in (14, 'get','fetch')
  begin
     insert into vm values
       -- push value at address to return stack
       ('tmp', (select value from vm_ram
                where id=(select value from tos))),
       -- drop the address
       ('drop',null),
       -- and leave the value
       ('pop', null);
  end;

create trigger op_put instead of insert on vm when new.op in (15, 'put',' store')
  begin
     replace into vm_ram (id, value) values
       ((select value from tos),   -- addr is in tos
        (select value from nos));  -- data is in nos
     -- pop top two values:
     delete from vm_data where id > (select max(id) from vm_addr)-2;
  end;

----------------------------------------------------------------
-- opcodes : arithmetic and logic
----------------------------------------------------------------

create trigger op_binop instead of insert on vm when new.op in (-3, 'binop')
  begin
    insert into vm values
       ('drop',''),
       ('drop',''),
       ('lit', new.arg);
  end;

create trigger op_add instead of insert on vm when new.op in (16, 'add')
  begin
    insert into vm values
      ('binop', (select value from vm_nos)+(select value from vm_tos));
  end;

create trigger op_sub instead of insert on vm when new.op in (17, 'sub')
  begin
    insert into vm values
      ('binop', (select value from vm_nos)-(select value from vm_tos));
  end;

create trigger op_mul instead of insert on vm when new.op in (18, 'mul')
  begin
    insert into vm values
      ('binop', (select value from vm_nos)*(select value from vm_tos));
  end;

create trigger op_div instead of insert on vm when new.op in (19, 'div')
  begin
    insert into vm values
       ('binop', (select value from vm_nos)/(select value from vm_tos));
  end;

create trigger op_and instead of insert on vm when new.op in (20, 'and')
  begin
    insert into vm values
       ('binop', (select value from vm_nos)&(select value from vm_tos));
  end;

-- vel is the latin word for inclusive or. i just like it better
create trigger op_vel instead of insert on vm when new.op in (21, 'vel','or')
  begin
    insert into vm values
       ('binop', (select value from vm_nos)|(select value from vm_tos));
  end;

-- vel is the latin word for inclusive or. i just like it better
create trigger op_xor instead of insert on vm when new.op in (22, 'xor')
  begin
    -- a xor b = (a & ~b) | (b&~a)
    insert into vm values
       ('binop',
          (  (select value from vm_nos) & ~(select value from vm_tos))
        | ( ~(select value from vm_nos) &  (select value from vm_tos)));
  end;

create trigger op_shl instead of insert on vm when new.op in (23, 'shl')
  begin
    insert into vm values
       ('binop', (select value from vm_nos)<<(select value from vm_tos));
  end;

create trigger op_shr instead of insert on vm when new.op in (24, 'shr')
  begin
    insert into vm values
       ('binop', (select value from vm_nos)>>(select value from vm_tos));
  end;

create trigger op_inc instead of insert on vm when new.op in (26, 'inc')
  begin
    update vm_data set value = value + 1 where id in vm_depth;
  end;

create trigger op_dec instead of insert on vm when new.op in (27, 'dec')
  begin
    update vm_data set value = value - 1 where id in vm_depth;
  end;

create trigger op_lt instead of insert on vm when new.op in (10, 'lt')
  begin
    insert into vm values
       ('binop', (select value from vm_nos) < (select value from vm_tos));
  end;

create trigger op_gt instead of insert on vm when new.op in (11, 'gt')
  begin
    insert into vm values
       ('binop', (select value from vm_nos) > (select value from vm_tos));
  end;

create trigger op_ne instead of insert on vm when new.op in (12, 'ne')
  begin
    insert into vm values
       ('binop', (select value from vm_nos) <> (select value from vm_tos));
  end;

create trigger op_eq instead of insert on vm when new.op in (13, 'eq')
  begin
    insert into vm values
       ('binop', (select value from vm_nos) = (select value from vm_tos));
  end;

----------------------------------------------------------------
-- opcodes : i/o system  ( TODO)
----------------------------------------------------------------

create trigger op_in instead of insert on vm when new.op in (28, 'in')
  begin
     select 0;
  end;

create trigger op_out instead of insert on vm when new.op in (29, 'out')
  begin
     select 0;
  end;

create trigger op_wait instead of insert on vm when new.op in (30, 'wait')
  begin
     select 0;
  end;

----------------------------------------------------------------
-- opcodes : control flow ( TODO )
----------------------------------------------------------------

-- unconditional jump
create trigger op_jmp instead of insert on vm when new.op in ( 8, 'jmp')
  begin
    update vm_ram set value = new.arg where id=-1;
  end;

-- counted loop
create trigger op_loop instead of insert on vm when new.op in ( 7, 'loop')
  begin
    -- decrement the tos
    update vm_data set value = value - 1 where id in vm_depth;
    -- loop back if tos > 0 else proceed to next instruction
    update vm_ram set value =
      case (select value from tos)
        when 0 then value + 1
        else new.arg
      end
    where id=-1;
    -- also drop the tos if 0
    delete from vm_data where value = 0 and id = vm_depth;
  end;


-- invoke
create trigger op_ivk instead of insert on vm when new.op > 32
  begin
    -- store instruction pointer
    insert into vm values ('tmp', (select value from vm_ram where id = -1));
    -- make the jump 
    update vm_ram set value = new.op where id = -1;
  end;

-- return 
create trigger op_ret instead of insert on vm when new.op in ( 9, 'ret')
  begin
    insert into vm values
       ('jmp',  (select value from vm_toa)),
       ('pop',  null), -- move return address to main stack
       ('drop', null); -- then delete it
  end;

-- conditional jump (jump when zero)
create trigger op_jwz instead of insert on vm when new.op in ( 31, 'jwz')
  begin
    insert into vm values (-- the pair consisting of...
       -- this opcode:
       case (select value from vm_tos)
         when 0 then 'jmp'
         else 'nop'
       end,
       -- ... and the argument
       new.arg);
    -- also drop the tos if 0
    delete from vm_data where value = 0 and id in vm_depth;
  end;

-- return when zero
create trigger op_rwz instead of insert on vm when new.op in (25, 'rwz')
  begin
    -- pretty much the same as jwz, but return instead of jump
    insert into vm values (-- the pair consisting of...
       -- this opcode:
       case (select value from vm_tos)
         when 0 then 'ret'
         else 'nop'
       end,
       -- ... and a null
       null);
    -- also drop the tos if 0
    delete from vm_data where value = 0 and id in vm_depth;
  end;
