-- the  comments are for sql2pas

create table if not exists node (
  ID integer primary key,
  name text unique );

--
create table if not exists edge (
   ID integer primary key,
   subID integer,
   relID integer,
   objID integer,
   seq   integer,
   began datetime default 'now',
   ended datetime default null );
--
create view if not exists trip as
  select
    arc.ID as ID,
    sub.name as sub,
    rel.name as rel,
    obj.name as obj
  from edge arc, node sub, node rel, node obj
  where arc.subID=sub.ID
    and arc.relID=rel.ID
    and arc.objID=obj.ID
    and ended is null;
--
-- management of the triplestore
create table if not exists meta (
  id integer primary key,
  k text unique,
  v variant );
--
create trigger if not exists del_triple
  instead of delete on trip
  begin
    update edge set ended = 'now' where edge.id = old.id;
  end;
--
create trigger if not exists new_triple
  instead of insert on trip
  begin
    replace into node (name) values (new.sub), (new.rel), (new.obj);
    insert into edge (subID, relID, objID, began)
    select (select ID from node where name=new.sub) as subID, 
           (select ID from node where name=new.rel) as relID,
           (select ID from node where name=new.obj) as objID,
	   'now' as began;
    replace into meta ('k','v')
      values ('last_insert_rowid', last_insert_rowid());
  end;
