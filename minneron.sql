-- these three tables should be enough to track basic outlines.

create table kind (
  kid integer primary key,
  nid integer references node
);

create table node (
  nid integer primary key,
  kid integer references kind,
  val, -- (can be any of the sqlite primitive types)
  foreign key(kid) references kind deferrable initially deferred
);

-- built-in types and nodes all have non-positive primary keys
begin;
  insert into node (nid, kid, val) values
     -- meta stuff --
     (   0,  0, 'null'),
     (  -1, -1, 'type'),
     -- primitive types --
     (  -2, -1, 'Str'),
     (  -3, -1, 'Int'),
     (  -4, -1, 'Num'),
     (  -5, -1, 'Set'),
     -- compound types --
     (  -6, -1, 'Tuple'),
     (  -7, -1, 'List'),
     (  -8, -1, 'Tree'),
     (  -9, -1, 'Grid'),
     ( -10, -1, 'Dict'),
     -- grammar combinators --
     (-100,   -1, 'Grammar'),
     (-101, -100, 'nul' ),
     (-102, -100, 'any' ),
     (-103, -100, 'lit' ),
     (-104, -100, 'alt' ),
     (-105, -100, 'seq' ),
     (-106, -100, 'rep' ),
     (-107, -100, 'neg' ),
     (-108, -100, 'opt' ),
     (-109, -100, 'orp' ),
     (-110, -100, 'def' ),
     (-111, -100, 'act' ),
     (-112, -100, 'tok' ),
     (-113, -100, 'skip'),
     (-114, -100, 'node'),
     (-115, -100, 'hide'),
     (-116, -100, 'lift'),
     (-117, -100, 'virt');
commit;


create table tree (
  trid  integer references node,
  above integer references node,
  below integer references node,
  depth integer
);

create table edge (
  eid integer primary key,
  sub integer references node,
  rel integer references node,
  obj integer references node,
  seq integer
);

create table list (
  lid integer references node,
  nid integer references node,
  seq integer
);

create table dict (
  did    integer references node,
  keynid integer references node,
  valnid integer references node
);

create table grid (
  nid integer references node,
  kid integer references kind,
  x   integer,
  y   integer,
  val integer
);
