------------------------------------------------------------------
-- core relations
------------------------------------------------------------------
create table if not exists node (
  nid integer primary key,
  knd integer  not null default 0,
  val, -- (can be any of the sqlite primitive types)
  foreign key(knd) references kind deferrable initially deferred );

--
create table if not exists edge (
  eid integer primary key,
  sub integer references node,
  rel integer references node,
  obj integer references node,
  seq integer not null default 0,
  began datetime default current_timestamp,
  ended datetime default null );
--  unique (sub, rel, obj, seq) );

--
create view if not exists trip as
  select eid, seq,
    s.knd as subknd, s.nid as subnid, s.val as sub,
    r.knd as relknd, r.nid as relnid, r.val as rel,
    o.knd as objknd, o.nid as objnid, o.val as obj
  from edge, node as s, node as r, node as o
  where edge.sub = s.nid
    and edge.rel = r.nid
    and edge.obj = o.nid
    and ended is null ;


-- -- management of the triplestore  ---
create table if not exists meta (
  id integer primary key,
  k text unique,
  v variant );

--
create trigger if not exists del_triple
  instead of delete on trip
  begin
    update edge set ended = datetime('now') where edge.eid = old.eid;
  end;

--
create trigger if not exists new_triple
  instead of insert on trip
  begin
    insert or ignore into node (val) values (new.sub), (new.rel), (new.obj);
    insert into edge (sub, rel, obj, seq, began)
    select (select nid from node where val=new.sub) as subID,
           (select nid from node where val=new.rel) as relID,
           (select nid from node where val=new.obj) as objID,
           new.seq,
	   datetime('now') as began;
    replace into meta ('k','v')
      values ('last_insert_rowid', last_insert_rowid());
  end;


-- -- type system. system nodes have keys <= 0
create table if not exists kind (
  knd integer primary key,
  foreign key (knd) references node (nid));
--
insert or ignore into node (nid, knd, val) values
   -- meta stuff --
   (0, -2, 'null'), (-1, -1, 'kind'), (-2, -1, 'void'),
   -- primitive types --
   (-3, -1, 'Str'), (-4, -1, 'Int'), (-5, -1, 'Num'),
   (-6, -1, 'Set'),
   -- compound types --
   (-7, -1, 'Tuple'), (-8, -1, 'List'), (-9, -1, 'Tree'),
   (-10, -1, 'Grid'), (-11, -1, 'Dict'), (-12, -1, 'Page'),
   -- grammar combinators --
   (-100, -1, 'Grammar'), (-101, -1, 'nul' ), (-102, -1, 'any' ),
   (-103, -1, 'lit' ), (-104, -1, 'alt' ), (-105, -1, 'seq' ),
   (-106, -1, 'rep' ), (-107, -1, 'neg' ), (-108, -1, 'opt' ),
   (-109, -1, 'orp' ), (-110, -1, 'def' ), (-111, -1, 'act' ),
   (-112, -1, 'tok' ), (-113, -1, 'skip'), (-114, -1, 'node'),
   (-115, -1, 'hide'), (-116, -1, 'lift'), (-117, -1, 'virt'),
   -- edge types --
   (-200, -1, 'edge'), (-201, -200, '::'), (-202, -200, ':='),
   (-203, -200, './'), (-204, -200, '->'), (-205, -200, '<-'),
   (-206, -200, '[wd]'),
   -- home page --
   (-1000, -12, 'home');
--
create view if not exists kinds as
  select nid as knd, val as kind from node where nid in kind;
--
create trigger if not exists new_kind instead of insert on kinds
  begin
    insert into node (knd, val) values (-1, new.kind);
    insert into kind values(last_insert_rowid());
  end;
--
insert or ignore into kind (knd) -- TODO: auto-maintain with a trigger
  select nid from node where knd=-1;
--
pragma foreign_keys=1;

----------------------------------------------------------
-- -- trees
-----------------------------------------------------------
create table if not exists trees ( tree primary key );

--
create table if not exists tree_data (
  tree   integer references trees,
  parent integer references node,
  child  integer references node,
  seq    integer );

--
create table if not exists tree_path ( -- auto-generated subtree information
  tree   integer references trees,
  above  integer references node,
  below  integer references node,
  steps  integer not null default 0 );

-- -- temp tables (sqlite prohibits create/drop inside a trigger)
create table if not exists subtree (nid integer);
create table if not exists flags ( flag text primary key );

-- -- tree triggers : insert/update

create trigger if not exists tree_add_node after insert on tree_data
  begin
    insert into tree_path (tree, above, below, steps)
        -- create a new path to itself:
        select new.tree, new.child, new.child, 0
      union all
        -- copy the parent's path to the root:
        select tree, above, new.child, steps + 1
        from tree_path
        where below = new.parent
          and tree = new.tree;
  end;

--
create trigger if not exists tree_prevent_child_mod
  before update of child on tree_data
  begin
    select raise (abort,
      'update of tree_data.child prohibited. delete and re-add instead.');
  end;

--
create trigger if not exists tree_prevent_tree_mod
  before update of tree on tree_data
  begin
    select raise (abort,
      'update of tree_data.tree prohibited. delete and re-add instead.');
  end;

-- -- tree triggers : delete

create trigger if not exists tree_del_node after delete on tree_data
  when not exists(select * from flags
                  where flag='recursive-tree-delete')
  begin
    -- this trigger deletes the whole subtree, which would cause
    -- some harmless but slightly wasteful recursion.
    insert into flags values ('recursive-tree-delete');

    -- first, find all all descendants of the node to delete:
    delete from subtree;
    insert into subtree (nid)
      select below as nid from tree_path
      where tree=old.tree and above=old.child;

    -- delete all traces of these nodes from tree_path.
    -- we don't need to consider the 'above' column because
    -- that's already covered by the notion of 'subtree'.
    delete from tree_path
      where tree = old.tree
        and below in (select nid from subtree);

    -- now we can 'garbage collect' the subtree from the
    -- main tree table. this would trigger recursion if it
    -- weren't for the flag we set.
    delete from tree_data
    where tree = old.tree
      and child in (select nid from subtree);

    -- finally, clean up our mess:
    delete from flags where flag = 'recursive-tree-delete';
    delete from subtree;
  end;

-- -- tree triggers : moving nodes

create trigger if not exists tree_move_node
  after update of parent on tree_data when new.parent is not null
  begin
 -- techniques adapted from :
 --  http://jdobbie.blogspot.com/2009/07/closure-trees.html and
 --  www.mysqlperformanceblog.com/2011/02/14/moving-subtrees-in-closure-table/

    delete from subtree;
    insert into subtree
      select below from tree_path
      where tree=old.tree and above=old.child;

    -- first delete any 'old' ancestors for our subtree:
    delete from tree_path
      where tree = old.tree
        and above in (select below from subtree)
        and below not in (select below from subtree);

    -- create new ancestors in their place:
    insert into tree_path (above, below, steps)
    select up.above,
           dn.below,
           up.steps + dn.steps + 1 as steps
      from tree_path as up cross join tree_path as dn
     where up.tree = old.tree
       and dn.tree = old.tree
       and dn.above = new.child
       and up.below = new.parent;

     -- clean up:
     delete from subtree;
  end;

-- -- tree_crumbs shows breadcrumb trail for a path.
-- also sorts results in depth-first walk order.
--
-- !! if your tree has nodes with more than 10000 child
--    nodes, you will need to change the call to substr()
--    to include more digits, or the nodes will not be
--    sorted correctly.
create view if not exists tree_crumbs as
  select tree, target, group_concat(crumb, ':') as crumbs
  from (
    select tp.tree, tp.below as target, substr('00000'||seq, -4) as crumb
    from tree_path tp
      left join tree_data td on (tp.tree=td.tree and tp.above=td.child)
      left join node n on (above=n.nid)
    order by tp.tree, target, steps desc )
  group by tree, target;

-- -- a view to give you the depth of any node
create view if not exists tree_depth as
  select tree, below as nid, max(steps) as depth
  from tree_path
  group by tree, below;

-- -- this combines tree_crumbs with depth, node and
-- type data so you can just select from this table and
-- get everything you need for a walk of the tree.
create view if not exists tree_walk as
  select tc.tree,
     k.nid as knd, k.val as kind,
     n.nid, n.val as node,
     td.depth, ts.seq, tc.crumbs
  from tree_crumbs tc
    left join tree_depth td on (tc.tree=td.tree and tc.target=td.nid)
    left join tree_data  ts on (tc.tree=ts.tree and tc.target=ts.child)
    left join node n on (tc.target=n.nid)
    left join node k on (n.knd=k.nid)
  order by crumbs;

-- -- a view to give you the leaves of a tree
create view if not exists tree_leaf as
  select tree, above as leaf
  from tree_path
  group by tree, above
  having count(below) = 1;

-- -- and the 'roots' (or rather all top-level nodes)
create view if not exists tree_root as
  select tree, below as root
  from tree_path
  group by tree, below
  having count(above) = 1;


-------------------------------------------------------
-- -- outlines (collapsed/expanded view of a tree)
-------------------------------------------------------
create table if not exists outline_master (
  olid integer primary key,
  tree  integer references trees );

--
create table if not exists outline_collapse (
  olid integer references outline_master,
  collapse integer references node );

--
create view if not exists outline_hidden as
  select olid, below as hide from outline_collapse oc
  inner join tree_path tp on oc.collapse=tp.above
  where tp.steps <> 0;

--
create view if not exists outline as
  select olid, nid, tw.depth, tw.kind, tw.node,
    exists(select collapse from outline_collapse oc
           where oc.olid=om.olid and collapse=nid) as collapsed,
    exists(select hide from outline_hidden oh
           where oh.olid=om.olid and hide=nid) as hidden,
    exists(select leaf from tree_leaf where leaf=nid) as leaf
  from outline_master om natural join tree_walk tw;


-----------------------------------------------------------
-- grammar type system
-----------------------------------------------------------
--
insert or ignore into trees values (-1),(-12),(-1000); -- kinds, pages, home
--
insert or ignore into outline_master values (-1,-1);
--
insert or ignore into tree_data (tree, parent, child, seq) values
  (  -1,      0,     -1,   1), -- kind
  (  -1,     -1,     -2,   2), -- void
  (  -1,     -1,     -3,   3), -- Str
  (  -1,     -1,     -4,   4), -- Int
  (  -1,     -1,     -5,   5), -- Num
  (  -1,     -1,     -6,   6), -- Set
  (  -1,     -1,     -7,   7), -- Tuple
  (  -1,     -1,     -8,   8), -- List
  (  -1,     -1,     -9,   9), -- Tree
  (  -1,     -1,    -10,  10), -- Grid
  (  -1,     -1,    -11,  11), -- Dict
  (  -1,     -1,   -100, 100), -- Grammar
  (  -1,   -100,   -101,   1),    -- nul
  (  -1,   -100,   -102,   2),    -- any
  (  -1,   -100,   -103,   3),    -- lit
  (  -1,   -100,   -104,   4),    -- alt
  (  -1,   -100,   -105,   5),    -- seq
  (  -1,   -100,   -106,   6),    -- rep
  (  -1,   -100,   -107,   7),    -- neg
  (  -1,   -100,   -108,   8),    -- opt
  (  -1,   -100,   -109,   9),    -- orp
  (  -1,   -100,   -110,  10),    -- def
  (  -1,   -100,   -111,  11),    -- act
  (  -1,   -100,   -112,  12),    -- tok
  (  -1,   -100,   -113,  13),    -- skip
  (  -1,   -100,   -114,  14),    -- node
  (  -1,   -100,   -115,  15),    -- hide
  (  -1,   -100,   -116,  16),    -- lift
  (  -1,   -100,   -117,  17);    -- virt

-----------------------------------------------------------
-- pages
-----------------------------------------------------------
--
insert into trip(sub, rel, obj, seq) values
  ('home', ':=', 'hello, world.'||x'0a'||'welcome to minneron!'||x'0a', 0);

------------------------------------------------------
-- support for arbitrary data structures
------------------------------------------------------
--
create table if not exists list (
  lid integer references node,
  nid integer references node,
  seq integer );

--
create table if not exists dict (
  did    integer references node,
  keynid integer references node,
  valnid integer references node );

--
create table if not exists grid (
  nid integer references node,
  knd integer references kind,
  x   integer,
  y   integer,
  val integer );

------------------------------------------------------------------
-- help / docs table
------------------------------------------------------------------
--
create table if not exists db_meta (name string unique, purpose string);
--
create view if not exists tables as
  select master.name, master.type, meta.purpose
  from sqlite_master as master
    left natural join db_meta as meta
  where type in ('table', 'view');
--
insert or ignore into db_meta (name, purpose) values
   ('db_meta', 'the table containing these descriptions'),
   ('edge', 'arbitrary relations between nodes'),
   ('flags', '(helper table used by triggers)'),
   ('kind', 'redundant index of internal type system'),
   ('kinds', 'lookup view (with insert trigger) for kinds'),
   ('node', 'the main table, containing all values'),
   ('outline', 'combined view of most outline/tree data'),
   ('outline_collapse', 'allows outlines to fold parts of the tree'),
   ('outline_hidden', 'does the work of showing/hiding subnodes'),
   ('outline_master', 'outlines are just views of a tree'),
   ('subtree', '(helper table used by triggers)'),
   ('tree_crumbs', 'shows the "breadcrumb trail" for tree paths'),
   ('tree_data', 'core data for ordered trees'),
   ('tree_depth', 'calculates depth of nodes in the tree'),
   ('tree_leaves', 'selects the leaves of trees'),
   ('trees', 'redundant list of all trees'),
   ('tree_path', '(trigger-generated helper for trees)'),
   ('tree_root', 'selects the roots of the trees'),
   ('tree_walk', 'a flattened view of the trees');
