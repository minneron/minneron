-- these three tables should be enough to track basic outlines.

------------------------------------------------------------------
-- basic data type
------------------------------------------------------------------

create table kind (
  knd integer primary key,
  foreign key (knd) references node (nid)
);

create table node (
  nid integer primary key,
  knd integer references kind,
  val, -- (can be any of the sqlite primitive types)
  foreign key(knd) references kind deferrable initially deferred
);

-- built-in types and nodes all have non-positive primary keys
begin;
  insert into node (nid, knd, val) values
     -- meta stuff --
     (   0, -2, 'null'),
     (  -1, -1, 'kind'),
     (  -2, -1, 'void'),
     -- primitive types --
     (  -3, -1, 'Str'),
     (  -4, -1, 'Int'),
     (  -5, -1, 'Num'),
     (  -6, -1, 'Set'),
     -- compound types --
     (  -7, -1, 'Tuple'),
     (  -8, -1, 'List'),
     (  -9, -1, 'Tree'),
     ( -10, -1, 'Grid'),
     ( -11, -1, 'Dict'),
     -- grammar combinators --
     (-100, -1, 'Grammar'),
     (-101, -1, 'nul' ),
     (-102, -1, 'any' ),
     (-103, -1, 'lit' ),
     (-104, -1, 'alt' ),
     (-105, -1, 'seq' ),
     (-106, -1, 'rep' ),
     (-107, -1, 'neg' ),
     (-108, -1, 'opt' ),
     (-109, -1, 'orp' ),
     (-110, -1, 'def' ),
     (-111, -1, 'act' ),
     (-112, -1, 'tok' ),
     (-113, -1, 'skip'),
     (-114, -1, 'node'),
     (-115, -1, 'hide'),
     (-116, -1, 'lift'),
     (-117, -1, 'virt');
commit;


create view kinds as
  select nid as knd, val as kind from node where nid in kind;

create trigger new_kind instead of insert on kinds
  begin
    insert into node (knd, val) values (-1, new.kind);
    insert into kind values(last_insert_rowid());
  end;

-- TODO: auto-maintain the kind table with a trigger
replace into kind (knd)
  select nid from node where knd=-1;

pragma foreign_keys=1;


create table trees (
  tree primary key
);

-- tree_data contains the core data for ordered trees.
create table tree_data (
  tree   integer references trees,
  parent integer references node,
  child  integer references node,
  seq    integer );

-- tree_path contains automatically generated information
-- about each node's full subtree.
create table tree_path (
  tree   integer references trees,
  above  integer references node,
  below  integer references node,
  steps  integer not null default 0
);


-- this is used by tree_del_node
create table flags (
  flag text primary key
);

create trigger tree_add_node after insert on tree_data
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

-- i would have preferred to have this get created and
-- destroyed inside the triggers, but sqlite doesn't
-- allow create/drop statements inside triggers.
create table subtree (nid integer);

create trigger tree_del_node after delete on tree_data
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

create trigger tree_prevent_child_mod before update of child on tree_data
  begin
    select raise (abort,
      'update of tree_data.child prohibited. delete and re-add instead.');
  end;

create trigger tree_prevent_tree_mod before update of tree on tree_data
  begin
    select raise (abort,
      'update of tree_data.tree prohibited. delete and re-add instead.');
  end;

create trigger tree_move_node after update of parent on tree_data
  when new.parent is not null
  begin
    -- this technique is derived from:
    --       http://jdobbie.blogspot.com/2009/07/closure-trees.html
    --   and http://www.mysqlperformanceblog.com/2011/02/14/moving-subtrees-in-closure-table/
    -- but adapted for sqlite, which seems to have a more flexible syntax than mysql.
    delete from subtree;
    insert into subtree
      select below from tree_path
      where tree=old.tree and above=old.child;

    -- first we delete any 'old' ancestors for our subtree:
    delete from tree_path
      where tree = old.tree
        and above in (select below from subtree)
        and below not in (select below from subtree);

    -- now, create some new ancestors in their place:
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

-- A view that shows the 'breadcrumb trail' for a path.
-- this also sorts the results in the correct order for
-- performing a depth-first walk of the tree.
--
-- !! if your tree has nodes with more than 1000 child
--    nodes, you will need to change the call to substr()
--    to include more digits, or the nodes will not be
--    sorted correctly.
create view tree_crumbs as
  select tree, target, group_concat(crumb, ':') as crumbs
  from (
    select tp.tree, tp.below as target, substr('0000'||seq, -3) as crumb
    from tree_path tp
      left join tree_data td on (tp.tree=td.tree and tp.above=td.child)
      left join node n on (above=n.nid)
    order by tp.tree, target, steps desc )
  group by tree, target;

-- a view to give you the depth of any node
create view tree_depth as
  select tree, below as nid, max(steps) as depth
  from tree_path
  group by tree, below;

-- this combines tree_crumbs with depth, node and type data
-- so you can just select from this table and get
-- everything you need for a walk of the tree.
create view tree_walk as
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

-- a view to give you the leaves of a tree
create view tree_leaf as
  select tree, above as leaf
  from tree_path
  group by tree, above
  having count(below) = 1;

-- and the 'roots' (or rather all top-level nodes)
create view tree_root as
  select tree, below as root
  from tree_path
  group by tree, below
  having count(above) = 1;


create table outline_master (
  olid integer primary key,
  tree  integer references trees
);

create table outline_collapse (
  olid integer references outline_master,
  collapse integer references node
);

create view outline_hidden as
  select olid, below as hide from outline_collapse oc
  inner join tree_path tp on oc.collapse=tp.above
  where tp.steps <> 0;

create view outline as
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
insert into trees values (-1);              -- kinds
insert into outline_master values (-1,-1);
insert into tree_data (tree, parent, child, seq) values
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

------------------------------------------------------
-- amoeba-style graph database
------------------------------------------------------
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
  knd integer references kind,
  x   integer,
  y   integer,
  val integer
);

------------------------------------------------------------------
-- help / docs table
------------------------------------------------------------------
create table db_meta (name string unique, purpose string);
create view tables as
  select master.name, master.type, meta.purpose
  from sqlite_master as master
    left natural join db_meta as meta
  where type in ('table', 'view');
insert into db_meta (name, purpose) values
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
