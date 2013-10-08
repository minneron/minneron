-- these three tables should be enough to track basic outlines.

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
     (  -1, -1, 'type'),
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

-- TODO: auto-maintain the kind table with a trigger
insert into kind (knd)
  select nid from node where knd=-1;

pragma foreign_keys=1;

-- tree_data contains the core data for ordered trees.
create table tree_data (
  tree   integer references node,  -- a node can appear in multiple trees
  parent integer references node,
  child  integer references node,
  seq    integer );

-- tree_path contains automatically generated information
-- about each node's full subtree.
create table tree_path (
  tree   integer references node,
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
  when not exists (select * from flags where flag='recursive-tree-delete')
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

create view tree_roots as
  select tree, below as root
  from tree_path
  group by tree, below
  having count(above) = 1;

create view tree_leaf as
  select tree, above as leaf
  from tree_path
  group by tree, above
  having count(below) = 1;

create view tree_depth as
  select tree, below as nid, max(steps) as depth
  from tree_path
  group by tree, below;

-- a depth first walk of the tree. (sort of)
-- 'leaf' indicates the leaf we're walking toward.
-- each time it changes, go back to the top of the tree,
-- so anything that isn't a leaf may be visited
-- multiple times.
create view tree_walk as
  select tl.tree, leaf, n.nid, k.val as kind, n.val as data
  from tree_leaf tl
    left join tree_path tp on (tl.tree=tp.tree and tl.leaf=tp.below)
    left join node n on (above=n.nid)
    left join node k on (n.knd=k.nid)
  order by tp.below, steps desc;

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
