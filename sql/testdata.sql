insert into node (nid, knd, val) values
  (1,    -3, 'a'),
  (2,    -3, 'b'),
  (11,   -3, 'aa'),
  (111,  -3, 'aaa'),
  (1111, -3, 'aaaa'),
  (12,   -3, 'ab'),
  (13,   -3, 'ac'),
  (21,   -3, 'ba'),
  (22,   -3, 'bb'),
  (23,   -3, 'bc');

insert into trees values (1);

insert into tree_data
  (tree, parent, child, seq) values
  (   1,      0,     1,   1),
  (   1,      0,     2,   2),
  (   1,      1,    11,   1),
  (   1,     11,   111,   1),
  (   1,    111,  1111,   1),
  (   1,      1,    12,   2),
  (   1,      1,    13,   3),
  (   1,      2,    21,   1),
  (   1,      2,    22,   2),
  (   1,      2,    23,   3);

insert into outline_master values (1,1);
insert into outline_collapse values (1,11);
