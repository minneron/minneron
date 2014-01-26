-- for compatability with legacy python version: ---
create view if not exists statement as select * from edge;
create view if not exists triples as select * from trip;
