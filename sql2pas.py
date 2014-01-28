# this is a quick hack to generate a pascal include file
# from the database schema for nodak. i will probably
# clean it up and use it for minneron too when i get to
# that point.
#
# usage:
# python sql2pas.py > .gen/nodak-sql2pas.inc
import os

nospace = lambda s: ' '.join(s.split())

# the file is split with  (ascii file separator control code)
# but these are commented out to appease sqlite's parser.
for i,q in enumerate(open('sql/nodak.sql').read().split('--')):
    lines = [nospace(line) for line in q.split("\n")
             if not line.startswith('--')]
    sql = nospace(' '.join(lines)
        .replace("'","''")) # strip quotes for pascal

    # and now the pascal syntax:
    print "_dbc.RunSQL('{0}',[]);".format(sql)
