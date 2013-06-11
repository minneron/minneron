{$mode objfpc}
unit org;
interface uses classes, regexpr;

  type
    priority = ( priA, priB, priC );
    date     = string; //  for now
    item     = class
		 depth	    : integer;
		 headline,
		 todo	    : string;
		 pri	    : priority;
		 body, tags : tstringlist;
		 scheduled,
		 deadline   : date;
		 children   : array of item;
	       end;

  function parse( path : string ) : item;
  function walk( itm : item )

implementation

  var
    match : array of string;

   '^(\*+)\s(.*?)\s*$'
  function re( pat, src : string; groups : tstringlist ) : boolean;
    var p, s : integer; undecided : boolean = true; pch, sch : char;
  begin
    result := false; p := 1; s := 1;
    while undecided and ( p < length( pat )) and ( s < length( src )) do
    begin
      pch := pat[ p ]; sch := src[ s ];
      case pch of
	'^' : match_bol;
	'$' : match_eol;
	'\' : special;
	'(' : group;
	'.' : match_wild;
	'+' : wrap_kplus;
	'*' : wrap_kstar;
	'?' : wrap_kques;
	'!' : wrap_kdrop;
      end
    end
  end;


  function parse( path ) : item;
    var this : item; depth : integer;
  begin
    result := item.create;
    re := TRegExpr.create;
  end;

initialization
  re_head := TRegExpr.create( '^(\*+)\s(.*?)\s*$' );
end.
