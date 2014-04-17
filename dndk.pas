{$i xpc.inc}{$mode delphi}{$H+}
unit dndk; interface uses xpc;
type  { forward declarations since these refer to each other. }
  INode=interface; IEdge=interface; ICell=interface;
  TEdges = array of IEdge;
  IBase	= interface
    function e(sub, rel, obj : TStr; seq:integer=0) : IEdge;   // store edge
    function f(eid : integer) : IEdge;            // fetch edge
    function q(sub,rel,obj : TStr) : TEdges;    // query edges
    function w(sub,rel,obj : TStr) : TEdges;    // write edges (to debug)
    function n(key : TStr) : INode;             // node name -> nid
    function a(key, val : TStr) : IEdge;        // assign = e(key,':=',val)
    function v(key : TStr) : ICell;             // v(key) = n(key).val
    property edges[i : integer] : IEdge read f;
    property nodes[s : TStr] : INode read n; default;
  end;
  ICell	= interface { these cast cells (raw values) to various types }
    function s : TStr;
    function i : integer;
    function b : boolean;
    function n : INode;
  end;
  INode	= interface
    function nid : integer;                       // database row id for node
    function key : ICell;
    function val : ICell;
    function ie : TEdges;                         // incoming edges
    function oe : TEdges;                         // outgoing edges
    function qe(s : TStr) : TEdges;             // query edges
    function q1(s : TStr) : ICell;              // q1(s) = qe(s)[0].val
    property any[s : TStr] : ICell  read q1; default;
    property all[s : TStr] : TEdges read qe;
  end;
  IEdge	= interface
    function eid : integer;                       // database row id for edge
    function sub : ICell;                         // subject
    function rel : ICell;                         // relation
    function obj : ICell;                         // object
    function GetSeq : integer;                    // sequence
    procedure SetSeq(val : integer);
    procedure del;
    property seq : integer read GetSeq write SetSeq;
  end;
implementation
end.
