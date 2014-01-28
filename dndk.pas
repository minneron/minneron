{$i xpc.inc}{$mode delphi}{$H+}
unit dndk; interface type
  { forward declarations since these refer to each other. }
  INode=interface; IEdge=interface; ICell=interface;
  TEdges = array of IEdge;
  IBase	= interface
    function e(sub, rel, obj : string) : IEdge;   // store edge
    function f(eid : integer) : IEdge;            // fetch edge
    function q(sub,rel,obj : string) : TEdges;    // query edges
    function n(key : string) : INode;             // node name -> nid
    function a(key, val : string) : IEdge;        // assign = e(key,':=',val)
    function v(key : string) : ICell;             // v(key) = n(key).val
    property edges[i : integer] : IEdge read f;
    property nodes[s : string] : INode read n; default;
  end;
  ICell	= interface { these cast cells (raw values) to various types }
    function s : string;
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
    function qe(s : String) : TEdges;             // query edges
    function q1(s : String) : ICell;              // q1(s) = qe(s)[0].val
    property any[s : string] : ICell  read q1; default;
    property all[s : string] : TEdges read qe;
  end;
  IEdge	= interface
    function eid : integer;                       // database row id for edge
    function sub : ICell;                         // subject
    function rel : ICell;                         // relation
    function obj : ICell;                         // object
  end;
implementation
end.
