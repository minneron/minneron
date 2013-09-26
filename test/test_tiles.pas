{$mode delphi}
{$i test_tiles.def }
implementation uses tiles;

var
  tile : ITextTile;

procedure test_TextTile;
  begin
    tile := TTextTile.Create( 64, 32 );
    chk.equal( tile.w, 64 );
    chk.equal( tile.h, 32 );
  end;

begin
end.
