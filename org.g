grammar org;

org 
    : block*
    ;

block
    : headline
    | metaline
    | comment 
    | paragraph
    | verbatim 
    | hrule 
    | text
    ;

headline
    : '*'+ progress? keyword? markup tags? NL
    ;

metaline 
    : '#+'
    ;

comment 
    : '#' .* '\n'
    ;

paragraph
    : verse 
    | quote 
    | center
    ;

verbatim
    : ':' .* '\n'
    ;

markup
    : ENTITY
    | ANYTHING
    ;


fragment Upper : 'A' .. 'Z' ;
fragment Lower : 'a' .. 'z' ;
fragment Alpha : Upper | Lower ;

ENTITY : '\\' Alpha+ ;
