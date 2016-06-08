type pos = ErrorMsg.pos
type svalue = Tokens.svalue
type ('svalue,'pos) token = ('svalue,'pos) Tokens.token
type lexresult  = (svalue,ErrorMsg.pos) token

val newLine = ErrorMsg.newLine
val ctr = ref (0);

fun make_pos (yypos,yytext) : ErrorMsg.pos2
    = (yypos, yypos + String.size(yytext) - 1)

(* Handling EOF.  Note that this function reports the wrong file-position for
   end-of-file.  Because of a design infelicity of ML-Lex, it's possible but
   not easy to get access to the correct file position.  There is a way to 
   do it using the %arg feature of ML-Lex, but you don't need to bother 
   with it for this exercise. 
*)
fun eof () = 
	if !ctr = 0 
	then Tokens.EOF(0,0)
	else (ErrorMsg.error ((0,0), ("Syntax Error: No Matching Close Comment")); Tokens.EOF(0,0))
%%

%s COMMENT;
%header (functor FunLexFun(structure Tokens: Fun_TOKENS));

alpha = [A-Za-z];
digit = [0-9];

%%

<INITIAL>fun  					=> (Tokens.FUN(make_pos(yypos,yytext)));
<INITIAL>\n   					=> (newLine yypos; continue ());
<INITIAL>"->"  			       	        => (Tokens.ARROW(make_pos(yypos,yytext)));
<INITIAL>in  					=> (Tokens.IN(make_pos(yypos,yytext)));
<INITIAL>let    	        		=> (Tokens.LET(make_pos(yypos,yytext)));
<INITIAL>then   				=> (Tokens.THEN(make_pos(yypos,yytext)));
<INITIAL>else  					=> (Tokens.ELSE(make_pos(yypos,yytext)));
<INITIAL>if     				=> (Tokens.IF(make_pos(yypos,yytext)));
<INITIAL>":="   				=> (Tokens.ASSIGN(make_pos(yypos,yytext)));
<INITIAL>"!"    				=> (Tokens.BANG(make_pos(yypos,yytext)));
<INITIAL>ref    				=> (Tokens.REF(make_pos(yypos,yytext)));
<INITIAL>do     				=> (Tokens.DO(make_pos(yypos,yytext)));
<INITIAL>while 					=> (Tokens.WHILE(make_pos(yypos,yytext)));
<INITIAL>"||" 					=> (Tokens.OR(make_pos(yypos,yytext)));
<INITIAL>not    				=> (Tokens.NOT(make_pos(yypos,yytext)));
<INITIAL>"&"   					=> (Tokens.AND(make_pos(yypos,yytext)));
<INITIAL>">"   					=> (Tokens.GT(make_pos(yypos,yytext)));
<INITIAL>"="    				=> (Tokens.EQ(make_pos(yypos,yytext)));
<INITIAL>"<"    				=> (Tokens.LT(make_pos(yypos,yytext)));
<INITIAL>"*"    				=> (Tokens.TIMES(make_pos(yypos,yytext)));
<INITIAL>"-"    				=> (Tokens.MINUS(make_pos(yypos,yytext)));
<INITIAL>"+"    				=> (Tokens.PLUS(make_pos(yypos,yytext)));
<INITIAL>"("    				=> (Tokens.LPAREN(make_pos(yypos,yytext)));
<INITIAL>")"    				=> (Tokens.RPAREN(make_pos(yypos,yytext)));
<INITIAL>":"    				=> (Tokens.COLON(make_pos(yypos,yytext)));
<INITIAL>";"    				=> (Tokens.SEMICOLON(make_pos(yypos,yytext)));
<INITIAL>","    				=> (Tokens.COMMA(make_pos(yypos,yytext)));
<INITIAL>type				        => (ErrorMsg.error (make_pos(yypos, yytext), 
							("Syntax Error: Invalid Character: type\n type is a reserved word ")); continue());  
<INITIAL>{alpha}({alpha}|{digit}|"_")*  	=> (Tokens.ID(yytext,yypos,yypos + String.size(yytext) - 1));
<INITIAL>#((0|[1-9]){digit}*)		        => (Tokens.PROJ(Option.getOpt(Int.fromString(String.extract(yytext,1,NONE)),0),yypos,yypos + String.size(yytext) - 1));
<INITIAL>{digit}*			        => (Tokens.INT(Option.getOpt(Int.fromString(yytext),0),yypos,yypos + String.size(yytext) - 1));
<INITIAL>EOF                                    => (Tokens.EOF(make_pos(yypos,yytext)));
<INITIAL>"/*"				        => (ctr := 1; YYBEGIN COMMENT; continue());
<COMMENT>"/*"				        => (ctr := !ctr + 1; continue());
<COMMENT>"*/"				        => (ctr := !ctr - 1; if !ctr = 0 then YYBEGIN INITIAL else (); continue());	
<COMMENT>\n   				        => (newLine yypos; continue ());
<COMMENT>.		 		        => (continue());
<INITIAL>[\ \t]+ 			        => (continue());
<INITIAL>.				        => (ErrorMsg.error (make_pos(yypos, yytext), ("Syntax Error: Invalid Character: " ^ 								yytext)); continue());  




