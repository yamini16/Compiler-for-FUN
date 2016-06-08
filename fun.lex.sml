functor FunLexFun(structure Tokens: Fun_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
COMMENT | INITIAL
    structure UserDeclarations = 
      struct

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


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.FUN(make_pos(yypos,yytext)))
      end
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newLine yypos; continue ()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ARROW(make_pos(yypos,yytext)))
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.IN(make_pos(yypos,yytext)))
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LET(make_pos(yypos,yytext)))
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.THEN(make_pos(yypos,yytext)))
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ELSE(make_pos(yypos,yytext)))
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.IF(make_pos(yypos,yytext)))
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ASSIGN(make_pos(yypos,yytext)))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.BANG(make_pos(yypos,yytext)))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.REF(make_pos(yypos,yytext)))
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.DO(make_pos(yypos,yytext)))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.WHILE(make_pos(yypos,yytext)))
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.OR(make_pos(yypos,yytext)))
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.NOT(make_pos(yypos,yytext)))
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.AND(make_pos(yypos,yytext)))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.GT(make_pos(yypos,yytext)))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.EQ(make_pos(yypos,yytext)))
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LT(make_pos(yypos,yytext)))
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.TIMES(make_pos(yypos,yytext)))
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.MINUS(make_pos(yypos,yytext)))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.PLUS(make_pos(yypos,yytext)))
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LPAREN(make_pos(yypos,yytext)))
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RPAREN(make_pos(yypos,yytext)))
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.COLON(make_pos(yypos,yytext)))
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.SEMICOLON(make_pos(yypos,yytext)))
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.COMMA(make_pos(yypos,yytext)))
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.error (make_pos(yypos, yytext), 
							("Syntax Error: Invalid Character: type\n type is a reserved word ")); continue())
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.ID(yytext,yypos,yypos + String.size(yytext) - 1))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.PROJ(Option.getOpt(Int.fromString(String.extract(yytext,1,NONE)),0),yypos,yypos + String.size(yytext) - 1))
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT(Option.getOpt(Int.fromString(yytext),0),yypos,yypos + String.size(yytext) - 1))
      end
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.EOF(make_pos(yypos,yytext)))
      end
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (ctr := 1; YYBEGIN COMMENT; continue()))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (ctr := !ctr + 1; continue()))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (ctr := !ctr - 1; if !ctr = 0 then YYBEGIN INITIAL else (); continue()))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (newLine yypos; continue ()))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.error (make_pos(yypos, yytext), ("Syntax Error: Invalid Character: " ^ 								yytext)); continue())
      end
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ39(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction28(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = #"`"
              then yyAction28(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction12(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = #"`"
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ44(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ43(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ42(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"h"
              then yyQ41(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"h"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction27(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction27(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction27(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp = #"`"
              then yyAction27(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ48(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ47(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction5(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction5(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp = #"`"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ50(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ49(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction28(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"y"
              then yyQ46(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"y"
              then if inp = #"h"
                  then yyQ45(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction10(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction10(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = #"`"
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ52(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ51(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction14(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction14(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = #"`"
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ54(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ53(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"`"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ56(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ55(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction3(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"`"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = #"`"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction28(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction28(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ58(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"f"
                  then yyQ57(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction0(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"`"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ60(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ59(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ63(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ62(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ61(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction11(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction11(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = #"`"
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ64(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction28(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = #"`"
              then yyAction28(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"G"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"G"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp = #"F"
                  then yyQ66(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = #"`"
              then yyAction28(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"P"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"P"
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp = #"O"
                  then yyQ65(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = #"`"
              then yyAction28(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction28(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction28(strm, yyNO_MATCH)
                      else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = #"`"
              then yyAction28(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ40(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ67(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ68(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"0"
              then yyAction30(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ68(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ68(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"0"
              then yyAction30(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ68(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ69(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ70(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ71(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"0"
              then yyAction29(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ71(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ71(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"0"
              then yyAction38(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ71(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ72(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ72(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ72(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ72(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"?"
              then yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"?"
              then if inp = #")"
                  then yyQ15(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp < #")"
                  then if inp = #"\""
                      then yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                    else if inp < #"\""
                      then if inp = #"\v"
                          then yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                        else if inp < #"\v"
                          then if inp = #"\t"
                              then yyQ9(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                            else if inp = #"\n"
                              then yyQ10(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                              else yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                        else if inp = #" "
                          then yyQ9(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                        else if inp = #"!"
                          then yyQ11(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                          else yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                    else if inp = #"&"
                      then yyQ13(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                    else if inp < #"&"
                      then if inp = #"#"
                          then yyQ12(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                          else yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                    else if inp = #"'"
                      then yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                      else yyQ14(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"-"
                      then yyQ19(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                    else if inp < #"-"
                      then if inp = #"+"
                          then yyQ17(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                        else if inp = #"*"
                          then yyQ16(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                          else yyQ18(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                    else if inp = #"."
                      then yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                      else yyQ20(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp = #"<"
                  then yyQ24(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp < #"<"
                  then if inp = #":"
                      then yyQ22(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                    else if inp = #";"
                      then yyQ23(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                      else yyQ21(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp = #"="
                  then yyQ25(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyQ26(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp = #"l"
              then yyQ33(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"d"
                  then yyQ29(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp < #"d"
                  then if inp = #"F"
                      then yyQ27(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                    else if inp < #"F"
                      then if inp = #"A"
                          then yyQ27(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                        else if inp < #"A"
                          then yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                        else if inp = #"E"
                          then yyQ28(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                          else yyQ27(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                    else if inp = #"["
                      then yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                    else if inp < #"["
                      then yyQ27(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                    else if inp <= #"`"
                      then yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                      else yyQ27(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp = #"g"
                  then yyQ27(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp < #"g"
                  then if inp = #"e"
                      then yyQ30(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                      else yyQ31(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp = #"i"
                  then yyQ32(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyQ27(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp = #"u"
              then yyQ27(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"r"
                  then yyQ35(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp < #"r"
                  then if inp = #"n"
                      then yyQ34(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                      else yyQ27(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp = #"s"
                  then yyQ27(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp = #"{"
              then yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"{"
              then if inp = #"w"
                  then yyQ37(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyQ27(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp = #"|"
              then yyQ38(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyQ8(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ6(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ7(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ4(strm', lastMatch)
            else if inp < #"*"
              then if inp = #"\n"
                  then yyQ3(strm', lastMatch)
                  else yyQ2(strm', lastMatch)
            else if inp = #"/"
              then yyQ5(strm', lastMatch)
              else yyQ2(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COMMENT => yyQ0(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ1(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
