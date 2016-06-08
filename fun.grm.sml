functor FunLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Fun_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
val ctr = ref (0);
structure A = Absyn


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\120\000\004\000\120\000\005\000\120\000\006\000\120\000\
\\007\000\053\000\008\000\120\000\010\000\120\000\011\000\120\000\
\\013\000\120\000\014\000\120\000\016\000\120\000\019\000\120\000\
\\021\000\120\000\023\000\120\000\024\000\120\000\028\000\047\000\
\\029\000\046\000\030\000\045\000\000\000\
\\001\000\001\000\121\000\004\000\121\000\005\000\121\000\006\000\121\000\
\\007\000\053\000\008\000\121\000\010\000\121\000\011\000\121\000\
\\013\000\121\000\014\000\121\000\016\000\121\000\019\000\121\000\
\\021\000\121\000\023\000\121\000\024\000\121\000\028\000\047\000\
\\029\000\046\000\030\000\045\000\000\000\
\\001\000\001\000\122\000\004\000\122\000\005\000\122\000\006\000\054\000\
\\007\000\053\000\008\000\122\000\010\000\122\000\011\000\122\000\
\\013\000\122\000\014\000\122\000\019\000\122\000\021\000\051\000\
\\023\000\050\000\024\000\122\000\025\000\049\000\026\000\048\000\
\\028\000\047\000\029\000\046\000\030\000\045\000\000\000\
\\001\000\002\000\044\000\003\000\043\000\007\000\042\000\012\000\041\000\
\\015\000\040\000\017\000\039\000\018\000\038\000\020\000\037\000\
\\022\000\036\000\024\000\060\000\026\000\035\000\027\000\034\000\
\\029\000\033\000\000\000\
\\001\000\002\000\044\000\003\000\043\000\007\000\042\000\012\000\041\000\
\\015\000\040\000\017\000\039\000\018\000\038\000\020\000\037\000\
\\022\000\036\000\026\000\035\000\027\000\034\000\029\000\033\000\000\000\
\\001\000\003\000\008\000\000\000\
\\001\000\003\000\010\000\000\000\
\\001\000\003\000\015\000\007\000\014\000\024\000\021\000\026\000\013\000\000\000\
\\001\000\003\000\015\000\007\000\014\000\026\000\013\000\000\000\
\\001\000\003\000\066\000\000\000\
\\001\000\004\000\026\000\024\000\025\000\000\000\
\\001\000\004\000\080\000\024\000\079\000\000\000\
\\001\000\005\000\055\000\006\000\054\000\007\000\053\000\008\000\084\000\
\\016\000\052\000\021\000\051\000\023\000\050\000\025\000\049\000\
\\026\000\048\000\028\000\047\000\029\000\046\000\030\000\045\000\000\000\
\\001\000\005\000\055\000\006\000\054\000\007\000\053\000\008\000\085\000\
\\016\000\052\000\021\000\051\000\023\000\050\000\025\000\049\000\
\\026\000\048\000\028\000\047\000\029\000\046\000\030\000\045\000\000\000\
\\001\000\005\000\055\000\006\000\054\000\007\000\053\000\011\000\091\000\
\\016\000\052\000\021\000\051\000\023\000\050\000\025\000\049\000\
\\026\000\048\000\028\000\047\000\029\000\046\000\030\000\045\000\000\000\
\\001\000\005\000\055\000\006\000\054\000\007\000\053\000\014\000\082\000\
\\016\000\052\000\021\000\051\000\023\000\050\000\025\000\049\000\
\\026\000\048\000\028\000\047\000\029\000\046\000\030\000\045\000\000\000\
\\001\000\005\000\055\000\006\000\054\000\007\000\053\000\016\000\052\000\
\\019\000\081\000\021\000\051\000\023\000\050\000\025\000\049\000\
\\026\000\048\000\028\000\047\000\029\000\046\000\030\000\045\000\000\000\
\\001\000\006\000\011\000\000\000\
\\001\000\006\000\024\000\000\000\
\\001\000\007\000\009\000\000\000\
\\001\000\008\000\018\000\009\000\017\000\018\000\016\000\000\000\
\\001\000\008\000\027\000\009\000\017\000\018\000\016\000\000\000\
\\001\000\009\000\017\000\018\000\016\000\025\000\030\000\000\000\
\\001\000\010\000\006\000\000\000\
\\001\000\025\000\083\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\010\000\006\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\005\000\055\000\006\000\054\000\007\000\053\000\016\000\052\000\
\\021\000\051\000\023\000\050\000\025\000\049\000\026\000\048\000\
\\028\000\047\000\029\000\046\000\030\000\045\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\018\000\016\000\000\000\
\\106\000\000\000\
\\107\000\009\000\017\000\018\000\016\000\000\000\
\\108\000\009\000\017\000\018\000\016\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\007\000\053\000\000\000\
\\113\000\007\000\053\000\025\000\049\000\026\000\048\000\028\000\047\000\
\\029\000\046\000\030\000\045\000\000\000\
\\114\000\007\000\053\000\000\000\
\\115\000\007\000\053\000\000\000\
\\116\000\007\000\053\000\000\000\
\\117\000\007\000\053\000\000\000\
\\118\000\007\000\053\000\028\000\047\000\000\000\
\\119\000\007\000\053\000\028\000\047\000\000\000\
\\123\000\007\000\053\000\025\000\049\000\026\000\048\000\028\000\047\000\
\\029\000\046\000\030\000\045\000\000\000\
\\124\000\007\000\053\000\025\000\049\000\026\000\048\000\028\000\047\000\
\\029\000\046\000\030\000\045\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\009\000\017\000\018\000\016\000\000\000\
\\129\000\006\000\054\000\007\000\053\000\016\000\052\000\021\000\051\000\
\\023\000\050\000\025\000\049\000\026\000\048\000\028\000\047\000\
\\029\000\046\000\030\000\045\000\000\000\
\\130\000\005\000\055\000\006\000\054\000\007\000\053\000\013\000\090\000\
\\016\000\052\000\021\000\051\000\023\000\050\000\025\000\049\000\
\\026\000\048\000\028\000\047\000\029\000\046\000\030\000\045\000\000\000\
\\131\000\006\000\054\000\007\000\053\000\016\000\052\000\021\000\051\000\
\\023\000\050\000\025\000\049\000\026\000\048\000\028\000\047\000\
\\029\000\046\000\030\000\045\000\000\000\
\\132\000\005\000\055\000\006\000\054\000\007\000\053\000\016\000\052\000\
\\021\000\051\000\023\000\050\000\025\000\049\000\026\000\048\000\
\\028\000\047\000\029\000\046\000\030\000\045\000\000\000\
\\133\000\006\000\054\000\007\000\053\000\016\000\052\000\021\000\051\000\
\\023\000\050\000\025\000\049\000\026\000\048\000\028\000\047\000\
\\029\000\046\000\030\000\045\000\000\000\
\\134\000\005\000\055\000\006\000\054\000\007\000\053\000\016\000\052\000\
\\021\000\051\000\023\000\050\000\025\000\049\000\026\000\048\000\
\\028\000\047\000\029\000\046\000\030\000\045\000\000\000\
\\135\000\005\000\055\000\006\000\054\000\007\000\053\000\016\000\052\000\
\\021\000\051\000\023\000\050\000\025\000\049\000\026\000\048\000\
\\028\000\047\000\029\000\046\000\030\000\045\000\000\000\
\"
val actionRowNumbers =
"\024\000\029\000\026\000\028\000\
\\006\000\027\000\020\000\007\000\
\\018\000\009\000\021\000\008\000\
\\009\000\032\000\037\000\009\000\
\\019\000\011\000\038\000\035\000\
\\022\000\036\000\009\000\034\000\
\\009\000\033\000\023\000\039\000\
\\005\000\030\000\031\000\005\000\
\\005\000\004\000\005\000\005\000\
\\005\000\005\000\005\000\010\000\
\\005\000\041\000\042\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\009\000\005\000\043\000\046\000\
\\012\000\062\000\054\000\044\000\
\\017\000\047\000\045\000\016\000\
\\025\000\013\000\049\000\050\000\
\\048\000\002\000\001\000\051\000\
\\052\000\003\000\014\000\056\000\
\\061\000\053\000\005\000\005\000\
\\005\000\005\000\040\000\055\000\
\\063\000\059\000\058\000\015\000\
\\005\000\005\000\057\000\060\000\
\\000\000"
val gotoT =
"\
\\001\000\092\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\003\000\003\000\005\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\010\000\000\000\
\\000\000\
\\007\000\018\000\009\000\017\000\000\000\
\\007\000\020\000\000\000\
\\000\000\
\\000\000\
\\007\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\026\000\000\000\
\\000\000\
\\007\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\030\000\006\000\029\000\000\000\
\\000\000\
\\000\000\
\\005\000\054\000\000\000\
\\005\000\055\000\000\000\
\\005\000\057\000\008\000\056\000\000\000\
\\005\000\059\000\000\000\
\\005\000\060\000\000\000\
\\005\000\061\000\000\000\
\\005\000\062\000\000\000\
\\005\000\063\000\000\000\
\\000\000\
\\005\000\065\000\000\000\
\\000\000\
\\000\000\
\\005\000\066\000\000\000\
\\005\000\067\000\000\000\
\\005\000\068\000\000\000\
\\005\000\069\000\000\000\
\\005\000\070\000\000\000\
\\005\000\071\000\000\000\
\\005\000\072\000\000\000\
\\005\000\073\000\000\000\
\\005\000\074\000\000\000\
\\007\000\075\000\000\000\
\\005\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\084\000\000\000\
\\005\000\085\000\000\000\
\\005\000\086\000\000\000\
\\005\000\087\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\090\000\000\000\
\\005\000\091\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 93
val numrules = 41
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = ErrorMsg.pos
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | PROJ of unit ->  (int) | ID of unit ->  (string)
 | INT of unit ->  (int) | tplist of unit ->  (A.tp list)
 | explist of unit ->  (A.exp list) | tp of unit ->  (A.tp)
 | posexp of unit ->  (A.exp) | exp of unit ->  (A.exp)
 | func of unit ->  (A.func) | fundecs of unit ->  (A.fundec list)
 | fundec of unit ->  (A.fundec) | prog of unit ->  (A.prog)
end
type svalue = MlyValue.svalue
type result = A.prog
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "INT"
  | (T 2) => "ID"
  | (T 3) => "COMMA"
  | (T 4) => "SEMICOLON"
  | (T 5) => "COLON"
  | (T 6) => "LPAREN"
  | (T 7) => "RPAREN"
  | (T 8) => "ARROW"
  | (T 9) => "FUN"
  | (T 10) => "IN"
  | (T 11) => "LET"
  | (T 12) => "ELSE"
  | (T 13) => "THEN"
  | (T 14) => "IF"
  | (T 15) => "ASSIGN"
  | (T 16) => "BANG"
  | (T 17) => "REF"
  | (T 18) => "DO"
  | (T 19) => "WHILE"
  | (T 20) => "OR"
  | (T 21) => "NOT"
  | (T 22) => "AND"
  | (T 23) => "GT"
  | (T 24) => "EQ"
  | (T 25) => "LT"
  | (T 26) => "PROJ"
  | (T 27) => "TIMES"
  | (T 28) => "MINUS"
  | (T 29) => "PLUS"
  | (T 30) => "CALL"
  | (T 31) => "UMINUS"
  | (T 32) => "ITE"
  | (T 33) => "DW"
  | (T 34) => "LETIN"
  | (T 35) => "IFTHEN"
  | (T 36) => "EX"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.fundecs fundecs1, fundecs1left, 
fundecs1right)) :: rest671)) => let val  result = MlyValue.prog (fn _
 => let val  (fundecs as fundecs1) = fundecs1 ()
 in (fundecs)
end)
 in ( LrTable.NT 0, ( result, fundecs1left, fundecs1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.fundecs fundecs1, _, fundecs1right)) :: ( _,
 ( MlyValue.fundec fundec1, fundec1left, _)) :: rest671)) => let val  
result = MlyValue.fundecs (fn _ => let val  (fundec as fundec1) = 
fundec1 ()
 val  (fundecs as fundecs1) = fundecs1 ()
 in (fundec::fundecs)
end)
 in ( LrTable.NT 2, ( result, fundec1left, fundecs1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.fundecs (fn _ => let val 
 (fundec as fundec1) = fundec1 ()
 in ([fundec])
end)
 in ( LrTable.NT 2, ( result, fundec1left, fundec1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.func func1, (funcleft as func1left), (
funcright as func1right))) :: rest671)) => let val  result = 
MlyValue.fundec (fn _ => let val  (func as func1) = func1 ()
 in ((funcleft,funcright), func)
end)
 in ( LrTable.NT 1, ( result, func1left, func1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.posexp posexp1, _, posexp1right)) :: _ :: (
 _, ( MlyValue.tp tp2, _, _)) :: _ :: _ :: ( _, ( MlyValue.tp tp1, _,
 _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID
 ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  
result = MlyValue.func (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  tp1 = tp1 ()
 val  tp2 = tp2 ()
 val  (posexp as posexp1) = posexp1 ()
 in (Symbol.symbol ID1, Symbol.symbol ID2,tp1,tp2,posexp)
end)
 in ( LrTable.NT 3, ( result, FUN1left, posexp1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp1, (expleft as exp1left), (expright
 as exp1right))) :: rest671)) => let val  result = MlyValue.posexp (fn
 _ => let val  (exp as exp1) = exp1 ()
 in (A.Pos((expleft,expright), exp))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), (IDright as 
ID1right))) :: rest671)) => let val  result = MlyValue.tp (fn _ => let
 val  (ID as ID1) = ID1 ()
 in (
 if ID = "int" 
						then A.Inttp 
						else (ErrorMsg.error (( IDleft, IDright ), "Int Expected") ; A.Inttp)
)
end)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.tp tp1, _, _)
) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.tp (fn _ => let val  (tp as tp1) = tp1 ()
 in (tp)
end)
 in ( LrTable.NT 6, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 8, ( ( _, ( _, _, GT1right)) :: ( _, ( MlyValue.tplist tplist1, _
, _)) :: ( _, ( _, LT1left, _)) :: rest671)) => let val  result = 
MlyValue.tp (fn _ => let val  (tplist as tplist1) = tplist1 ()
 in (A.Tupletp(tplist))
end)
 in ( LrTable.NT 6, ( result, LT1left, GT1right), rest671)
end
|  ( 9, ( ( _, ( _, _, GT1right)) :: ( _, ( _, LT1left, _)) :: rest671
)) => let val  result = MlyValue.tp (fn _ => (A.Tupletp([])))
 in ( LrTable.NT 6, ( result, LT1left, GT1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.tp tp2, _, tp2right)) :: _ :: ( _, ( 
MlyValue.tp tp1, tp1left, _)) :: rest671)) => let val  result = 
MlyValue.tp (fn _ => let val  tp1 = tp1 ()
 val  tp2 = tp2 ()
 in (A.Arrowtp(tp1,tp2))
end)
 in ( LrTable.NT 6, ( result, tp1left, tp2right), rest671)
end
|  ( 11, ( ( _, ( _, _, REF1right)) :: ( _, ( MlyValue.tp tp1, tp1left
, _)) :: rest671)) => let val  result = MlyValue.tp (fn _ => let val 
 (tp as tp1) = tp1 ()
 in (A.Reftp(tp))
end)
 in ( LrTable.NT 6, ( result, tp1left, REF1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.tp tp1, tp1left, tp1right)) :: rest671)) =>
 let val  result = MlyValue.tplist (fn _ => let val  (tp as tp1) = tp1
 ()
 in ([tp])
end)
 in ( LrTable.NT 8, ( result, tp1left, tp1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.tp tp1, _, tp1right)) :: _ :: ( _, ( 
MlyValue.tplist tplist1, tplist1left, _)) :: rest671)) => let val  
result = MlyValue.tplist (fn _ => let val  (tplist as tplist1) = 
tplist1 ()
 val  (tp as tp1) = tp1 ()
 in (tplist@[tp])
end)
 in ( LrTable.NT 8, ( result, tplist1left, tp1right), rest671)
end
|  ( 14, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.Id(Symbol.symbol ID))
end)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.Int(INT))
end)
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (exp as exp1) = exp1 ()
 in (A.Op(A.Sub, [A.Int(0),exp]))
end)
 in ( LrTable.NT 4, ( result, MINUS1left, exp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (A.If(exp, A.Int(0), A.Int(1)))
end)
 in ( LrTable.NT 4, ( result, NOT1left, exp1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
BANG1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (A.Op(A.Get, [exp]))
end)
 in ( LrTable.NT 4, ( result, BANG1left, exp1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( 
MlyValue.PROJ PROJ1, PROJ1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (PROJ as PROJ1) = PROJ1 ()
 val  (exp as exp1) = exp1 ()
 in (A.Proj(PROJ,exp))
end)
 in ( LrTable.NT 4, ( result, PROJ1left, exp1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
REF1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (A.Op(A.Ref, [exp]))
end)
 in ( LrTable.NT 4, ( result, REF1left, exp1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.Mul, [exp1, exp2]))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.Add, [exp1, exp2]))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.Sub, [exp1, exp2]))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.Eq, [exp1, exp2]))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.LT, [exp1, exp2]))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.Set, [exp1, exp2]))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.If(exp1, A.If(exp2, A.Int(1), A.Int(0)), A.Int(0)))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.If(exp1, A.Int(1),  A.If(exp2, A.Int(1), A.Int(0))))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 30, ( ( _, ( _, _, GT1right)) :: ( _, ( MlyValue.explist explist1
, _, _)) :: ( _, ( _, LT1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (explist as explist1) = explist1 ()
 in (A.Tuple(explist))
end)
 in ( LrTable.NT 4, ( result, LT1left, GT1right), rest671)
end
|  ( 31, ( ( _, ( _, _, GT1right)) :: ( _, ( _, LT1left, _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => (A.Tuple([])))
 in ( LrTable.NT 4, ( result, LT1left, GT1right), rest671)
end
|  ( 32, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp2, _,
 _)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Call (exp1,exp2))
end)
 in ( LrTable.NT 4, ( result, exp1left, RPAREN1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.tp tp1, _, tp1right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 val  (tp as tp1) = tp1 ()
 in (A.Constrain(exp,tp))
end)
 in ( LrTable.NT 4, ( result, exp1left, tp1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (A.If(exp1, exp2, exp3))
end)
 in ( LrTable.NT 4, ( result, IF1left, exp3right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.If(exp1, exp2, A.Tuple([])))
end)
 in ( LrTable.NT 4, ( result, IF1left, exp2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.While(exp1, exp2))
end)
 in ( LrTable.NT 4, ( result, WHILE1left, exp2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: (
 _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Let (Symbol.symbol ID,exp1,exp2))
end)
 in ( LrTable.NT 4, ( result, LET1left, exp2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
(ctr:= !ctr+1); 
							A.Let (Symbol.symbol ("$id" ^ (Int.toString(!ctr))),exp1,exp2)
)
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.explist (fn _ => let val  (exp as exp1
) = exp1 ()
 in ([exp])
end)
 in ( LrTable.NT 7, ( result, exp1left, exp1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.explist explist1, explist1left, _)) :: rest671)) => let val  
result = MlyValue.explist (fn _ => let val  (explist as explist1) = 
explist1 ()
 val  (exp as exp1) = exp1 ()
 in (explist@[exp])
end)
 in ( LrTable.NT 7, ( result, explist1left, exp1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Fun_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun BANG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun REF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun PROJ (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.PROJ (fn () => i),p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun ITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun DW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun LETIN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun IFTHEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun EX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
end
end
