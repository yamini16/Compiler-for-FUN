signature TYPECHECK =
sig
  val tc : Absyn.prog -> unit
  (* if there are errors, these are reported through ErrorMsg.error *)

  val sub: Absyn.tp * Absyn.tp -> bool
  val join: (string->unit) -> Absyn.tp * Absyn.tp -> Absyn.tp
end

structure TypeCheck :> TYPECHECK =
struct

 structure A = Absyn
 structure F = FunPP

 exception WarningCatch
 exception FunctionNameError
 exception SymbolTableError
 exception CallError
     
 fun list2string nil = ""
   | list2string [t] = t
   | list2string (h::t) = h ^ "," ^ list2string t

 fun tp2string A.Inttp = "int"
   | tp2string (A.Tupletp tps) = "<" ^ (list2string (map tp2string tps)) ^ ">"
   | tp2string (A.Arrowtp (tp1, tp2)) = tp2string tp1 ^ " -> " ^ tp2string tp2
   | tp2string (A.Reftp tp) = tp2string tp ^ " ref"
   (*| tp2string _ = raise WarningCatch *)


 type context = A.tp Symbol.table

 (* subtyping *)

 fun rec_sub (t1,t2) =                    (* recursive subtype check *) 
 case (t1,t2) 
      of (x,[]) => true
      |(x::xs,y::ys)  => (sub (x,y) andalso rec_sub(xs,ys))  
      
      

 and sub (t1,t2) = 
     if t1 = t2 
        then true 
        else case (t1, t2) 
             of (A.Tupletp(xlist), A.Tupletp(ylist)) => if (length (xlist) < length (ylist)) then false else rec_sub(xlist,ylist)
							
		   															                 |(A.Reftp(x),A.Reftp (y)) => sub(x,y)
	     																		 |(A.Arrowtp(a,b),A.Arrowtp(a', b')) => sub(a', a) andalso sub(b,b') 
																		         |_ =>  false  


 fun check_sub pos (tp1, tp2) =                      
   if sub (tp1, tp2) then ()
   else ErrorMsg.error (pos, ("Type Error: Incompatible types " ^ tp2string tp1 ^ " and " ^  tp2string tp2)) 



 fun flashError (e:A.exp, treq:A.tp list, tgiven:A.tp list, pos, s) =  (* Flash error and erroneous expression along with required value 
                                                                          types *)
    ( ErrorMsg.error(pos, ("Type Error: " ^ s ^ " in ")); 
      F.print_exp (e); print "\nRequired: ";
      map (fn s => print (s ^ "  ")) (map tp2string treq);print "\nGiven: ";
      map (fn s => print (s ^ "  ")) (map tp2string tgiven); 
      print "\n"; 
      ())   

(* subtype join *)
 fun join complain (t1,t2) : A.tp =
     case (t1, t2) 
	  of (A.Arrowtp (a,b) , A.Arrowtp(a',b')) => A.Arrowtp( meet complain  (a,a'), join complain (b,b') )
          |_ => if sub(t1, t2) 
		then t2 
		else
                     if sub (t2,t1) 
		     then t1 
		     else
			 
			 case (t1,t2) 
				  of (A.Tupletp(x),A.Tupletp(y)) => let val (t1',t2') = if (length x = Int.min(length x, length y)) 
										then (A.Tupletp(List.take (x, length (x) -1)),
										      A.Tupletp(List.take (y, length (x) -1)))
										
										else (A.Tupletp(List.take (x, length (y) -1)),
										      A.Tupletp(List.take (y, length (y) -1)))

								     in
									 join complain (t1',t2')
								     end
				  |_  => (complain (" Type Mismatch "); t1 )         (* Return t1 always if types are not compatible *)                 

 and meet complain (t1,t2) = 
     if t1 = t2 
     then t1 
     else 
	 if sub(t1,t2) 
	 then t1 
	 else (if sub(t2,t1) 
	       then t2 
	       else (complain (" Incompatible Types ")  ; t1 ))                     (* Return t1 always if types are not compatible *)



 fun mylook (ctxt, id) =                                                (* Symbol Table Lookup *)
     case Symbol.look(ctxt, id)
	  of NONE => raise SymbolTableError
          |SOME s =>  s 

 fun arg (n) =
     if n = 0 then [] else A.Inttp::arg(n-1) 

(* expression typing *)
 fun tc_exp ctxt pos e: A.tp = 
     case e
	  of A.Int (i) => A.Inttp
	  |A.Id (id) => mylook(ctxt,id)
	  |A.Op (oper, elist) => (case (oper, elist) 
				     of (A.Add,[e1,e2]) => let val e1' = tc_exp ctxt pos e1
							       val e2' = tc_exp ctxt pos e2
							   in
							       if e1' = e2' andalso e1' =  A.Inttp 
							       then A.Inttp 
							       else (flashError (A.Op (oper, elist),[A.Inttp,A.Inttp],[e1',e2'],pos,
										 "Case A.Add: Operator and operand don't agree "); A.Inttp)
							   end
                                     | (A.Sub,[e1,e2]) => let val e1' = tc_exp ctxt pos e1
							      val e2' = tc_exp ctxt pos e2
							   in
							       if e1' = e2' andalso e1' = A.Inttp 
							       then A.Inttp 
							       else (flashError (A.Op (oper, elist),[A.Inttp,A.Inttp],[e1',e2'],pos,
										 "Case A.Sub: Operator and operand don't agree "); A.Inttp)
							   end
                                     | (A.Mul,[e1,e2]) => let val e1' = tc_exp ctxt pos e1
							      val e2' = tc_exp ctxt pos e2
							   in
							       if e1' = e2' andalso e1' =  A.Inttp 
							       then A.Inttp 
							       else (flashError (A.Op (oper, elist),[A.Inttp,A.Inttp],[e1',e2'],pos,
										 "Case A.Mul: Operator and operand don't agree "); A.Inttp)
							   end     
                                     | (A.LT,[e1,e2]) => let val e1' = tc_exp ctxt pos e1
							     val e2' = tc_exp ctxt pos e2
							   in
							       if e1' = e2' andalso e1' =  A.Inttp 
							       then A.Inttp 
							       else (flashError (A.Op (oper, elist),[A.Inttp,A.Inttp],[e1',e2'],pos,
										 "Case A.LT: Operator and operand don't agree "); A.Inttp)
							   end       
                                     | (A.Eq,[e1,e2]) => let val e1' = tc_exp ctxt pos e1
							     val e2' = tc_exp ctxt pos e2
							   in
							       if e1' = e2' andalso e1' = A.Inttp 
							       then A.Inttp 
							       else (flashError (A.Op (oper, elist),[A.Inttp,A.Inttp],[e1',e2'],pos,
										 "Case A.Eq: Operator and operand don't agree "); A.Inttp)
							   end      
                                     | (A.Ref, [e]) => A.Reftp(tc_exp ctxt pos e)      
                                     | (A.Get, [e]) => let val e' =  (tc_exp ctxt pos e) 
					               in
							   case e'
								of A.Reftp(x) => x 
								| _ => (flashError (A.Op (oper, elist),[A.Reftp A.Inttp],[e'],pos,
										    "Case A.get: Operator and operand don't agree "); A.Inttp)
									   (* Consider int ref if object of get is not refttp *)
						       end
                                     | (A.Set, [id,e]) => let val id' = tc_exp ctxt pos id  
							      val e' = tc_exp ctxt pos e
							  in
							      case id'
								   of A.Reftp (x) => if x = e' 
										     then (A.Tupletp [])
										     else (flashError (A.Op (oper, elist),[x],[e'],pos,
												       "A.Set: Operator and operand don't agree ");                                                                                                        e')
								   | _ => (flashError (A.Op (oper, elist),[A.Reftp A.Inttp],[id'],pos,
										       "Case A.Set_Else_case: Operator and operand don't agree "); 
									               e') 
							  end
				    |_  => raise WarningCatch )
							  
				   

          |A.Tuple (elist) => A.Tupletp(map (tc_exp ctxt pos) elist)     

	  |A.Proj (num, exp) => let
                                   val exp' = tc_exp ctxt pos exp
				in
				   (case exp'
                                         of A.Tupletp (x::xs) => if num < (length (x::xs)) then List.nth ((x::xs),num) 
								 else
								      let val x = arg(num+1) 
								      in
								         (flashError (A.Proj (num, exp),[A.Tupletp x],[exp'],pos, 
										      "Tuple does not have that many elements"); A.Inttp) 
								      end
					  |_  => (flashError (A.Proj (num, exp),[A.Tupletp[A.Inttp]],[exp'],pos, 
							      "Projection can only be applied to Tuple Type elements"); A.Inttp) )
							    
				end
				       
	  |A.If (e1,e2,e3) => let val e1' = (tc_exp ctxt pos e1)
				  val e2' = tc_exp ctxt pos e2
				  val e3' = tc_exp ctxt pos e3
			      in
				  case e1'
			            of A.Inttp => join (fn s => flashError(A.If (e1,e2,e3),[A.Inttp,A.Inttp],[e2',e3'],pos,s)) (e2', e3')
						  
				    | _ => (flashError(A.If (e1,e2,e3),[A.Inttp],[e1'],pos,"Condition check must evaluate to an integer ");
					   join (fn s => flashError(A.If (e1,e2,e3),[A.Inttp,A.Inttp],[e2',e3'],pos,s)) (e2', e3'))
			     end

	  |A.While (e1,e2) => let val e1'= (tc_exp ctxt pos e1) 
			      in
				  case e1'
			           of A.Inttp => (tc_exp ctxt pos e2)
				   | _ => (flashError (A.While (e1,e2),[A.Inttp],[e1'],pos,"Case A.While: Operator and operand don't agree "); A.Inttp)
 			       end
	  |A.Call(e1,e2) => let val e1' = tc_exp ctxt pos e1
				val e2' = tc_exp ctxt pos e2
			    in
				case e1'
				 of A.Arrowtp (tp1,tp2) => (if sub (e2',tp1) 
							    then tp2 
							    else(flashError (A.Call (e1,e2),[tp1],[e2'],pos,
									     "Case A.Call: Incompatible types for function call "); 
								              tp2) )   (* Return expected return type *)
				 | _  => raise CallError
			    end				     
	  |A.Let (id, e1, e2) => let 
	                                   val e1' = tc_exp ctxt pos e1
		                           val ctxt' = Symbol.enter(ctxt,id,e1')
                                           val e2' = tc_exp ctxt' pos e2
		                       in
					     e2'
				       end
	  |A.Constrain(e, tp) => if sub((tc_exp ctxt pos e), tp) 
				 then tp 
				 else (flashError (A.Constrain(e, tp),[tp],[tc_exp ctxt pos e],pos,
						   "Case A.Constraints: Incompatible types for expresion constrain "); 
				                    tp) (* Return expected type *)
	  |A.Pos (pos2, e) => tc_exp ctxt pos2 e



 
 fun tc_fundec ctxt ((pos, (f, x, tp1, tp2, exp)): A.fundec) =
     let val A.Arrowtp(para_type, ret_tp) = Option.valOf(Symbol.look(ctxt,f))
	 val ctxt' = Symbol.enter(ctxt,x,para_type)
	 val tp = tc_exp ctxt' pos exp
                                                                      (* For main: Return type and parameter type should always be int *)
     in  if sub (tp, ret_tp) 
	 then ()  
	 else flashError (exp,[ret_tp],[para_type],pos,"Incompatible types ")
     end 

 fun do_another_fun ((pos, fdec), ctxt) = 
     let 
         fun check ((f, x, tp1, tp2, exp),ctxt) =
	     case Symbol.look(ctxt, f) 
	      of NONE => if (Symbol.name(f) = "main") 
			 then (if (tp1 = tp2 andalso tp1 = A.Inttp) 
			       then Symbol.enter(ctxt,f,A.Arrowtp(tp1,tp2)) 
			       else (ErrorMsg.error(pos, ("Type Error: Main should be of type int -> int " ^ " in ")); 
				     F.print_func (fdec); print "\nRequired: int -> int" ;
				     print "\nGiven: ";
				     map (fn s => print (s ^ "  ")) (map tp2string [tp1,tp2]); 
				     print "\n";Symbol.enter(ctxt,f,A.Arrowtp(A.Inttp,A.Inttp))))  
			             (* Assume parameter and return type of main to be int type *) 
			 else Symbol.enter(ctxt,f,A.Arrowtp(tp1,tp2))
			 
                 |_ => (ErrorMsg.error(pos, ("Type Error: Function name already exists " ^ " in ")); 
			F.print_func (fdec);
			raise FunctionNameError )
        
     in
         check  (fdec,ctxt)
     end   

 fun build_global_context (fundecs) =
  foldl do_another_fun (Symbol.enter(Symbol.empty,Symbol.symbol("printint"),A.Arrowtp(A.Inttp,A.Tupletp[]))) fundecs                              (* add printint *)

 fun tc (fundecs : A.prog)  = 
  let val ctxt = build_global_context(fundecs) 
   in app (tc_fundec ctxt) fundecs
  end
end
