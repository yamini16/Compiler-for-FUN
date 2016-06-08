signature CODEGEN = sig
  val codegen : Absyn.prog -> Mips.program
end

structure Codegen :> CODEGEN = 
  struct

    structure A = Absyn
    structure M = Mips
    structure E = ErrorMsg

  local
    (* Last label emitted. *)
    val last_lab = ref (NONE: M.lab option)
    (* List of instructions being generated in the current codeblock.
     * For efficiency, this list is kept in reverse order. *)
    val ilist = ref (nil:M.instruction list)
    (* List of codeblocks generated, in reverse order *)
    val blist = ref (nil:M.codeblock list)
    (* List of functions generated, in reverse order *)
    val flist = ref (nil:M.funcode list)
  in
    (* Here's the protocol for using these functions,
       described as a regular expression:

       init_lists ( (emit_label emit* )+ finish_fun )* finish_prog
    *)

    fun init_lists () = (ilist := nil; blist := nil; flist := nil; 
                         last_lab := NONE)

    fun finish_block () = 
           case (!last_lab, !ilist)
            of (NONE, nil) => ()
             | (NONE, _) => E.impossible "No start label"
             | (SOME lab, il) => 
                  (blist := (lab, rev il) :: (!blist);
                   ilist := nil;
                   last_lab := NONE)

    fun finish_fun () = (finish_block();
                         flist := (rev(!blist))::(!flist);
                         blist := nil)

    fun finish_prog() = 
	   case !last_lab
             of SOME _ => E.impossible "finish_prog without finish_fun"
              | NONE => rev(!flist) before flist := nil

    (* Append an instruction to the list of generated instructions. *)
    fun emit i = ilist := i::(!ilist)

    fun emit_label l = (finish_block(); last_lab := SOME l)
  end

    val newline_lab = M.thislab "NL"

    (* Memory management functions. *) 

    val heap_size = 32000 (* in bytes -- should be less than 64KB *)
    val init_lab = M.thislab("init")
    val alloc_lab = M.thislab("alloc")

    (* Emits a call to alloc, to allocate 'size' bytes, and put the 
     * returned address in 'ret_reg'. *) 
    fun emit_alloc_call (size:M.immed, ret_reg:M.reg) = 
      (emit (M.Li(M.reg("$a0"), size));
       emit (M.Jal(alloc_lab));
       emit (M.Move(ret_reg, M.reg("$v0"))))
          
    fun emit_alloc_func () = 
      (emit_label alloc_lab;
       emit (M.Lw(M.reg "$v0", (M.immed 0, M.reg "$gp"))); (* Lw(reg1,addr); addr = immi + reg2  *)
       emit (M.Arith3(M.Add, M.reg "$t0",M.reg "$v0", M.reg("$a0")));
       emit (M.Sw(M.reg "$t0", (M.immed 0, M.reg "$gp")));   (* Why? *)
       emit_label (M.thislab "alloc.epilog");
       emit (M.Jr(M.reg("$ra"), M.reg "$v0" :: M.calleeSaved));
       finish_fun())

    fun emit_init_func () = 
     let val ra_tmp = M.newReg()
      in emit_label (M.thislab "main");
         emit (M.Move(ra_tmp, M.reg "$ra"));
         emit (M.Li(M.reg("$a0"), M.immed(heap_size)));
         emit (M.Li(M.reg("$v0"), M.immed(9)));
         emit (M.Syscall);
         emit (M.Sw(M.reg "$v0", (M.immed 0, M.reg "$gp")));
         emit (M.Jal(M.thislab "_main"));
         emit (M.Move(M.reg "$ra", ra_tmp));
         emit_label (M.thislab "main.epilog");
         emit (M.Jr(M.reg("$ra"), M.reg "$v0" :: M.calleeSaved));
         finish_fun()
      end

    fun emit_printint_func () =
      (emit_label (M.thislab "_printint");
       emit (M.Li(M.reg("$v0"), M.immed(1)));
       emit (M.Syscall);
       (* Print a newline after the integer, for clarity. *)
       emit (M.La(M.reg("$a0"), newline_lab));
       emit (M.Li(M.reg("$v0"), M.immed(4)));
       emit (M.Syscall);
       emit_label (M.thislab "_printint.epilog");
       emit (M.Jr(M.reg("$ra"),M.reg "$v0" :: M.calleeSaved));
       finish_fun())

    datatype value = Reg of M.reg | Lab of M.lab

    (* A function environment maps: A.id -> M.lab * A.func *)

    fun fun_label id = M.thislab("_" ^ Symbol.name id)

    fun add_fun_to_env (id,env) = 
            Symbol.enter (env, id, Lab(fun_label id))

    (* A variable environment maps: A.id -> M.reg *)

    fun fun2mips_arith_op A.Add = M.Add
      | fun2mips_arith_op A.Sub = M.Sub
      | fun2mips_arith_op A.Mul = M.Mulo
      | fun2mips_arith_op A.LT  = M.Slt
      | fun2mips_arith_op A.Eq  = M.Seq
      | fun2mips_arith_op _      = E.impossible "Arith op expected"

    (* Remove Pos and Constrain, to simplify pattern matching. *)
    fun strip(A.Pos(_,e))     = strip e
      | strip(A.Constrain(e,_)) = strip e
      | strip(A.Op(oper,el))  = A.Op(oper, map strip el)
      | strip(A.Tuple(el))    = A.Tuple(map strip el)
      | strip(A.Proj(i,e))    = A.Proj(i,strip e)
      | strip(A.If(e1,e2,e3)) = A.If(strip e1, strip e2, strip e3)
      | strip(A.Call(e1,e2))  = A.Call(strip e1, strip e2)
      | strip(A.While(e1,e2)) = A.While(strip e1, strip e2)
      | strip(A.Let(i,e1,e2)) = A.Let(i,strip e1, strip e2)
      | strip(e)                = e
        
    (* gen_exp: generates code for one expression 
     *    inputs: env:  environment 
     *            exp:  the expression to emit code for
     *    output: M.reg -- if ret value is <>, we return r0
     *)
  fun gen_exp env : A.exp -> M.reg = 
  let
       fun gen (A.Id (id)) =       
              (case Symbol.look (env, id) of
                 SOME (Reg r) => r
               | SOME (Lab lab) => 
	               let val r = M.newReg()
	                in emit (M.La(r, lab));
	                   r
	               end
               | NONE => E.impossible ("Can't find " ^ Symbol.name id))
	      
      (* IMPLEMENT ME! *)
		  
	 | gen  (A.Int (num)) =
	  let val r = M.newReg()
	  in
	      emit (M.Li(r, M.immed(num)));
	      r
          end
	      
	 | gen  (A.Op (oper, explist)) =
	       (case (oper, explist) 
                           of  (A.Ref, [e]) => (case e 
						of (A.Int num) =>	  
						                 let val r = M.newReg()
								 in
								     emit_alloc_call (M.wordSizeImmed, r); 
								     (* returns v0 with addr of allocated space  *)
								     emit (M.Move(r, M.reg "$v0"));
								     emit (M.Sw(gen e, (M.immed 0, r)));
								     r
								 end 
					        |(A.Tuple tplist) => gen (A.Tuple tplist)  
			                        (*|_  => E.impossible "unimplemented translation" ) *)
						|_  => 						                 let val r = M.newReg()
								 in
								     emit_alloc_call (M.wordSizeImmed, r); 
								     (* returns v0 with addr of allocated space  *)
								     emit (M.Move(r, M.reg "$v0"));
								     emit (M.Sw(gen e, (M.immed 0, r)));
								     r
								 end  ) 
       
                           | (A.Get, [e]) => let val res = M.newReg()
					     in
						 
						 (case e 
							  of (A.Op(A.Ref, [x])) => gen x  (* Munch: !(ref x) = x  *)
							  |_  => (emit (M.Lw(res, (M.immed 0, gen e)));
                                                                 res))
					     end
			                     
					     
                           | (A.Set, [id,e]) => let val iden = gen (id)
						    val exp = gen (e)
						in
						    emit (M.Sw(exp, (M.immed 0, iden))); (* Should return the value of the exp  *)
                                                    exp
						end 
			   | (_,[e1,e2])  => let val r1 = gen e1;
				       val r2 = gen e2;
				       val r = M.newReg()
				   in
				       (emit(M.Arith3(fun2mips_arith_op oper,r,r1,r2));
					r)

				 end
	       )

         | gen (A.Tuple explist) = let val words = length explist * 4
				       val values = map gen explist
				       val res = M.newReg()
			               fun store_values (ctr: int ref, values) = if !ctr = length explist 
									       then res
									       else (								   (emit (M.Sw(List.nth(values,(!ctr)), (M.immed (!ctr * 4), res)))); ctr := !ctr + 1;
								   store_values (ctr, values))
				       
				       val ctr = ref 0
				   in
				       emit_alloc_call (M.immed words, res); (* returns v0 with addr of allocated space  *)
				       store_values ((ref 0), values)
					   
				   end
	                          
         | gen (A.Proj(i,e))    = let val addr = gen e
				      val res = M.newReg()
				  in
				      emit (M.Lw(res, (M.immed (i * 4), addr)));
				      res
				  end
         | gen (A.If(e1,e2,e3)) = let val ifexp = gen e1
				       val result = M.newReg()
				       val else_cond = M.freshlab()
				       val exit = M.freshlab()
				   in
				       
				       emit(M.Branchz (M.Eq, ifexp, else_cond)); (* Else part if ifexp = 0 *)
				       emit (M.Move(result, gen e2));
				       emit(M.J(exit));
				       emit_label (else_cond);
				       emit (M.Move(result, gen e3));
				       emit_label (exit);
				       result
				   end
				
         | gen (A.Call(A.Id e1,e2))  = let val arg = gen e2
				           val a0_temp = M.newReg()    
					   val result = M.newReg()
				       in
				           emit (M.Move(a0_temp,M.reg "$a0"));                                     (* save a0 in temp *)
				           emit (M.Move(M.reg "$a0", arg));                                        (* passed argument in a0 *)
             	                           emit (M.Jal(fun_label (e1)));                                           (* jump and link *)
					   emit (M.Move(result,M.reg "$v0"));
				           emit (M.Move(M.reg "$a0",a0_temp));                                        (* restore a0 *)
				           (result)                                            (* return v0 because it contains the result *)
				      end 
         | gen (A.While(e1,e2)) = let val cond = gen e1
				      val res = M.newReg()
				      val temp = M.newReg()
				      val body = M.freshlab()
				      val exit = M.freshlab()
				   in
				       emit_label (body);		
				       emit(M.Branchz (M.Eq, cond, exit));
				       emit (M.Move(res, gen e2));
				       emit (M.J(body));
				       emit_label (exit);
				       emit (M.Move(res, gen (A.Tuple [])));  (* Return type of while is unit  *)
				       res
				  end
 
         | gen (A.Let(id',e1,e2)) =  let 
					 val value = gen e1
                                         val fenv' = Symbol.enter (env, id', Reg(value))
				     in
					 gen_exp fenv' e2
				     end

        | gen _ = E.impossible "unimplemented translation"
     in gen
    end

    (* gen_func: generates code for one function
     *    inputs: fenv: functions environment
     *            func: the function to be generated
     *)
    fun gen_func (fenv, (f,x,t1,t2,exp)) = 
          (  
           emit_label (fun_label f);
           let val ra_tmp = M.newReg()
	       val temps  = map (fn (x)=> ( let val newReg = M.newReg()
                                            in 
                                               ( emit (M.Move(newReg,x));
                                                 (newReg)
                                               )
                                            end
                                          )
                                ) M.calleeSaved
	       val a0_tmp = M.newReg()         
	       val fenv' = Symbol.enter (fenv, x, Reg (M.reg "$a0")) 
	       val id = M.newReg()
	       val arg = M.newReg()
	   in
	       (* Save callee saved registers  *)
	       emit (M.Move(ra_tmp, M.reg "$ra"));
	       emit (M.Move(a0_tmp, M.reg "$a0"));

               emit (M.Move(M.reg "$v0", gen_exp fenv' (strip exp)));

	       (* emit restore *)
	       emit (M.Move(M.reg "$ra", ra_tmp));
	       (ListPair.map (fn (x,y) => (emit (M.Move(x,y)))) (M.calleeSaved,temps));
	       emit (M.Move(M.reg "$a0", a0_tmp));

               emit_label (Symbol.symbol(Symbol.name (fun_label f) ^ ".epilog"));
	       emit (M.Jr(M.reg("$ra"), M.reg "$v0" :: M.calleeSaved));
               finish_fun ()
	end
          )

    (* codegen: generates code for a program 
     *    input:  A.prog
     *    output: M.program 
     *)
    fun codegen (fundec_list :A.prog) = 
      (* 1. Generate functions-env
       * 2. Emit runtime-system functions
       * 3. For each function, generate code for it
       *)
      let
        val fenv = foldl add_fun_to_env Symbol.empty 
                     (map #1 Absyn.externals @ map (#1 o #2) fundec_list)
      in
         init_lists(); 
         emit_init_func();
         emit_alloc_func();
         emit_printint_func();
         List.app (fn (_,fd) => gen_func (fenv, fd)) fundec_list;          
         ([(newline_lab,"\\n")], finish_prog())
      end
  end
