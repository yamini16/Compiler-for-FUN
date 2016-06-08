signature REGALLOC = sig
(* alloc(f) returns mips code for function f that has all the temporaries
    replaced by mips regs and the load/store of spilled temps taken care of
*)
 val alloc : Mips.funcode ->  Mips.funcode 
end

structure RegAlloc :> REGALLOC =
struct
   structure M = Mips
   structure RS = Mips.RegSet
   structure IG = Liveness.IG

   fun spillCost x = 1

   fun getmove g (M.Move(r1,r2)) = IG.mk_edge g {from=r2,to=r1} 
     | getmove g _ = ()

   fun alloc(instrL as ((funlab,block)::rest) : M.funcode) = 
   let val ig = Liveness.interference_graph instrL
       val movegraph = IG.newGraph()
       val _ = app (fn (_,l) => app (getmove movegraph) l) instrL
       val _ = print "###### Move graph\n"
       val _ = Liveness.printgraph print movegraph
       val palette = M.list2set (M.reg"$ra"::M.callerSaved @ M.calleeSaved)
	val _ = print "In here before coloring"
       val coloring = Color.color {interference = ig, moves=movegraph, 
	                  spillCost = spillCost, palette=palette}
       (*val _ = Color.verify{complain=ErrorMsg.impossible, func=instrL, 
                            spillCost=spillCost, palette=palette, 
                            coloring=coloring}
       *)
       val _ = print "Register Allocation verified.\n"
       val {alloc,spills} = coloring
       val spillList = RS.listItems(#spills coloring) 
       val numSpills = RS.numItems spills 
       (* val index = ref 0 *)
       val _ = (print "Spills: "; 
                RS.app (fn r => (print (M.reg2name r); print " ")) spills;
	        print "\n")
       fun indexOf (slist, s) = case slist 
				  of [] => 0
				  |x::xs => if (M.comparereg(x,s) = EQUAL ) then 0 else ( 1 + indexOf (xs, s))   
       (*fun output (funlab,block) = (funlab,(List.map (fn (instruction) => 
                                                          ((M.rename_regs alloc) instruction)
                                                     ) (M.Arithi(M.Addi, M.reg "$sp", M.reg "$sp", M.immed(numSpills * M.wordSize))::block)
                                           )                  
                                   )
       *)
       (* Simple Spilling  *)
       fun output (funlab,block) = (funlab,(List.map (fn (instruction) => 
						      case instruction of
							M.Syscall => (M.Syscall)
                                                        | _  =>  
                                                          (let val def_set = #def(M.instr_def_use(instruction))
							      val use_set = #use(M.instr_def_use(instruction))
							      val def_use = RS.union (def_set, use_set)
							      fun inSpills () = RS.exists (fn x => RS.member (spills, x)) def_use
                                                          in
                                                              if inSpills () 
							         then (case instruction of
								(M.Move (rd, rs)) => let val index1 = indexOf(spillList, rd)* M.wordSize
											 val index2 = indexOf(spillList, rs)* M.wordSize
						                                     in
									                        if RS.member (spills, rd)
												then M.Sw(rs,(M.immed (index1),M.reg "$sp"))
										                else M.Lw(rd,(M.immed (index2),M.reg "$sp"))                                                                                     end
									| _  => ErrorMsg.impossible "Call General Spilling")  
								 else ((M.rename_regs alloc) instruction)
							  end
							)
                                                     ) block
                                           )                  
                                   )
   
    in 
       (
	 let val (epilog, instrl) = (List.nth(instrL, (length(instrL)-1)) )
             val newLastBlock = (epilog, (M.Arithi(M.Addi, M.reg "$sp", M.reg "$sp", M.immed(numSpills * M.wordSize))::instrl))
             (*val firstFunBlock = (funlab,(M.Arithi(M.Addi, M.reg "$sp", M.reg "$sp", M.immed(~(numSpills * M.wordSize)))::block)) *)
             val newInstrL = List.drop(List.take(instrL, (length(instrL)-1)),1)@[newLastBlock]
       in
        map output ((funlab,(M.Arithi(M.Addi, M.reg "$sp", M.reg "$sp", M.immed(~(numSpills * M.wordSize))))::block)::newInstrL)
       end
      )
  end

end
