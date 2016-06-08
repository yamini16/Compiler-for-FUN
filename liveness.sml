signature LIVENESS = sig
  structure IG: GRAPH where S=Mips.RegSet
  val analyze: {mention: Mips.reg -> unit, 
	        interfere: Mips.reg -> Mips.reg -> unit} ->
               Mips.funcode -> unit
  val interference_graph: Mips.funcode -> IG.graph
  val printgraph: (string->unit) -> IG.graph -> unit
end

structure Liveness : LIVENESS = struct
  structure IG = Graph(Mips.RegSet)
  structure M = Mips
  structure RS = Mips.RegSet


 fun analyze {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
             (blocks: M.codeblock list) = 
 
 let fun is_single_line (i) = 
    case i 
         of M.Arith2 (_,_,_) => true
         | M.Arith3 (_,_,_,_) => true
         | M.Arithi (_,_,_,_) => true
         | M.Li (_,_) => true
         | M.La (_,_) => true
         | M.Lw (_,_) => true
         | M.Sw (_,_) => true
         | M.Move (_,_) => true
         | M.Jal(_) => true
	 | M.Jr (_) => true
         | _ => false

 fun is_cond_L (i) = 
    case i 
         of M.Branchz (_,_,L) => (true,L)
    | M.Branchu (_,_,_,L) => (true,L)
    | M.Branch (_,_,_,L) => (true,L)
    | _ => (false, M.thislab "dummy") 

 fun is_uncond_L (i) = 
    case i 
         of  M.J(lab) => (true, lab)        
    | _ => (false, M.thislab "dummy") 
  
 fun add_lab ((x,y), tab) =  Symbol.enter(tab, x, RS.empty )(* we expect this table to be a live set map instr->live set *)
 val live_at = ref (foldl add_lab Symbol.empty blocks) 
 val _ = map (fn (x,y) => print (Symbol.name x)) blocks 

 (*Function to: For each node in FromSet, create an edge to every node in ToSet *)
 fun setInterfere (FromSet, ToSet) =
     RS.app (fn (x) => RS.app (fn (y)=>(interfere x y)
                              ) ToSet
            ) FromSet

 fun compute_live_in(nil, live_at_end) =
     live_at_end 
   
   | compute_live_in (M.Li(r, i)::M.Syscall::rest, live_at_end) =
      let val live_out = compute_live_in(rest, live_at_end)
          val _ =  mention r
          in 
               if r = Mips.reg "$v0"
               then (case M.syscall_def_use (M.immed2int i) of
	                SOME {def,use} => ( (setInterfere (def, live_out));
                                   M.RegSet.union(use, M.RegSet.difference(live_out, def))
                                          )
	                | NONE => ErrorMsg.impossible "Unknown Syscall"
                    )
               else ErrorMsg.impossible "Syscall not preceded by li $v0"
      end  

   | compute_live_in(i::rest, live_at_end) =

   let val live_out = compute_live_in(rest, live_at_end)
       val regs = RS.listItems(#use(M.instr_def_use(i)))@RS.listItems(#def(M.instr_def_use(i))) 
       val _ = map mention regs
       val def_set = #def(M.instr_def_use(i))
       val use_set = #use(M.instr_def_use(i))
       val _= (setInterfere (def_set, live_out))
   in
       (*map mention regs;*)
       if is_single_line (i)
	  then (M.RegSet.union(use_set, M.RegSet.difference(live_out, def_set)))

          else if #1(is_cond_L (i)) 
		    then (M.RegSet.union(use_set , 
					(M.RegSet.difference 
					     (M.RegSet.union
						  (Option.getOpt(Symbol.look(!live_at,(#2(is_cond_L (i)))), RS.empty), 
						  live_out), def_set ))))

                  else (*if #1(is_uncond_L (i)) *)
		      (M.RegSet.union(use_set, 
				      M.RegSet.difference
					  (Option.getOpt(Symbol.look(!live_at,(#2(is_uncond_L (i)))), RS.empty), 
					   def_set)))
   end

     

 val changed = ref true
 
 fun live ([], live_tab) = live_tab 

   | live ((n::next):M.codeblock list, live_tab) = let val newval = (case next 
						of [] => compute_live_in(#2 n, RS.empty)
					| next::next' => (*compute_live_in(#2 n, (Option.getOpt(Symbol.look(!live_tab,(#1 next)), RS.empty)))) *)                                                     (let val _ = live (next', live_tab)
							  in
						  compute_live_in(#2 n, (Option.getOpt(Symbol.look(!live_tab,(#1 next)), RS.empty)))    
						          end) 
																									  )
                         in 

                             ((if not(RS.equal(newval, (Option.getOpt(Symbol.look(!live_tab,(#1 n)), RS.empty))))  
                             then changed := !changed else changed := false);
                             (*try(next,let val tab = ref (Symbol.enter(!live_tab, #1 n, newval)) in tab end )) *)
			      live_tab :=  (Symbol.enter(!live_tab, #1 n, newval));
			      live(next, live_tab ))
                         end
  val _ = live(blocks, live_at)   
 val _ = while ((!changed)= true) do live(blocks, live_at)
 
 in
    ()
 end  


(**********************************************************************************************************************************)
 fun printadj say g i = 
     (say (M.reg2name i); say ":";
      IG.S.app (fn j => (say " "; say (M.reg2name j))) (IG.adj g i);
      say "\n")

 fun printgraph say g = IG.S.app (printadj say g) (IG.nodes g);

 fun interference_graph (func: M.funcode) =
  let val _ = (print "################## LIVENESS: "; 
               print (Symbol.name(#1(List.nth(func,0)))); print "\n")
      val g = IG.newGraph()
      fun mention (r: M.reg) = (IG.succ g r; ())
      fun interfere r1 r2 = if (M.comparereg (r1,r2)= EQUAL) then () else (IG.mk_edge g {from=r1,to=r2})
      
   in analyze {mention=mention,interfere=interfere} func;
      print "################## INTERFERENCE GRAPH \n";
      printgraph print g;
      (IG.rm_edge g {from=M.reg "$v0",to=M.reg "$ra"});
      let 
	  val num = RS.numItems(IG.adj g  (M.reg ("$ra")))
          val _ = RS.app (fn x => print (M.reg2name x)) (IG.nodes g)
          val palette = M.list2set (M.reg"$ra"::M.callerSaved @ M.calleeSaved)
          val unColored = RS.delete ((IG.nodes g), M.reg "$v0")
          val _ = RS.app (fn x => print (M.reg2name x)) unColored
      in  
          print ("No. of nodes = " ^ Int.toString(num)) ;  g
	  end
  end

end
