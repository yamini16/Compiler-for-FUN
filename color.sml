structure Color : COLOR = 
struct

 structure IG = Liveness.IG
 structure M = Mips
 structure RS = M.RegSet

 val (SOME spillCostInfinity) = Int.maxInt

 type coloring = {alloc: M.allocation, spills: RS.set} (* (reg RegTb.table Table reg -> int) Allocated registers Table,
                                                           set of spilled registers  *)

 fun verify {complain: string -> unit,
             func: M.funcode, 
             spillCost: Mips.reg -> int,
             palette: RS.set,

	     coloring={alloc: M.allocation, spills: RS.set}} : unit =
   ErrorMsg.impossible "Color.verify unimplemented"

 fun color ({interference = ig: IG.graph,
             moves: IG.graph, (* Don't consider for now  *)
             spillCost: M.reg ->int,
             palette: RS.set}) = 
    
      let val spills = RS.empty
          val graph = IG.newGraph() (* copying ig to graph *)
          val _ = (RS.app (fn (x) => ((IG.succ graph x);())) (IG.nodes ig)) (* nodes copied*)
          val _ = (RS.app (fn (x) => (RS.app (fn (succ) => IG.mk_edge graph {from=x,to=succ}) (IG.adj ig x) )) (IG.nodes graph)) 
          val unColored = ref (RS.filter (fn n => not(RS.member(palette,n))) (IG.nodes ig))
          val preColored = RS.filter (fn n => (RS.member(palette,n))) (IG.nodes ig)
         
	  fun calc_degs (unColored) = 
              let val _ = print "\nIn calc_deg: Registers = "
		  val _ = RS.app (fn x => print (M.reg2name x)) (!unColored)
                  val avail_colors = RS.numItems(palette) 
              in  
		  (
		    (print ("\nLen calc_deg = " ^ Int.toString(RS.numItems(RS.filter (fn x => (RS.numItems(IG.adj ig x) < avail_colors)) (!unColored))));
			  RS.filter (fn x => (RS.numItems(IG.adj ig x) < avail_colors)) (!unColored)),
	            RS.filter (fn x => (RS.numItems(IG.adj ig x) >= avail_colors)) (!unColored)
		  )
              end 


         fun select (ig, node, {alloc, spills}):coloring =
                                      
              let val colored_succ = RS.filter (fn x => case M.RegTb.look(alloc,x)  (*Get non-precolored, now colored adjacent nodes in interferece graph *)
                                                of SOME x => true
                                                | NONE => false ) (IG.adj graph node) 
                  val usedColor = RS.map (fn x => Option.getOpt(M.RegTb.look(alloc,x), List.nth(M.calleeSaved,0))) colored_succ
                  val preColoredSucc = RS.filter (fn n => (RS.member(palette,n))) (IG.adj graph node)
                  val _ = ((print ("\n Pre colored Succ for "^(M.reg2name node)^" are: "));(RS.app (fn (x)=>(print (M.reg2name x); print " ")) preColoredSucc))
                        (* in the above, List.nth is a default value because default must be register, but we don't expect it to be used*)
               in 
                     if RS.member(spills,node)
                        then {alloc = alloc, spills = spills} 
                        else    
                                    (
                                       let val palette' = RS.difference(RS.difference(palette, usedColor),preColoredSucc) 
                                       in
					  (print ("\n available color for "^(M.reg2name node)^" options: "));(RS.app (fn (x)=>(print (M.reg2name x); print " ")) palette');
                                          {alloc=M.RegTb.enter(alloc,node,List.nth(RS.listItems(palette'),0)),spills=spills} 
                                       end  
                                   )                                   
               end             

                (* return type is coloring *)
          fun simplify (lowdeg):coloring = 
	      let val g = IG.newGraph()
		  val _ = RS.app (fn x => (IG.rm_edge ig {from = x, to = lowdeg})) (IG.adj ig lowdeg)
                  val _ = print("\n In simplify lowdeg = " ^ (M.reg2name lowdeg)) 
                             
	          val _ = RS.app (fn x => print("\n In simplify Uncolored " ^ (M.reg2name x))) (!unColored)
              in
		  (unColored := RS.delete (!unColored, lowdeg);
		  select(ig, lowdeg, phase(calc_degs (unColored))))
             end                                     

          and spill (highdeg):coloring =  (RS.app (fn x => (IG.rm_edge ig {from = highdeg, to = x})) (IG.adj ig highdeg);
					   RS.app (fn x => (IG.rm_edge ig {from = x, to = highdeg})) (IG.adj ig highdeg);
                                           unColored := RS.delete (!unColored, highdeg);
                                           let val newColor = phase(calc_degs (unColored))
					       val _ = print("\n In spill highdeg = " ^ (M.reg2name highdeg) ^ "\nSpilled list: "  )
					       
					       
					       val _ = RS.app (fn x => print( (M.reg2name x) ^ " "  )) (RS.add(#spills(newColor),highdeg))
					   in
                            		      select(ig, highdeg, {alloc = #alloc(newColor), spills = RS.add(#spills(newColor),highdeg)})
					   end
                                          )
	  and phase (lowdegs: RS.set, highdegs: RS.set):coloring =
	      let 
                  val _ = print "\nIn phase: lowdegs Registers = "
		  val _ = RS.app (fn x => print (M.reg2name x)) lowdegs
                  val _ = print "\nIn phase: highdegs Registers = "
		  val _ = RS.app (fn x => print (M.reg2name x)) highdegs
              in
		  case  ((RS.isEmpty lowdegs),(RS.isEmpty highdegs)) 
                        of (false, _)  => (print (" calling simplify \n ") ; simplify ((List.nth(RS.listItems(lowdegs),0)) ))
                        |(true, false) => (print (" calling spill \n  ") ; spill ((List.nth(RS.listItems(highdegs),0))) (* Least spill cost?? *) )
		        |(true,true)   => (print (" base case \n ") ; {alloc=M.RegTb.empty,spills=RS.empty} )
              end
     in 
	  phase(calc_degs (unColored))
     end 
end
