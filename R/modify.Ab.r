modify.Ab=function(y,idx.mod,reps,diel.setup,delta){
    
  bf.Ab.new=vector("list",length(idx.mod)); names(bf.Ab.new)=names(idx.mod)

  for(i in 1:length(bf.Ab.new)){
      
    if(diel.setup[[idx.mod[i]]]$func=="bf_equality"){
      #Get A matrix and b vector
      A=diel.setup[[idx.mod[i]]][[2]]
      b=diel.setup[[idx.mod[i]]][[3]]
      C=diel.setup[[idx.mod[i]]][[4]]
      d=diel.setup[[idx.mod[i]]][[5]]
      #Need to repeat A matrix the number of reps  
      A=do.call("cbind", rep(list(A), reps))
      C=do.call("cbind", rep(list(C), reps))

      temp=multinomineq::bf_equality(k=y,
                               options=rep(3,reps),
                               A=A,
                               b=b,
                               C=C,
                               d=d,
                               delta = delta,
                               return_Ab = TRUE)
      bf.Ab.new[[i]]=list(A=temp$A,b=temp$b)
      
    }else{#End if statement                
    #if_equality is false then  
      A=diel.setup[[idx.mod[i]]][[2]]
      b=diel.setup[[idx.mod[i]]][[3]]
      A=do.call("cbind", rep(list(A), reps))

      bf.Ab.new[[i]]=list(A=A,b=b)
      
    }
  }#End Model for Loop
  bf.Ab.new
}#End Function


