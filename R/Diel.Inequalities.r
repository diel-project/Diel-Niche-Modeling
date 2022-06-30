#' Inequality Setup
#'
#' Multinomial model inequalities for diel hypotheses
#' @param e Default is 0.05. A single value of variation for probabilities. If specified, it will be applied to all hypotheses, regardless of whether individual epsilon hypotheses values are specified. 
#' @param e.D Default is 0.05. A single value of variation for the Diurnal hypothesis.
#' @param e.Dn Default is 0.05. A single value of variation for the Diurnal-nocturnal hypothesis.
#' @param e.Dcr Default is 0.05. A single value of variation for the Diurnal-crepuscular hypothesis.
#' @param e.N Default is 0.05. A single value of variation for the Nocturnal hypothesis.
#' @param e.Nd Default is 0.05. A single value of variation for the Nocturnal-diurnal hypothesis.
#' @param e.Ncr Default is 0.05. A single value of variation for the Nocturnal-crepuscular hypothesis.
#' @param e.CR Default is 0.05. A single value of variation for the Crepuscular hypothesis.
#' @param e.CRd Default is 0.05. A single value of variation for the Crepuscular-diurnal hypothesis.
#' @param e.CRn Default is 0.05. A single value of variation for the Crepuscular-nocturnal hypothesis.
#' @param e.EC  Default is 0.05. A single value of variation for the Evan Cathemeral hypothesis.
#' @param e.AC   Default is 0.10. A single value of variation for the Available Cathemeral hypothesis.
#' @param xi.D Default c(0.90,0.95). A vector of the lower threshold value and the most likely value, respectively for the Diurnal hypothesis.
#' @param xi.Dn Default c(0.80,0.20,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Diurnal-nocturnal hypothesis.
#' @param xi.Dc Default c(0.70,0.05,0.80,0.10). A vector of the lower threshold value for the primary probability, lower threshold value for the secondary probabilities, most likely primary probability, and most likely secondary probability value, respectively for the Diurnal-cathemeral hypothesis.
#' @param xi.Dcr Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Diurnal-crepuscular hypothesis.
#' @param xi.N  Default c(0.80,0.90). A vector of the lower threshold value and most likely value,, respectively for the Nocturnal hypothesis.
#' @param xi.Nc Default c(0.70,0.05,0.80,0.10). A vector of the lower threshold value for the primary probability, lower threshold value for the secondary probabilities, most likely primary probability, and most likely secondary probability value, respectively for the Nocturnal-cathemeral hypothesis.
#' @param xi.Nd  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Nocturnal-diurnal hypothesis.
#' @param xi.Ncr  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Nocturnal-crepuscular hypothesis.
#' @param xi.CR Default c(0.80,0.90). A vector of the lower threshold value and most likely value,, respectively for the Crepuscular hypothesis.
#' @param xi.CRd  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Nocturnal-crepuscular hypothesis.
#' @param xi.CRn  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Crepuscular-nocturnal hypothesis.
#' @param xi.CRc Default c(0.70,0.05,0.80,0.10). A vector of the lower threshold value for the primary probability, lower threshold value for the secondary probabilities, most likely primary probability, and most likely secondary probability value, respectively for the Crepuscular-cathemeral hypothesis.
#' @param xi.EC Default c(0.33). A single value of the available amount of time in all three diel periods.
#' @param p.avail Default c(0.166666,0.4166667). A vector of the available time in the periods of crepuscular and diurnal. Nighttime availability is found by subtraction.
#' 
#' @return diel.hyp A list of diel hypotheses as multinomial inequalities.
#' \item{inputs}{Includes all inputted values; epsilon, xi, and p.avail.} 
#' \item{D.th, D.max, D.var, Dn.th, Dn.max, Dn.var, Dc.max, Dcr.th, Dc.th,
#' Dcr.max,Dcr.var, N.th, N.max, N.var, Nd.th, Nd.max, Nd.var, Nc.th, 
#'  Nc.var, Ncr.th, Ncr.max, Ncr.var, CR.th, CR.max, CR.var,CRd.th,
#' CRd.max, CRd.var, CRn.th, CRn.max, CRn.var,'EC.th, EC.var, AC.var, C.th, Uncon}{Each is a list of three elements: Hypotheis Descriptive Name, A matrix, and b vector.} 
#' @examples 
#' diel.ineq()
#' diel.ineq(e=0.01) #To replace all epsilon values with 0.01.
#' #To replace the default values with a new epsilon value and a new xi value.
#' diel.ineq(e.Dn=0.2, xi.Dn=c(0.80,0.20,0.90,0.10)) 
#' @export
############################
#REMOVED
## @param xi.CRc Default c(0.80,0.01,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Crepuscular-cathemeral hypothesis.

############################


#################################
#################################
# Input variables

# e  -  lower and upper difference to calculate range of probability values.

# e varies by hypothesis, so:      e.D, e.Dn, e.Dcr, 
#                                  e.N, e.Nd, e.Ncr, 
#                                  e.CR, e.CRd, e.CRn, 
#                                  e.EC, e.AC

# p.avail - (Available Cathemeral; length 2)
#        [1] Available proportion of time in twilight time period
#        [2] Available proportion of time in daytime period

# xi.D - (Diurnal Hyp; length 2) 
#        [1] lowest probability of p_d , 
#        [2] most likely probability of p_d.

# xi.Dn - (Diurnal-nocturnal Hyp.; length 4)  
#        [1] lower prob. of Primary prob., 
#        [2] upper prob. of Secondary prob.,
#        [3] most likely prob. of Primary prob., 
#        [4] most likely prob. of Secondary prob.

# xi.Dcr - (Diurnal-crepuscular; length 4)
#          [1] lower prob. of Primary prob.  
#          [2] upper prob. of Secondary prob (i.e., non-Diurnal periods). 
#          [3] most likely prob. of Primary prob., 
#          [4] most likely prob. of Secondary prob.

# xi.N - (Nocturnal Hyp; length 2) 
#        [1] lowest probability of p_n , 
#        [2] most likely probability of p_n.

# xi.Nd - (Nocturnal-diurnal Hyp.; length 4)  
#        [1] lower prob. of Primary prob., 
#        [2] upper prob. of Secondary prob.,
#        [3] most likely prob. of Primary prob., 
#        [4] most likely prob. of Secondary prob.

# xi.Ncr - (Nocturnal-crepuscular; length 4)
#          [1] lower prob. of Primary prob.  
#          [2] upper prob. of Secondary prob. 
#          [3] most likely prob. of Primary prob., 
#          [4] most likely prob. of Secondary prob.

# xi.CR - (Crepuscular Hyp; length 2) 
#        [1] lowest probability of p_c , 
#        [2] most likely probability of p_c.

# xi.CRd - (Crepuscular-diurnal; length 4)
#          [1] lower prob. of Primary prob.  
#          [2] upper prob. of Secondary prob. 
#          [3] most likely prob. of Primary prob., 
#          [4] most likely prob. of Secondary prob.

# xi.CRn - (Crepuscular-nocturnal; length 4)
#          [1] lower prob. of Primary prob.  
#          [2] upper prob. of Secondary prob. 
#          [3] most likely prob. of Primary prob., 
#          [4] most likely prob. of Secondary prob.


# xi.EC - (Even Cathemeral; length 1)
#        [1] most likely probability, which is applied to all
#           diel probs.

#start function
diel.ineq=function(e=NULL,
                   e.D=NULL, e.Dn=NULL, e.Dcr=NULL, 
                   e.N=NULL, e.Nd=NULL, e.Ncr=NULL, 
                   e.CR=NULL, e.CRd=NULL, e.CRn=NULL,
                   e.EC=NULL, e.AC=NULL,
                   xi.D=NULL, xi.Dn=NULL, xi.Dcr=NULL,
                   xi.N=NULL,xi.Nd=NULL,xi.Ncr=NULL,
                   xi.CR=NULL,xi.CRd=NULL,xi.CRn=NULL,xi.CRc=NULL,
                   xi.EC=NULL,xi.Dc=NULL,xi.Nc=NULL, p.avail=NULL){
  
  #Default epsilon values for variation hypotheses
  if(is.null(e.D)){e.D=0.05}
  if(is.null(e.Dn)){e.Dn=0.05}
  if(is.null(e.Dcr)){e.Dcr=0.05}
  if(is.null(e.N)){e.N=0.05}
  if(is.null(e.Nd)){e.Nd=0.05}
  if(is.null(e.Ncr)){e.Ncr=0.05}
  if(is.null(e.CR)){e.CR=0.05}
  if(is.null(e.CRd)){e.CRd=0.05}
  if(is.null(e.CRn)){e.CRn=0.05}
  if(is.null(e.EC)){e.EC=0.05}
  if(is.null(e.AC)){e.AC=0.1}
  
  # If e is entered then this forces all e's to be the same.  
  if(!is.null(e)){e.D=e.Dn=e.Dcr=e.N=e.Nd=e.Ncr=e.CR=e.CRd=e.CRn=e.EC=e.AC=e}
  
  # Defaults for threshold and variation models     
  if(is.null(xi.D)){xi.D=c(0.90,0.95)} #lower threshold value, and most likely value
  if(is.null(xi.Dn)){xi.Dn=c(0.8,0.2,0.9,0.1)}
  if(is.null(xi.Dcr)){xi.Dcr=c(0.8,0.2,0.9,0.1)}
  
  if(is.null(xi.N)){xi.N=c(0.8,0.9)}
  if(is.null(xi.Nd)){xi.Nd=c(0.8,0.2,0.9,0.1)}
  if(is.null(xi.Ncr)){xi.Ncr=c(0.8,0.2,0.9,0.1)}
  
  if(is.null(xi.CR)){xi.CR=c(0.8,0.9)}
  if(is.null(xi.CRd)){xi.CRd=c(0.8,0.2,0.9,0.1)}
  if(is.null(xi.CRn)){xi.CRn=c(0.8,0.2,0.9,0.1)}

  if(is.null(xi.EC)){xi.EC  = c(0.33)}
  
  if(is.null(xi.Dc)){xi.Dc  = c(0.70,0.05,0.80,0.10)}
  if(is.null(xi.Nc)){xi.Nc  = c(0.70,0.05,0.80,0.10)}
  if(is.null(xi.CRc)){xi.CRc  = c(0.70,0.05,0.80,0.10)}
  
  if(is.null(p.avail)){p.avail  = c(0.166666,0.4166667)}
  
  
  #################################
  #################################
  #Unconstrained model
  #Consraints specify p_c and p_d have to be between 0 and 1
  A.uncon <- matrix(c(0,1,0,-1,1,0,-1,0),ncol = 2, byrow = TRUE)
  b.uncon <- c(1,0,1,0)
  Uncon=list(Name="Unconstrained",A=A.uncon,b=b.uncon,func="bf_multinom")

  #################################
  #################################

#################################
#################################
#Using general hyps as inequalities
small.num=0.001  
  #D
  A.D <- matrix(c(1,-1,-1,-2,0,-1),ncol = 2, byrow = TRUE)
  b.D <- c(0,-1,-0.45)
  D=list(Name="Diurnal (General)",A=A.D,b=b.D,func="bf_multinom")     

  #N
  A.N <- matrix(c(2,1,1,2,1,1),ncol = 2, byrow = TRUE)
  b.N <- c(1,1,-0.45+1)
  N=list(Name="Nocturnal (General)",A=A.N,b=b.N,func="bf_multinom")     
  
  #CR
  A.CR <- matrix(c(-1,1,-2,-1,-1,0),ncol = 2, byrow = TRUE)
  b.CR <- c(0,-1,-0.45)
  CR=list(Name="Crepuscular (General)",A=A.CR,b=b.CR,func="bf_multinom")     

  #C
  A.C <- matrix(c(1,0,0,1,-1,-1),ncol = 2, byrow = TRUE)
  b.C <- c(0.45-small.num,0.45-small.num,(0.45-small.num)-1)
  C=list(Name="Cathemeral (General)",A=A.C,b=b.C,func="bf_multinom")     

#################################
#################################
#FULL hyps as inequalities
  #CR.full
  A.CR <- matrix(c(0,1,-1,-1),ncol = 2, byrow = TRUE)
  b.CR <- c(0.1-small.num,(0.1-small.num)-1)
  CR.full=list(Name="Crepuscular (Full)",A=A.CR,b=b.CR,func="bf_multinom")     

  #D.full
  A.D <- matrix(c(1,0,-1,-1),ncol = 2, byrow = TRUE)
  b.D <- c(0.1-small.num,(0.1-small.num)-1)
  D.full=list(Name="Diurnal (Full)",A=A.D,b=b.D,func="bf_multinom")     

  #N.full
  A.N <- matrix(c(1,0,0,1),ncol = 2, byrow = TRUE)
  b.N <- c(0.1-small.num,0.1-small.num)
  N.full=list(Name="Nocturnal (Full)",A=A.N,b=b.N,func="bf_multinom")     
  
  #C.full
  A.C <- matrix(c(0,1,-1,-1,1,0,0,-1,1,1,-1,0),ncol = 2, byrow = TRUE)
  b.C <- c(0.5-small.num,(0.5-small.num)-1,0.5-small.num,-(0.1+small.num),-(0.1+small.num)+1,-(0.1+small.num))
  C.full=list(Name="Cathemeral (Full)",A=A.C,b=b.C,func="bf_multinom")     

  #Dcr.full
  A.Dcr <- matrix(c(0,-1,-1,0,1,0,-1,-1),ncol = 2, byrow = TRUE)
  b.Dcr <- c(-0.5,-0.1,0.4,(0.1-small.num)-1)
  Dcr.full=list(Name="Diurnal-crepuscular (Full)",A=A.Dcr,b=b.Dcr,func="bf_multinom")     

  #Dn.full
  A.Dn <- matrix(c(0,-1,1,1,-1,-1,1,0),ncol = 2, byrow = TRUE)
  b.Dn <- c(-0.5,0.1+1,0.4-1,0.1-small.num)
  Dn.full=list(Name="Diurnal-nocturnal (Full)",A=A.Dn,b=b.Dn,func="bf_multinom")     
  
  #Ncr.full
  A.Ncr <- matrix(c(1,1,-1,0,1,0,0,1),ncol = 2, byrow = TRUE)
  b.Ncr <- c(0.5+1,-0.1,0.4,0.1-small.num)
  Ncr.full=list(Name="Nocturnal-crepuscular (Full)",A=A.Ncr,b=b.Ncr,func="bf_multinom")     

  #Nd.full
  A.Nd <- matrix(c(1,1,0,-1,0,1,1,0),ncol = 2, byrow = TRUE)
  b.Nd <- c(-0.5+1,-0.1,0.4,0.1-small.num)
  Nd.full=list(Name="Nocturnal-diurnal (Full)",A=A.Nd,b=b.Nd,func="bf_multinom")     

  #CRd.full
  A.CRd <- matrix(c(-1,0,0,-1,0,1,-1,-1),ncol = 2, byrow = TRUE)
  b.CRd <- c(-0.5,-0.1,0.4,(0.1-small.num)-1)
  CRd.full=list(Name="Crepuscular-diurnal (Full)",A=A.CRd,b=b.CRd,func="bf_multinom")     

  #CRn.full
  A.CRn <- matrix(c(-1,0,1,1,-1,-1,0,1),ncol = 2, byrow = TRUE)
  b.CRn <- c(-0.5,0.1+1,0.4-1,0.1-small.num)
  CRn.full=list(Name="Crepuscular-nocturnal (Full)",A=A.CRn,b=b.CRn,func="bf_multinom")     

  #DN.full
  A.DN <- matrix(c(1,0,0,1,0,-1,-1,-1,1,1),ncol = 2, byrow = TRUE)
  b.DN <- c(0.1-small.num,0.6,-(0.4+small.num),0.6-1,-(0.4+small.num)+1)
  DN.full=list(Name="Diurnal-Nocturnal (Full)",A=A.DN,b=b.DN,func="bf_multinom")     

  #DCR.full
  A.DCR <- matrix(c(-1,-1,0.1,0.6,0,-1,1,0,-1,0),ncol = 2, byrow = TRUE)
  b.DCR <- c((0.1-small.num)-1,0.6,-(0.4+small.num),0.6,-(0.4+small.num))
  DCR.full=list(Name="Diurnal-Crepuscular (Full)",A=A.DCR,b=b.DCR,func="bf_multinom")     

  #NCR.full
  A.NCR <- matrix(c(0,1,-1,-1,1,1,1,0,-1,0),ncol = 2, byrow = TRUE)
  b.NCR <- c(0.1-small.num,0.6-1,-(0.4+small.num)+1,0.6,-(0.4+small.num))
  NCR.full=list(Name="Nocturnal-Crepuscular (Full)",A=A.NCR,b=b.NCR,func="bf_multinom")     

  #Dc.full
  A.Dc <- matrix(c(0,-1,-1,0,1,1),ncol = 2, byrow = TRUE)
  b.Dc <- c(-0.5,-0.1,-0.1+1)
  Dc.full=list(Name="Diurnal-cathemeral (Full)",A=A.Dc,b=b.Dc,func="bf_multinom")     

  #Nc.full
  A.Nc <- matrix(c(1,1,0,-1,-1,0),ncol = 2, byrow = TRUE)
  b.Nc <- c(-0.5+1,-0.1,-0.1)
  Nc.full=list(Name="Nocturnal-cathemeral (Full)",A=A.Nc,b=b.Nc,func="bf_multinom")     
        
  #CRc.full
  A.CRc <- matrix(c(-1,0,0,-1,1,1),ncol = 2, byrow = TRUE)
  b.CRc <- c(-0.5,-0.1,-0.1+1)
  CRc.full=list(Name="Crepuscular-cathemeral (Full)",A=A.CRc,b=b.CRc,func="bf_multinom")     
        
#################################
#################################
    
  #Diurnal Hypotheses
  # Threshold
  A.D.th <- matrix(c(1,-1,-1,-2,0,-1),ncol = 2, byrow = TRUE)
  b.D.th <- c(0,-1,-xi.D[1])
  D.th=list(Name="Diurnal Threshold",A=A.D.th,b=b.D.th,func="bf_multinom")
  # Maximizing
  A.D.max <- matrix(c(1,-1,-1,-2),ncol = 2, byrow = TRUE)
  b.D.max <- c(0,-1)
  D.max=list(Name="Diurnal Max",A=A.D.max, b=b.D.max,func="bf_multinom")     
  # Variability
  A.D.var <- matrix(c(1,-1,-1,-2,0,-1,0,1),ncol = 2, byrow = TRUE)
  b.D.var <- c(0,-1,-xi.D[2]+e.D,xi.D[2]+e.D)
  D.var=list(Name="Diurnal Var",A=A.D.var,b=b.D.var,func="bf_multinom")     
  #################################
  #Diurnal-nocturnal Hypotheses
  # Threshold
  A.Dn.th <- matrix(c(2,1,-1,-2,0,-1,-1,-1),ncol = 2, byrow = TRUE)
  b.Dn.th <- c(1,-1,-xi.Dn[1],xi.Dn[2]-1)
  C.Dn.th <- matrix(c(1,0),ncol = 2, byrow = TRUE)
  d.Dn.th <- c(0)
  Dn.th=list(Name="Diurnal-nocturnal Threshold",A=A.Dn.th,b=b.Dn.th,C=C.Dn.th,d=d.Dn.th,func="bf_equality")     

  # Threshold - weak - no constraint on third probability being zero
  A.Dn.th.wk <- matrix(c(2,1,-1,-2,0,-1,-1,-1),ncol = 2, byrow = TRUE)
  b.Dn.th.wk <- c(1,-1,-xi.Dn[1],xi.Dn[2]-1)
  Dn.th.wk=list(Name="Diurnal-nocturnal Threshold (Weak)",A=A.Dn.th.wk,b=b.Dn.th.wk,func="bf_multinom")     

  
  # Maximizing
  A.Dn.max <- matrix(c(2,1,-1,-2),ncol = 2, byrow = TRUE)
  b.Dn.max <- c(1,-1)
  C.Dn.max <- matrix(c(1,0),ncol = 2, byrow = TRUE)
  d.Dn.max <- c(0)
  Dn.max=list(Name="Diurnal-nocturnal Max",A=A.Dn.max,b=b.Dn.max,C=C.Dn.max,d=d.Dn.max,func="bf_equality")     

  # Variability
  A.Dn.var <- matrix(c(2,1,-1,-2,0,-1,0,1,1,1,-1,-1),ncol = 2, byrow = TRUE)
  b.Dn.var <- c(1,-1, -xi.Dn[3]+e.Dn,xi.Dn[3]+e.Dn, -xi.Dn[4]+e.Dn+1,
                xi.Dn[4]+e.Dn-1)
  C.Dn.var <- matrix(c(1,0),ncol = 2, byrow = TRUE)
  d.Dn.var <- c(0)
  Dn.var=list(Name="Diurnal-nocturnal Var",A=A.Dn.var,b=b.Dn.var,C=C.Dn.var,d=d.Dn.var,func="bf_equality")     
  
  # Variability - weak - no constraint on third probability being zero
  A.Dn.var.wk <- matrix(c(2,1,-1,-2,0,-1,0,1,1,1,-1,-1),ncol = 2, byrow = TRUE)
  b.Dn.var.wk <- c(1,-1, -xi.Dn[3]+e.Dn,xi.Dn[3]+e.Dn, -xi.Dn[4]+e.Dn+1,
                xi.Dn[4]+e.Dn-1)
  Dn.var.wk=list(Name="Diurnal-nocturnal Var (Weak)",A=A.Dn.var.wk,b=b.Dn.var.wk,func="bf_multinom")     
  
  
  # #################################
  # #Diurnal-cathemeral Hypotheses
  # Threshold
   A.Dc.th <- matrix(c(-1,-2,1,-1,1,1,-1,0,0,-1),ncol = 2, byrow = TRUE)
   b.Dc.th <- c(-1,0,-xi.Dc[2]+1,-xi.Dc[2],-xi.Dc[1])
   C.Dc.th <- matrix(c(-2,-1),ncol = 2, byrow = TRUE)
   d.Dc.th <- c(-1)
   Dc.th=list("Diurnal-cathemeral Threshold",A=A.Dc.th,b=b.Dc.th,C=C.Dc.th,d=d.Dc.th,func="bf_equality")     
  # 
  # # Maximizing
  # A.Dc.max <- matrix(c(1,-1,-1,-2, -1, 0,1,1),ncol = 2, byrow = TRUE)
  # b.Dc.max <- c(0,-1,-xi.Dc[2],xi.Dc[2]+1)
  # Dc.max=list("Diurnal-cathemeral Max",A.Dc.max,b.Dc.max)     
  # 
  # # Variability
  # A.Dc.var <- matrix(c(1,-1,-1,-2,-1,0,1,1,0,1,0,-1,1,0,-1,0,-1,-1,1,1),ncol = 2, byrow = TRUE)
  # b.Dc.var <- c(0,-1, -xi.Dc[2],xi.Dc[2]+1, xi.Dc[3]+e.Dc,
  #               -xi.Dc[3]+e.Dc,xi.Dc[4]+e.Dc,-xi.Dc[4]+e.Dc,xi.Dc[4]+e.Dc-1,
  #               -xi.Dc[4]+e.Dc+1)
  # Dc.var=list("Diurnal-cathemeral Var",A.Dc.var,b.Dc.var)     
  # 
  #################################
  #Diurnal-crepuscular Hypotheses
  # Threshold
  A.Dcr.th <- matrix(c(-2,-1,1,-1,0,-1,1,0),ncol = 2, byrow = TRUE)
  b.Dcr.th <- c(-1,0,-xi.Dcr[1],xi.Dcr[2])
  C.Dcr.th <- matrix(c(-1,-1),ncol = 2, byrow = TRUE)
  d.Dcr.th <- c(-1)
  Dcr.th=list(Name="Diurnal-crepuscular Threshold",A=A.Dcr.th,b=b.Dcr.th,C=C.Dcr.th,d=d.Dcr.th,func="bf_equality")     
  
  # Threshold - weak - no constraint on third probability being zero
  A.Dcr.th.wk <- matrix(c(-2,-1,1,-1,0,-1,1,0),ncol = 2, byrow = TRUE)
  b.Dcr.th.wk <- c(-1,0,-xi.Dcr[1],xi.Dcr[2])
  Dcr.th.wk=list(Name="Diurnal-crepuscular Threshold (Weak)",A=A.Dcr.th.wk,b=b.Dcr.th.wk, func="bf_multinom")
  
  # Maximizing
  A.Dcr.max <- matrix(c(-2,-1, 1,-1),ncol = 2, byrow = TRUE)
  b.Dcr.max <- c(-1,0)
  C.Dcr.max <- matrix(c(-1,-1),ncol = 2, byrow = TRUE)
  d.Dcr.max <- c(-1)
  Dcr.max=list(Name="Diurnal-crepuscular Max",A=A.Dcr.max,b=b.Dcr.max,C=C.Dcr.max,d=d.Dcr.max,func="bf_equality")     
  
  # Variability
  A.Dcr.var <- matrix(c(-2,-1,1,-1,0,1,0,-1,-1,0, 1,0),ncol = 2, byrow = TRUE)
  b.Dcr.var <- c(-1, 0,xi.Dcr[3]+e.Dcr,-xi.Dcr[3]+e.Dcr,-xi.Dcr[4]+e.Dcr, xi.Dcr[4]+e.Dcr)
  C.Dcr.var <- matrix(c(-1,-1),ncol = 2, byrow = TRUE)
  d.Dcr.var <- c(-1)
  Dcr.var=list(Name="Diurnal-crepuscular Var",A=A.Dcr.var,b=b.Dcr.var,C=C.Dcr.var,d=d.Dcr.var,func="bf_equality")     
  
  # Variability - weak - no constrain o third probability being zero
  A.Dcr.var.wk <- matrix(c(-2,-1,1,-1,0,1,0,-1,-1,0, 1,0),ncol = 2, byrow = TRUE)
  b.Dcr.var.wk <- c(-1, 0,xi.Dcr[3]+e.Dcr,-xi.Dcr[3]+e.Dcr,-xi.Dcr[4]+e.Dcr, xi.Dcr[4]+e.Dcr)
  Dcr.var.wk=list(Name="Diurnal-crepuscular Var (Weak)",A=A.Dcr.var,b=b.Dcr.var, funct="bf_multinom")     
  
  
  #################################
  #################################
  #Nocturnal Hypotheses
  # Threshold
  A.N.th <- matrix(c(1,2,2,1,1,1),ncol = 2, byrow = TRUE)
  b.N.th <- c(1,1,-xi.N[1]+1)
  N.th=list(Name="Nocturnal Threshold",A=A.N.th,b=b.N.th,func="bf_multinom")     
  
  # Maximizing
  A.N.max <- matrix(c(1,2,2,1),ncol = 2, byrow = TRUE)
  b.N.max <- c(1,1)
  N.max=list(Name="Nocturnal Max",A=A.N.max,b=b.N.max,func="bf_multinom")     
  
  # Variability
  A.N.var <- matrix(c(1,2,2,1,-1,-1,1,1),ncol = 2, byrow = TRUE)
  b.N.var <- c(1, 1,xi.N[2]+e.N-1,-xi.N[2]+e.N+1)
  N.var=list(Name="Nocturnal Var",A=A.N.var,b=b.N.var,func="bf_multinom")     
  
  #################################
  #Nocturnal-diurnal Hypotheses
  # Threshold
  A.Nd.th <- matrix(c(1,2,1,-1,1,1,0,1),ncol = 2, byrow = TRUE)
  b.Nd.th <- c(1,0,-xi.Nd[1]+1,xi.Nd[2])
  C.Nd.th <- matrix(c(1,0),ncol = 2, byrow = TRUE)
  d.Nd.th <- c(0)
  Nd.th=list(Name="Nocturnal-diurnal Threshold",A=A.Nd.th,b=b.Nd.th,C=C.Nd.th,d=d.Nd.th,func="bf_equality")     
  
  # Threshold - weak - no constrain on third probability being zero
  A.Nd.th.wk <- matrix(c(1,2,1,-1,1,1,0,1),ncol = 2, byrow = TRUE)
  b.Nd.th.wk <- c(1,0,-xi.Nd[1]+1,xi.Nd[2])
  Nd.th.wk=list(Name="Nocturnal-diurnal Threshold",A=A.Nd.th.wk,b=b.Nd.th.wk,func="bf_multinom")     

  
  # Maximizing
  A.Nd.max <- matrix(c(1,2,1,-1),ncol = 2, byrow = TRUE)
  b.Nd.max <- c(1,0)
  C.Nd.max <- matrix(c(1,0),ncol = 2, byrow = TRUE)
  d.Nd.max <- c(0)
  Nd.max=list(Name="Nocturnal-diurnal Max",A=A.Nd.max,b=b.Nd.max,C=C.Nd.max,d=d.Nd.max,func="bf_equality")     
  
  # Variability
  A.Nd.var <- matrix(c(1,2,1,-1,-1,-1,1,1,0,1,0,-1),ncol = 2, byrow = TRUE)
  b.Nd.var <- c(1,0,xi.Nd[3]+e.Nd-1,-xi.Nd[3]+e.Nd+1,xi.Nd[3]+e.Nd,-xi.Nd[4]+e.Nd)
  C.Nd.var <- matrix(c(1,0),ncol = 2, byrow = TRUE)
  d.Nd.var <- c(0)
  Nd.var=list(Name="Nocturnal-diurnal Var",A.Nd.var,b.Nd.var,C=C.Nd.var,d=d.Nd.var,func="bf_equality")     
  
  # Variability - weak - no constraint on third probability being zero
  A.Nd.var.wk <- matrix(c(1,2,1,-1,-1,-1,1,1,0,1,0,-1),ncol = 2, byrow = TRUE)
  b.Nd.var.wk <- c(1,0,xi.Nd[3]+e.Nd-1,-xi.Nd[3]+e.Nd+1,xi.Nd[3]+e.Nd,-xi.Nd[4]+e.Nd)
  Nd.var.wk=list(Name="Nocturnal-diurnal Var (Weak)",A=A.Nd.var.wk,b=b.Nd.var.wk)
  
  #################################
  #Nocturnal-cathemeral Hypotheses
  # Threshold
  A.Nc.th <- matrix(c(1,2,2,1,-1,0,0,-1,1,1),ncol = 2, byrow = TRUE)
  b.Nc.th <- c(1,1,-xi.Nc[2],-xi.Nc[2],-xi.Nc[1]+1)
  C.Nc.th <- matrix(c(-1,1),ncol = 2, byrow = TRUE)
  d.Nc.th <- c(0)
  Nc.th=list("Nocturnal-cathemeral Threshold",A=A.Nc.th,b=b.Nc.th,C=C.Nc.th,d=d.Nc.th,func="bf_equality")     
  # 
  # # Maximizing
  # A.Nc.max <- matrix(c(1,2,2,1,0,-1,-1,0),ncol = 2, byrow = TRUE)
  # b.Nc.max <- c(1,1,-xi.Nc[2],-xi.Nc[2])
  # Nc.max=list("Nocturnal-cathemeral Max",A.Nc.max,b.Nc.max)     
  # 
  # # Variability
  # A.Nc.var <- matrix(c(1,2,2,1,0,-1,-1,0,1,1,-1,-1,-1,0,1,0),ncol = 2, byrow = TRUE)
  # b.Nc.var <- c(1,1,-xi.Nc[2],-xi.Nc[2],-xi.Nc[3]+e.Nc+1,xi.Nc[3]+e.Nc-1,
  #               -xi.Nc[4]+e.Nc,xi.Nc[4]+e.Nc)
  # Nc.var=list("Nocturnal-cathemeral Var",A.Nc.var,b.Nc.var)     
  # 
  #################################
  #Nocturnal-crepuscular Hypotheses
  # Threshold
  A.Ncr.th <- matrix(c(-1,1,2,1,1,1,1,0),ncol = 2, byrow = TRUE)
  b.Ncr.th <- c(0,1,xi.Ncr[1]+1,xi.Ncr[2])
  C.Ncr.th <- matrix(c(0,1),ncol = 2, byrow = TRUE)
  d.Ncr.th <- c(0)
  Ncr.th=list(Name="Nocturnal-crepuscular Threshold",A=A.Ncr.th,b=b.Ncr.th,C=C.Ncr.th,d=d.Ncr.th,func="bf_equality")     
  
  # Threshold - weak - no constraint on third probabilty being zero
  A.Ncr.th.wk <- matrix(c(-1,1,2,1,1,1,1,0),ncol = 2, byrow = TRUE)
  b.Ncr.th.wk <- c(0,1,xi.Ncr[1]+1,xi.Ncr[2])
  Ncr.th.wk=list(Name="Nocturnal-crepuscular Threshold (Weak)",A=A.Ncr.th.wk,b=b.Ncr.th.wk,func="bf_multinom")     
  
  # Maximizing
  A.Ncr.max <- matrix(c(-1,1,2,1),ncol = 2, byrow = TRUE)
  b.Ncr.max <- c(0,1)
  C.Ncr.max <- matrix(c(0,1),ncol = 2, byrow = TRUE)
  d.Ncr.max <- c(0)
  Ncr.max=list(Name="Nocturnal-crepuscular Max",A=A.Ncr.max,b=b.Ncr.max,C=C.Ncr.max,d=d.Ncr.max,func="bf_equality")     
  
  # Variability
  A.Ncr.var <- matrix(c(-1,1,2,1,-1,-1,1,1,1,0,-1,0),ncol = 2, byrow = TRUE)
  b.Ncr.var <- c(0,1,xi.Ncr[3]+e.Ncr-1,-xi.Ncr[3]+e.Ncr+1,xi.Ncr[4]+e.Ncr,-xi.Ncr[4]+e.Ncr)
  C.Ncr.var <- matrix(c(0,1),ncol = 2, byrow = TRUE)
  d.Ncr.var <- c(0)
  Ncr.var=list(Name="Nocturnal-crepuscular Var",A=A.Ncr.var,b=b.Ncr.var,C=C.Ncr.var,d=d.Ncr.var,func="bf_equality")          

  # Variability - weak - no constraint on the third probabilty being zero
  A.Ncr.var.wk <- matrix(c(-1,1,2,1,-1,-1,1,1,1,0,-1,0),ncol = 2, byrow = TRUE)
  b.Ncr.var.wk <- c(0,1,xi.Ncr[3]+e.Ncr-1,-xi.Ncr[3]+e.Ncr+1,
                xi.Ncr[4]+e.Ncr,-xi.Ncr[4]+e.Ncr)
  Ncr.var.wk=list(Name="Nocturnal-crepuscular Var (weak)",A=A.Ncr.var.wk,b=b.Ncr.var.wk, func="bf_multinom")     

  #################################
  #################################
  #Crepuscular Hypotheses
  # Threshold
  A.CR.th <- matrix(c(-1,1,-2,-1,-1,0),ncol = 2, byrow = TRUE)
  b.CR.th <- c(0,-1,-xi.CR[1])
  CR.th=list(Name="Crepuscular Threshold",A=A.CR.th,b=b.CR.th,func="bf_multinom")     
  
  # Maximizing
  A.CR.max <- matrix(c(-1,1,-2,-1),ncol = 2, byrow = TRUE)
  b.CR.max <- c(0,-1)
  CR.max=list(Name="Crepuscular Max",A=A.CR.max,b=b.CR.max,func="bf_multinom")     
  
  # Variability
  A.CR.var <- matrix(c(-1,1,-2,-1,-1,0,1,0),ncol = 2, byrow = TRUE)
  b.CR.var <- c(0,-1,-xi.CR[2]+e.CR,xi.CR[2]+e.CR)
  CR.var=list(Name="Crepuscular Var",A=A.CR.var,b=b.CR.var,func="bf_multinom")     
  
  #################################
  #Crepuscular-diurnal Hypotheses
  # Threshold
  A.CRd.th <- matrix(c(-1,-2,-1,1,-1,0,0,1),ncol = 2, byrow = TRUE)
  b.CRd.th <- c(-1,0,-xi.CRd[1],xi.CRd[2])
  C.CRd.th <- matrix(c(-1,-1),ncol = 2, byrow = TRUE)
  d.CRd.th <- c(-1)
  CRd.th=list(Name="Crepuscular-diurnal Threshold",A=A.CRd.th,b=b.CRd.th,C=C.CRd.th,d=d.CRd.th,func="bf_equality")     
  
  # Threshold - weak - no constraints on the third probability
  A.CRd.th.wk <- matrix(c(-1,-2,-1,1,-1,0,0,1),ncol = 2, byrow = TRUE)
  b.CRd.th.wk <- c(-1,0,-xi.CRd[1],xi.CRd[2])
  CRd.th.wk=list(Name="Crepuscular-diurnal Threshold (Weak)",A=A.CRd.th.wk,b=b.CRd.th.wk, func="bf_multinom")     

  # Maximizing
  A.CRd.max <- matrix(c(-1,-2,-1,1),ncol = 2, byrow = TRUE)
  b.CRd.max <- c(-1,0)
  C.CRd.max <- matrix(c(-1,-1),ncol = 2, byrow = TRUE)
  d.CRd.max <- c(-1)
  CRd.max=list(Name="Crepuscular-diurnal Max",A=A.CRd.max,b=b.CRd.max,C=C.CRd.max,d=d.CRd.max,func="bf_equality")     
  
  # Variability
  A.CRd.var <- matrix(c(-1,-2,-1,1,-1,0,1,0,0,-1,0,1),ncol = 2, byrow = TRUE)
  b.CRd.var <- c(-1, 0,-xi.CRd[3]+e.CRd,xi.CRd[3]+e.CRd,-xi.CRd[4]+e.CRd,xi.CRd[4]+e.CRd)
  C.CRd.var <- matrix(c(-1,-1),ncol = 2, byrow = TRUE)
  d.CRd.var <- c(-1)
  CRd.var=list(Name="Crepuscular-diurnal Var",A=A.CRd.var,b=b.CRd.var,C=C.CRd.var,d=d.CRd.var,func="bf_equality")     
  
  # Variability - weak - no constrain on the third probability being zero
  A.CRd.var.wk <- matrix(c(-1,-2,-1,1,-1,0,1,0,0,-1,0,1),ncol = 2, byrow = TRUE)
  b.CRd.var.wk <- c(-1, 0,-xi.CRd[3]+e.CRd,xi.CRd[3]+e.CRd,-xi.CRd[4]+e.CRd,xi.CRd[4]+e.CRd)
  CRd.var.wk=list(Name="Crepuscular-diurnal Var (Weak)",A=A.CRd.var.wk,b=b.CRd.var.wk, func="bf_multinom")     

  #################################
  #Crepuscular-nocturnal Hypotheses
  # Threshold
  A.CRn.th <- matrix(c(1,2,-2,-1,-1,0,-1,-1),ncol = 2, byrow = TRUE)
  b.CRn.th <- c(1,-1,xi.CRn[1],xi.CRn[2]-1)
  C.CRn.th <- matrix(c(0,1),ncol = 2, byrow = TRUE)
  d.CRn.th <- c(0)
  CRn.th=list(Name="Crepuscular-nocturnal Threshold",A=A.CRn.th,b=b.CRn.th,C=C.CRn.th,d=d.CRn.th,func="bf_equality")     
  
  # Threshold
  A.CRn.th.wk <- matrix(c(1,2,-2,-1,-1,0,-1,-1),ncol = 2, byrow = TRUE)
  b.CRn.th.wk <- c(1,-1,xi.CRn[1],xi.CRn[2]-1)
  CRn.th.wk=list(Name="Crepuscular-nocturnal Threshold (Weak)",A=A.CRn.th.wk,b=b.CRn.th.wk, func="bf_multinom")
  
  # Maximizing
  A.CRn.max <- matrix(c(1,2,-2,-1),ncol = 2, byrow = TRUE)
  b.CRn.max <- c(1,-1)
  C.CRn.max <- matrix(c(0,1),ncol = 2, byrow = TRUE)
  d.CRn.max <- c(0)
  CRn.max=list(Name="Crepuscular-nocturnal Max",A=A.CRn.max,b=b.CRn.max,C=C.CRn.max,d=d.CRn.max,func="bf_equality")     
  
  # Variability
  A.CRn.var <- matrix(c(1,2,-2,-1,-1,0,1,0,1,1,-1,-1),ncol = 2, byrow = TRUE)
  b.CRn.var <- c(1,-1,-xi.CRn[3]+e.CRn,xi.CRn[3]+e.CRn,-xi.CRn[4]+e.CRn+1,xi.CRn[4]+e.CRn-1)
  C.CRn.var <- matrix(c(0,1),ncol = 2, byrow = TRUE)
  d.CRn.var <- c(0)
  CRn.var=list(Name="Crepuscular-nocturnal Var",A=A.CRn.var,b=b.CRn.var,C=C.CRn.var,d=d.CRn.var,func="bf_equality")     
  
  # Variability - weak - no constraint on the third probabilty being zero
  A.CRn.var.wk <- matrix(c(1,2,-2,-1,-1,0,1,0,1,1,-1,-1),ncol = 2, byrow = TRUE)
  b.CRn.var.wk <- c(1,-1,-xi.CRn[3]+e.CRn,xi.CRn[3]+e.CRn,-xi.CRn[4]+e.CRn+1,xi.CRn[4]+e.CRn-1)
  CRn.var.wk=list(Name="Crepuscular-nocturnal Var (Weak)",A=A.CRn.var.wk,b=b.CRn.var.wk, func="bf_multinom")     

  #################################
  #Crepuscular-cathemeral Hypotheses
  # Threshold
  A.CRc.th <- matrix(c(-1,1,-2,-1,1,1,0,-1,-1,0),ncol = 2, byrow = TRUE)
  b.CRc.th <- c(0,-1,-xi.CRc[2]+1,-xi.CRc[2],-xi.CRc[1])
  C.CRc.th <- matrix(c(-1,-2),ncol = 2, byrow = TRUE)
  d.CRc.th <- c(-1)

  CRc.th=list("Crepuscular-cathemeral Threshold",A=A.CRc.th,b=b.CRc.th,C=C.CRc.th,d=d.CRc.th,func="bf_equality")     
  # 
  # # Maximizing
  # A.CRc.max <- matrix(c(-1,1,-2,-1,0,-1,1,1),ncol = 2, byrow = TRUE)
  # b.CRc.max <- c(0,-1,-xi.CRc[2],-xi.CRc[2]+1)
  # CRc.max=list("Crepuscular-cathemeral Max",A.CRc.max,b.CRc.max)     
  # 
  # # Variability
  # A.CRc.var <- matrix(c(-1,1,-2,-1,0,-1,1,1,-1,0,1,0,1,1,-1,-1,0,-1,0,1),ncol = 2, byrow = TRUE)
  # b.CRc.var <- c(0,-1,-xi.CRc[2],-xi.CRc[2]+1,-xi.CRc[3]+e.CRc,
  #                xi.CRc[3]+e.CRc,-xi.CRc[4]+e.CRc+1,xi.CRc[4]+e.CRc-1,
  #                -xi.CRc[4]+e.CRc,xi.CRc[4]+e.CRc)
  # CRc.var=list("Crepuscular-cathemeral Var",A.CRc.var,b.CRc.var)     
  # 
  #################################
  #################################
  #Even Cathemeral Hypotheses - Equality Hypothesis
  #A just forces p1 and p2 to be less than 1. Allows for consistency in code execution indiel.bf
  A.EC <- matrix(c(1,0,0,1),ncol = 2, byrow = TRUE)
  b.EC <- c(1,1)
  C.EC <- matrix(c(1,0,0,1),ncol = 2, byrow = TRUE)
  d.EC <- c(0.3333,0.3333)
  EC=list(Name="Even Cathemeral Equality",A=A.EC,b=b.EC,C=C.EC,d=d.EC,func="bf_equality") 

  
  # Threshold
  A.EC.th <- matrix(c(-1,-1,0,1,1,0,1,1,0,-1,-1,0),ncol = 2, byrow = TRUE)
  b.EC.th <- c(0.43-1,0.43,0.43,-0.23+1,-0.23,-0.23)
  EC.th=list(Name="Even Cathemeral Threshold",A=A.EC.th,b=b.EC.th,func="bf_multinom")     
  
  # Maximizing
  # NA
  
  # Variation
  A.EC.var <- matrix(c(1,0,-1,0,0,1,0,-1,-1,-1,1,1),ncol = 2, byrow = TRUE)
  b.EC.var <- c(xi.EC[1]+e.EC,-xi.EC[1]+e.EC,xi.EC[1]+e.EC,
                -xi.EC[1]+e.EC,xi.EC[1]+e.EC-1,-xi.EC[1]+e.EC+1)
  EC.var=list(Name="Even Cathemeral Var",A=A.EC.var,b=b.EC.var,func="bf_multinom")     
  
  #################################
  #################################
  #Available Cathemeral Hypotheses - Equality Hypothesis
  #A just forces p1 and p2 to be less than 1. Allows for consistency in code execution in diel.bf
  A.AC <- matrix(c(1,0,0,1),ncol = 2, byrow = TRUE)
  b.AC <- c(1,1)
  C.AC <- matrix(c(1,0,0,1),ncol = 2, byrow = TRUE)
  d.AC <- c(p.avail[1],p.avail[2])
  AC=list(Name="Available Cathemeral Equality",A=A.AC,b=b.AC,C=C.AC,d=d.AC,func="bf_equality") 

  # Threshold
  # NA  
  
  # Maximizing
  # NA
  
  # Variation
  A.AC.var <- matrix(c(-1/p.avail[1],0,
                       1/p.avail[1],0,
                       0,-1/p.avail[2],
                       0,1/p.avail[2],
                       1/(1-sum(p.avail)),1/(1-sum(p.avail)),
                       -1/(1-sum(p.avail)),-1/(1-sum(p.avail))),ncol = 2, byrow = TRUE)
  b.AC.var <- c(-1+e.AC,1+e.AC,-1+e.AC,1+e.AC,
                -1+e.AC+(1/(1-sum(p.avail))),1+e.AC-(1/(1-sum(p.avail))))
  AC.var=list(Name="Available Cathemeral Var",A=A.AC.var,b=b.AC.var,func="bf_equality")     
#################################
#################################
#General Cathemeral Hypotheses

# Threshold
  A.C.th <- matrix(c(1,1,
                    0, -1,
                    -1, 0),ncol = 2, byrow = TRUE)
  b.C.th <- c(-0.2+1,-0.2,-0.2)
  C.th=list(Name="Cathemeral Threshold",A=A.C.th,b=b.C.th,func="bf_multinom")     


  
# Maximizing
# NA
  
# Variation
##################################################

  ##################################################
  #Package Inputs
  inputs=list(e.D=e.D, e.Dn=e.Dn, e.Dcr=e.Dcr, 
              e.N=e.N, e.Nd=e.Nd, e.Ncr=e.Ncr, 
              e.CR=e.CR, e.CRd=e.CRd, e.CRn=e.CRn,
              e.EC=e.EC, e.AC=e.AC,
              xi.D=xi.D, xi.Dn=xi.Dn, xi.Dcr=xi.Dcr,
              xi.N=xi.N,xi.Nd=xi.Nd,xi.Ncr=xi.Ncr,
              xi.CR=xi.CR,xi.CRd=xi.CRd,xi.CRn=xi.CRn,xi.CRc=xi.CRc,
              xi.Dc=xi.Dc,xi.Nc=xi.Nc,
              xi.EC=xi.EC, p.avail=p.avail)  
  
  #package outputs  
  diel.hyp=list(   
    D.th=D.th, D.max=D.max,D.var=D.var,
    Dn.th=Dn.th,Dn.max=Dn.max,Dn.var=Dn.var,
    Dcr.th=Dcr.th,Dcr.max=Dcr.max,Dcr.var=Dcr.var,
    N.th =N.th,N.max=N.max,N.var=N.var,
    Nd.th=Nd.th, Nd.max=Nd.max,Nd.var=Nd.var,
    Ncr.th=Ncr.th,Ncr.max=Ncr.max,Ncr.var=Ncr.var,
    CR.th=CR.th,CR.max=CR.max,CR.var=CR.var,
    CRd.th=CRd.th,CRd.max=CRd.max,CRd.var=CRd.var,
    CRn.th=CRn.th,CRn.max=CRn.max,CRn.var=CRn.var,
    EC.th=EC.th,EC.var=EC.var,AC.var=AC.var, C.th=C.th,
    AC=AC,EC=EC,Dc.th=Dc.th,Nc.th=Nc.th,CRc.th=CRc.th,
    Dn.th.wk=Dn.th.wk,Dn.var.wk=Dn.var.wk,Dcr.th.wk=Dcr.th.wk,
    Dcr.var.wk=Dcr.var.wk,Nd.th.wk=Nd.th.wk,Nd.var.wk=Nd.var.wk,
    Ncr.var.wk=Ncr.var.wk,CRd.th.wk=CRd.th.wk,CRn.var.wk=CRn.var.wk,
    Ncr.th.wk=Ncr.th.wk,CRn.th.wk=CRn.th.wk,Uncon=Uncon,
    N=N,C=C,CR=CR,D=D,
     C.full=C.full, CR.full=CR.full,  DCR.full=DCR.full,NCR.full=NCR.full,
     CRc.full=CRc.full,CRd.full=CRd.full,CRn.full=CRn.full, D.full=D.full,  DN.full=DN.full,
     Dc.full=Dc.full, Dcr.full=Dcr.full,Dn.full=Dn.full, N.full=N.full,  
     Nc.full=Nc.full, Ncr.full=Ncr.full, Nd.full=Nd.full,
    inputs=inputs
    )
  
  #output from function
  diel.hyp
  
  
} #end function 
