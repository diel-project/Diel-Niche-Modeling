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
#' @param xi.Dcr Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Diurnal-crepuscular hypothesis.
#' @param xi.N  Default c(0.80,0.90). A vector of the lower threshold value and most likely value,, respectively for the Nocturnal hypothesis.
#' @param xi.Nd  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Nocturnal-diurnal hypothesis.
#' @param xi.Ncr  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Nocturnal-crepuscular hypothesis.
#' @param xi.CR Default c(0.80,0.90). A vector of the lower threshold value and most likely value,, respectively for the Crepuscular hypothesis.
#' @param xi.CRd  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Nocturnal-crepuscular hypothesis.
#' @param xi.CRn  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Crepuscular-nocturnal hypothesis.
#' @param xi.EC Default c(0.33). A single value of the available amount of time in all three diel periods.
#' @param p.avail Default c(0.166666,0.4166667). A vector of the available time in the periods of crepuscular and diurnal. Nighttime availability is found by subtraction.
#' 
#' @return diel.hyp A list of diel hypotheses as multinomial inequalities.
#' \item{inputs}{Includes all inputted values; epsilon, xi, and p.avail.} 
#' \item{D.th, D.max, D.var, Dn.th, Dn.max, Dn.var, Dc.max, Dcr.th,
#' Dcr.max,Dcr.var, N.th, N.max, N.var, Nd.th, Nd.max, Nd.var, 
#'  Nc.var, Ncr.th, Ncr.max, Ncr.var, CR.th, CR.max, CR.var,CRd.th,
#' CRd.max, CRd.var, CRn.th, CRn.max, CRn.var,'EC.th, EC.var, AC.var, C.th}{Each is a list of three elements: Hypotheis Descriptive Name, A matrix, and b vector.} 
#' @examples 
#' diel.ineq()
#' diel.ineq(e=0.01) #To replace all epsilon values with 0.01.
#' diel.ineq(e.Dn=0.2, xi.Dn=c(0.80,0.20,0.90,0.10)) #To replace teh default values with a new epsilon value and a new xi value.
#' @export
############################
#To do
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

# xi.Nd - (Nocturnal-dirunal Hyp.; length 4)  
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
                   xi.CR=NULL,xi.CRd=NULL,xi.CRn=NULL,
                   xi.EC=NULL, p.avail=NULL){
  
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
  
  if(is.null(p.avail)){p.avail  = c(0.166666,0.4166667)}
  
  
  soft.zero=0.005
  
  #################################
  #################################
  #Diurnal Hypotheses
  # Threshold
  A.D.th <- matrix(c(1,-1,-1,-2,0,-1),ncol = 2, byrow = TRUE)
  b.D.th <- c(0,-1,-xi.D[1])
  D.th=list(Name="Diurnal Threshold",A=A.D.th,b=b.D.th)
  # Maximizing
  A.D.max <- matrix(c(1,-1,-1,-2),ncol = 2, byrow = TRUE)
  b.D.max <- c(0,-1)
  D.max=list(Name="Diurnal Max",A=A.D.max, b=b.D.max)     
  # Variability
  A.D.var <- matrix(c(1,-1,-1,-2,0,-1,0,1),ncol = 2, byrow = TRUE)
  
  b.D.var <- c(0,-1,-xi.D[2]+e.D,xi.D[2]+e.D)
  D.var=list(Name="Diurnal Var",A=A.D.var,b=b.D.var)     
  #################################
  #Diurnal-nocturnal Hypotheses
  # Threshold
  A.Dn.th <- matrix(c(2,1,-1,-2,0,-1,-1,-1,1,0),ncol = 2, byrow = TRUE)
  b.Dn.th <- c(1,-1,-xi.Dn[1],xi.Dn[2]-1,soft.zero)
  Dn.th=list(Name="Diurnal-nocturnal Threshold",A=A.Dn.th,b=b.Dn.th)     

  # Maximizing
  A.Dn.max <- matrix(c(2,1,-1,-2,1,0),ncol = 2, byrow = TRUE)
  b.Dn.max <- c(1,-1,soft.zero)
  Dn.max=list(Name="Diurnal-nocturnal Max",A=A.Dn.max,b=b.Dn.max)     

  A.Dn.max2 <- matrix(c(2,1,-1,-2),ncol = 2, byrow = TRUE)
  b.Dn.max2 <- c(1,-1)
  C.Dn.max2 <- matrix(c(1,0),ncol = 2, byrow = TRUE)
  d.Dn.max2 <- c(0)
  Dn.max2=list(Name="Diurnal-nocturnal Max 2",A=A.Dn.max2,b=b.Dn.max2,C=C.Dn.max2,d=d.Dn.max2 )     

    
  # Variability
  A.Dn.var <- matrix(c(2,1,-1,-2,0,-1,0,1,1,1,-1,-1,1,0),ncol = 2, byrow = TRUE)
  b.Dn.var <- c(1,-1, -xi.Dn[3]+e.Dn,xi.Dn[3]+e.Dn, -xi.Dn[4]+e.Dn+1,
                xi.Dn[4]+e.Dn-1,soft.zero)
  Dn.var=list(Name="Diurnal-nocturnal Var",A=A.Dn.var,b=b.Dn.var)     
  
  # #################################
  # #Diurnal-cathemeral Hypotheses
  # # Threshold
  # A.Dc.th <- matrix(c(1,-1,-1,-2,-1,0,1,1,0,-1),ncol = 2, byrow = TRUE)
  # b.Dc.th <- c(0,-1,xi.Dc[2],1+xi.Dc[2],-xi.Dc[1])
  # Dc.th=list("Diurnal-cathemeral Threshold",A.Dc.th,b.Dc.th)     
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
  A.Dcr.th <- matrix(c(-2,-1,1,-1,0,-1,1,0,-1,-1),ncol = 2, byrow = TRUE)
  b.Dcr.th <- c(-1,0,-xi.Dcr[1],xi.Dcr[2],soft.zero-1)
  Dcr.th=list(Name="Diurnal-crepuscular Threshold",A=A.Dcr.th,b=b.Dcr.th)     
  
  # Maximizing
  A.Dcr.max <- matrix(c(-2,-1, 1,-1,-1,-1),ncol = 2, byrow = TRUE)
  b.Dcr.max <- c(-1,0,soft.zero-1)
  Dcr.max=list(Name="Diurnal-crepuscular Max",A=A.Dcr.max,b=b.Dcr.max)     
  
  # Variability
  A.Dcr.var <- matrix(c(-2,-1,1,-1,0,1,0,-1,-1,0, 1,0,-1,-1),ncol = 2, byrow = TRUE)
  b.Dcr.var <- c(-1, 0,xi.Dcr[3]+e.Dcr,-xi.Dcr[3]+e.Dcr,-xi.Dcr[4]+e.Dcr, xi.Dcr[4]+e.Dcr, soft.zero-1)
  Dcr.var=list(Name="Diurnal-crepuscular Var",A=A.Dcr.var,b=b.Dcr.var)     
  
  #################################
  #################################
  #Nocturnal Hypotheses
  # Threshold
  A.N.th <- matrix(c(1,2,2,1,1,1),ncol = 2, byrow = TRUE)
  b.N.th <- c(1,1,-xi.N[1]+1)
  N.th=list(Name="Nocturnal Threshold",A=A.N.th,b=b.N.th)     
  
  # Maximizing
  A.N.max <- matrix(c(1,2,2,1),ncol = 2, byrow = TRUE)
  b.N.max <- c(1,1)
  N.max=list(Name="Nocturnal Max",A=A.N.max,b=b.N.max)     
  
  # Variability
  A.N.var <- matrix(c(1,2,2,1,-1,-1,1,1),ncol = 2, byrow = TRUE)
  b.N.var <- c(1, 1,xi.N[2]+e.N-1,-xi.N[2]+e.N+1)
  N.var=list(Name="Nocturnal Var",A=A.N.var,b=b.N.var)     
  
  #################################
  #Nocturnal-diurnal Hypotheses
  # Threshold
  A.Nd.th <- matrix(c(1,2,1,-1,1,1,0,1,1,0),ncol = 2, byrow = TRUE)
  b.Nd.th <- c(1,0,-xi.Nd[1]+1,xi.Nd[2],soft.zero)
  Nd.th=list(Name="Nocturnal-diurnal Threshold",A=A.Nd.th,b=b.Nd.th)     
  
  # Maximizing
  A.Nd.max <- matrix(c(1,2,1,-1,1,0),ncol = 2, byrow = TRUE)
  b.Nd.max <- c(1,0,soft.zero)
  Nd.max=list(Name="Nocturnal-diurnal Max",A=A.Nd.max,b=b.Nd.max)     
  
  # Variability
  A.Nd.var <- matrix(c(1,2,1,-1,-1,-1,1,1,0,1,0,-1,1,0),ncol = 2, byrow = TRUE)
  b.Nd.var <- c(1,0,xi.Nd[3]+e.Nd-1,-xi.Nd[3]+e.Nd+1,xi.Nd[3]+e.Nd,-xi.Nd[4]+e.Nd,soft.zero)
  Nd.var=list(Name="Nocturnal-diurnal Var",A.Nd.var,b.Nd.var)     
  
  #################################
  # #Nocturnal-cathemeral Hypotheses
  # # Threshold
  # A.Nc.th <- matrix(c(1,2,2,1,0,-1,-1,0,1,1),ncol = 2, byrow = TRUE)
  # b.Nc.th <- c(1,1,-xi.Nc[2],-xi.Nc[2],-xi.Nc[1]+1)
  # Nc.th=list("Nocturnal-cathemeral Threshold",A.Nc.th,b.Nc.th)     
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
  A.Ncr.th <- matrix(c(-1,1,2,1,1,1,1,0,0,1),ncol = 2, byrow = TRUE)
  b.Ncr.th <- c(0,1,xi.Ncr[1]+1,xi.Ncr[2],soft.zero)
  Ncr.th=list(Name="Nocturnal-crepuscular Threshold",A=A.Ncr.th,b=b.Ncr.th)     
  
  # Maximizing
  A.Ncr.max <- matrix(c(-1,1,2,1,0,1),ncol = 2, byrow = TRUE)
  b.Ncr.max <- c(0,1,soft.zero)
  Ncr.max=list(Name="Nocturnal-crepuscular Max",A=A.Ncr.max,b=b.Ncr.max)     
  
  # Variability
  A.Ncr.var <- matrix(c(-1,1,2,1,-1,-1,1,1,1,0,-1,0,0,1),ncol = 2, byrow = TRUE)
  b.Ncr.var <- c(0,1,xi.Ncr[3]+e.Ncr-1,-xi.Ncr[3]+e.Ncr+1,
                 xi.Ncr[4]+e.Ncr,-xi.Ncr[4]+e.Ncr,soft.zero)
  Ncr.var=list(Name="Nocturnal-crepuscular Var",A=A.Ncr.var,b=b.Ncr.var)     
  
  
  #################################
  #################################
  #Crepuscular Hypotheses
  # Threshold
  A.CR.th <- matrix(c(-1,1,-2,-1,-1,0),ncol = 2, byrow = TRUE)
  b.CR.th <- c(0,-1,-xi.CR[1])
  CR.th=list(Name="Crepuscular Threshold",A=A.CR.th,b=b.CR.th)     
  
  # Maximizing
  A.CR.max <- matrix(c(-1,1,-2,-1),ncol = 2, byrow = TRUE)
  b.CR.max <- c(0,-1)
  CR.max=list(Name="Crepuscular Max",A=A.CR.max,b=b.CR.max)     
  
  # Variability
  A.CR.var <- matrix(c(-1,1,-2,-1,-1,0,1,0),ncol = 2, byrow = TRUE)
  b.CR.var <- c(0,-1,-xi.CR[2]+e.CR,xi.CR[2]+e.CR)
  CR.var=list(Name="Crepuscular Var",A=A.CR.var,b=b.CR.var)     
  
  #################################
  #Crepuscular-diurnal Hypotheses
  # Threshold
  A.CRd.th <- matrix(c(-1,-2,-1,1,-1,0,0,1,-1,-1),ncol = 2, byrow = TRUE)
  b.CRd.th <- c(-1,0,-xi.CRd[1],xi.CRd[2],soft.zero-1)
  CRd.th=list(Name="Crepuscular-diurnal Threshold",A=A.CRd.th,b=b.CRd.th)     
  
  # Maximizing
  A.CRd.max <- matrix(c(-1,-2,-1,1,-1,-1),ncol = 2, byrow = TRUE)
  b.CRd.max <- c(-1,0,soft.zero-1)
  CRd.max=list(Name="Crepuscular-diurnal Max",A=A.CRd.max,b=b.CRd.max)     
  
  # Variability
  A.CRd.var <- matrix(c(-1,-2,-1,1,-1,0,1,0,0,-1,0,1,-1,-1),ncol = 2, byrow = TRUE)
  b.CRd.var <- c(-1, 0,-xi.CRd[3]+e.CRd,xi.CRd[3]+e.CRd,-xi.CRd[4]+e.CRd,xi.CRd[4]+e.CRd,soft.zero-1)
  CRd.var=list(Name="Crepuscular-diurnal Var",A.CRd.var,b.CRd.var)     
  
  #################################
  #Crepuscular-nocturnal Hypotheses
  # Threshold
  A.CRn.th <- matrix(c(1,2,-2,-1,-1,0,-1,-1,0,1),ncol = 2, byrow = TRUE)
  b.CRn.th <- c(1,-1,xi.CRn[1],xi.CRn[2]-1,soft.zero)
  CRn.th=list(Name="Crepuscular-nocturnal Threshold",A=A.CRn.th,b=b.CRn.th)     
  
  # Maximizing
  A.CRn.max <- matrix(c(1,2,-2,-1,0,1),ncol = 2, byrow = TRUE)
  b.CRn.max <- c(1,-1,soft.zero)
  CRn.max=list(Name="Crepuscular-nocturnal Max",A=A.CRn.max,b=b.CRn.max)     
  
  # Variability
  A.CRn.var <- matrix(c(1,2,-2,-1,-1,0,1,0,1,1,-1,-1,0,1),ncol = 2, byrow = TRUE)
  b.CRn.var <- c(1,-1,-xi.CRn[3]+e.CRn,xi.CRn[3]+e.CRn,-xi.CRn[4]+e.CRn+1,xi.CRn[4]+e.CRn-1,soft.zero)
  CRn.var=list(Name="Crepuscular-nocturnal Var",A=A.CRn.var,b=b.CRn.var)     
  
  #################################
  # #Crepuscular-cathemeral Hypotheses
  # # Threshold
  # A.CRc.th <- matrix(c(-1,1,-2,-1,0,-1,1,1,-1,0),ncol = 2, byrow = TRUE)
  # b.CRc.th <- c(0,-1,-xi.CRc[2],-xi.CRc[2]+1,-xi.CRc[1])
  # CRc.th=list("Crepuscular-cathemeral Threshold",A.CRc.th,b.CRc.th)     
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
  EC=list(Name="Even Cathemeral Equality2",A=A.EC,b=b.EC,C=C.EC,d=d.EC) 

  
  # Threshold
  A.EC.th <- matrix(c(-1,-1,0,1,1,0,1,1,0,-1,-1,0),ncol = 2, byrow = TRUE)
  b.EC.th <- c(0.43-1,0.43,0.43,-0.23+1,-0.23,-0.23)
  EC.th=list(Name="Even Cathemeral Threshold",A=A.EC.th,b=b.EC.th)     
  
  # Maximizing
  # NA
  
  # Variation
  A.EC.var <- matrix(c(1,0,-1,0,0,1,0,-1,-1,-1,1,1),ncol = 2, byrow = TRUE)
  b.EC.var <- c(xi.EC[1]+e.EC,-xi.EC[1]+e.EC,xi.EC[1]+e.EC,
                -xi.EC[1]+e.EC,xi.EC[1]+e.EC-1,-xi.EC[1]+e.EC+1)
  EC.var=list(Name="Even Cathemeral Var",A=A.EC.var,b=b.EC.var)     
  
  #################################
  #################################
  #Available Cathemeral Hypotheses - Equality Hypothesis
  #A just forces p1 and p2 to be less than 1. Allows for consistency in code execution indiel.bf
  A.AC <- matrix(c(1,0,0,1),ncol = 2, byrow = TRUE)
  b.AC <- c(1,1)
  
  C.AC <- matrix(c(1,0,0,1),ncol = 2, byrow = TRUE)
  d.AC <- c(p.avail[1],p.avail[2])
  AC=list(Name="Available Cathemeral Equality2",A=A.AC,b=b.AC,C=C.AC,d=d.AC) 

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
  AC.var=list(Name="Available Cathemeral Var",A=A.AC.var,b=b.AC.var)     
#################################
#################################
#General Cathemeral Hypotheses

# Threshold
  A.C.th <- matrix(c(1,1,
                       0, -1,
                       -1, 0),ncol = 2, byrow = TRUE)
  b.C.th <- c(-0.2+1,-0.2,-0.2)
  C.th=list(Name="Cathemeral Threshold",A=A.C.th,b=b.C.th)     


  
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
              xi.CR=xi.CR,xi.CRd=xi.CRd,xi.CRn=xi.CRn,
              xi.EC=xi.EC, p.avail=p.avail,soft.zero=soft.zero)  
  
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
    inputs=inputs, Dn.max2=Dn.max2, AC=AC,EC=EC
  )
  
  #output from function
  diel.hyp
  
  
} #end function 