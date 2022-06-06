#' Inequality Setup
#'
#' Multinomial model inequalities for diel hypotheses
#' @param e Default is 0.05. A single value of variation for probabilities. If specified, it will be applied to all hypotheses, regardless of whether individual epsilon hypotheses values are specified. 
#' @param e.D Default is 0.05. A single value of variation for the Diurnal hypothesis.
#' @param e.Dc Default is 0.05. A single value of variation for the Diurnal-cathemeral hypothesis.
#' @param e.Dcr Default is 0.05. A single value of variation for the Diurnal-crepuscular hypothesis.
#' @param e.N Default is 0.05. A single value of variation for the Nocturnal hypothesis.
#' @param e.Nd Default is 0.05. A single value of variation for the Nocturnal-diurnal hypothesis.
#' @param e.Nc Default is 0.05. A single value of variation for the Nocturnal-cathenerak hypothesis.
#' @param e.Ncr Default is 0.05. A single value of variation for the Nocturnal-crepuscular hypothesis.
#' @param e.CR Default is 0.05. A single value of variation for the Crepuscular hypothesis.
#' @param e.CRd Default is 0.05. A single value of variation for the Crepuscular-diurnal hypothesis.
#' @param e.CRn Default is 0.05. A single value of variation for the Crepuscular-nocturnal hypothesis.
#' @param e.CRc Default is 0.05. A single value of variation for the Crepuscular-cathemeral hypothesis.
#' @param e.EC  Default is 0.05. A single value of variation for the Evan Cathemeral hypothesis.
#' @param e.AC   Default is 0.10. A single value of variation for the Available Cathemeral hypothesis.
#' @param xi.D Default c(0.90,0.95). A vector of the lower threshold value and the most likely value, respectively for the Diurnal hypothesis.
#' @param xi.Dn Default c(0.80,0.20,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Diurnal-nocturnal hypothesis.
#' @param xi.Dc Default c(0.80,0.01,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Diurnal-cathemeral hypothesis.
#' @param xi.Dcr Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Diurnal-crepuscular hypothesis.
#' @param xi.N  Default c(0.80,0.90). A vector of the lower threshold value and most likely value,, respectively for the Nocturnal hypothesis.
#' @param xi.Nd  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Nocturnal-diurnal hypothesis.
#' @param xi.Nc   Default c(0.80,0.01,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Nocturnal-cathemeral hypothesis.
#' @param xi.Ncr  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Nocturnal-crepuscular hypothesis.
#' @param xi.CR Default c(0.80,0.90). A vector of the lower threshold value and most likely value,, respectively for the Crepuscular hypothesis.
#' @param xi.CRd  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Nocturnal-crepuscular hypothesis.
#' @param xi.CRn  Default c(0.80,0.2,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Crepuscular-nocturnal hypothesis.
#' @param xi.CRc Default c(0.80,0.01,0.90,0.10). A vector of the lower threshold value for the primary probability, upper threshold value of the secondary probability, most likely primary probability, and most likely secondary probability value, respectively for the Crepuscular-cathemeral hypothesis.
#' @param xi.EC Default c(0.33). A single value of the available amount of time in all three diel periods.
#' @param p.avail Default c(0.166666,0.4166667). A vector of the available time in the periods of crepuscular and diurnal. Nighttime availability is found by subtraction.
#' 
#' @return diel.hyp A list of diel hypotheses as multinomial inequalities.
#' \item{inputs}{Includes all inputted values; epsilon, xi, and p.avail.} 
#' \item{D.th, D.max, D.var, Dn.th, Dn.max, Dn.var, Dc.th, Dc.max, Dc.var, Dcr.th,
#' Dcr.max,Dcr.var, N.th, N.max, N.var, Nd.th, Nd.max, Nd.var, Nc.th,
#' Nc.max, Nc.var, Ncr.th, Ncr.max, Ncr.var, CR.th, CR.max, CR.var,CRd.th,
#' CRd.max, CRd.var, CRn.th, CRn.max, CRn.var, CRc.th, CRc.max, CRc.var,
#'EC.th, EC.var, AC.var, C.th}{Each is a list of three elements: Hypotheis Descriptive Name, A matrix, and b vector.} 
#' @examples 
#' diel.ineq()
#' diel.ineq(e=0.01) #To replace all epsilon values with 0.01.
#' diel.ineq(e.Dn=0.2, xi.Dn=c(0.80,0.20,0.90,0.10)) #To replace teh default values with a new epsilon value and a new xi value.
#' @export
############################
#To do
# Need to constrain diel.hyp.func so that that we maintain probs b/w 0 and 1
# when using epsilon ,   #ifelse( level.general.diurnal+e > 1, 1,level.general.diurnal+e)
# need to work out 0 and 1 probability constraints

############################


#################################
#################################
# Input variables

# e  -  lower and upper difference to calculate range of probability values.

# e varies by hypothesis, so:      e.D, e.Dn, e.Dc, e.Dcr, 
#                                  e.N, e.Nd, e.Nc, e.Ncr, 
#                                  e.CR, e.CRd, e.CRn, e.CRc,
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

# xi.Dc - (Diurnal-cathemeral Hyp.; length 4) 
#        [1] lower prob. of Primary prob., 
#        [2] lower prob. of Secondary prob., which should be above 0 to be cathemeral (e.g., 0.01).
#        [3] most likely prob. of Primary prob., 
#        [4] most likely prob. of Secondary prob (nightime and crepuscular probs).

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

# xi.Nc - (Nocturnal-cathemeral Hyp.; length 4) 
#        [1] lower prob. of Primary prob., 
#        [2] lower prob. of Secondary prob., which should be above 0 to be cathemeral (e.g., 0.01).
#        [3] most likely prob. of Primary prob., 
#        [4] most likely prob. of Secondary prob (daytime and crepuscular probs.).

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

# xi.CRc - (Crepuscular-cathemeral; length 4)
#        [1] lower prob. of Primary prob., 
#        [2] lower prob. of Secondary prob., which should be above 0 to be cathemeral (e.g., 0.01).
#        [3] most likely prob. of Primary prob., 
#        [4] most likely prob. of Secondary prob (daytime and nocturnal probs.).

# xi.EC - (Even Cathemeral; length 1)
#        [1] most likely probability, which is applied to all
#           diel probs.

#start function
diel.ineq2=function(e=NULL,
                   e.D=NULL, e.Dn=NULL, e.Dc=NULL, e.Dcr=NULL, 
                   e.N=NULL, e.Nd=NULL, e.Nc=NULL, e.Ncr=NULL, 
                   e.CR=NULL, e.CRd=NULL, e.CRn=NULL, e.CRc=NULL,
                   e.EC=NULL, e.AC=NULL,
                   xi.D=NULL, xi.Dn=NULL, xi.Dc=NULL,xi.Dcr=NULL,
                   xi.N=NULL,xi.Nd=NULL,xi.Nc=NULL,xi.Ncr=NULL,
                   xi.CR=NULL,xi.CRd=NULL,xi.CRn=NULL,xi.CRc=NULL,
                   xi.EC=NULL, p.avail=NULL){
  
  #Default epsilon values for variation hypotheses
  if(is.null(e.D)){e.D=0.05}
  if(is.null(e.Dn)){e.Dn=0.05}
  if(is.null(e.Dc)){e.Dc=0.05}
  if(is.null(e.Dcr)){e.Dcr=0.05}
  if(is.null(e.N)){e.N=0.05}
  if(is.null(e.Nd)){e.Nd=0.05}
  if(is.null(e.Nc)){e.Nc=0.05}
  if(is.null(e.Ncr)){e.Ncr=0.05}
  if(is.null(e.CR)){e.CR=0.05}
  if(is.null(e.CRd)){e.CRd=0.05}
  if(is.null(e.CRn)){e.CRn=0.05}
  if(is.null(e.CRc)){e.CRc=0.05}
  if(is.null(e.EC)){e.EC=0.05}
  if(is.null(e.AC)){e.AC=0.1}
  
  # If e is entered then this forces all e's to be the same.  
  if(!is.null(e)){e.D=e.Dn=e.Dc=e.Dcr=e.N=e.Nd=e.Nc=e.Ncr=e.CR=e.CRd=e.CRn=e.CRc=e.EC=e.AC=e}
  
  # Defaults for threshold and variation models     
  if(is.null(xi.D)){xi.D=c(0.90,0.95)} #lower threshold value, and most likely value
  if(is.null(xi.Dn)){xi.Dn=c(0.8,0.2,0.9,0.1)}
  if(is.null(xi.Dc)){xi.Dc=c(0.8,0.01,0.9,0.1)}
  if(is.null(xi.Dcr)){xi.Dcr=c(0.8,0.2,0.9,0.1)}
  
  if(is.null(xi.N)){xi.N=c(0.8,0.9)}
  if(is.null(xi.Nd)){xi.Nd=c(0.8,0.2,0.9,0.1)}
  if(is.null(xi.Nc)){xi.Nc=c(0.8,0.01,0.9,0.1)}
  if(is.null(xi.Ncr)){xi.Ncr=c(0.8,0.2,0.9,0.1)}
  
  if(is.null(xi.CR)){xi.CR=c(0.8,0.9)}
  if(is.null(xi.CRd)){xi.CRd=c(0.8,0.2,0.9,0.1)}
  if(is.null(xi.CRn)){xi.CRn=c(0.8,0.2,0.9,0.1)}
  if(is.null(xi.CRc)){xi.CRc=c(0.8,0.01,0.9,0.1)}
  
  if(is.null(xi.EC)){xi.EC  = c(0.33)}
  
  if(is.null(p.avail)){p.avail  = c(0.166666,0.4166667)}
  
  
  #################################
  #################################
  #Diurnal Hypotheses
  # Threshold
  A.D.th <- matrix(c(1,-1,-1,-2,0,-1),ncol = 2, byrow = TRUE)
  b.D.th <- c(0,-1,-xi.D[1])
  D.th=list("Diurnal Threshold",A.D.th,b.D.th)
  # Maximizing
  A.D.max <- matrix(c(1,-1,-1,-2),ncol = 2, byrow = TRUE)
  b.D.max <- c(0,-1)
  D.max=list("Diurnal Max",A.D.max,b.D.max)     
  # Variability
  A.D.var <- matrix(c(1,-1,-1,-2,0,-1,0,1),ncol = 2, byrow = TRUE)
  
  b.D.var <- c(0,-1,-xi.D[2]+e.D,xi.D[2]+e.D)
  D.var=list("Diurnal Var",A.D.var,b.D.var)     
  #################################
  #Diurnal-nocturnal Hypotheses
  # Threshold
  A.Dn.th <- matrix(c(2,1,-1,-2,0,-1,-1,-1,1,0),ncol = 2, byrow = TRUE)
  b.Dn.th <- c(1,-1,-xi.Dn[1],xi.Dn[2]-1,0.001)
  Dn.th=list("Diurnal-nocturnal Threshold",A.Dn.th,b.Dn.th)     
  # Maximizing
  A.Dn.max <- matrix(c(2,1,-1,-2,1,0),ncol = 2, byrow = TRUE)
  b.Dn.max <- c(1,-1,0.001)
  Dn.max=list("Diurnal-nocturnal Max",A.Dn.max,b.Dn.max)     
  
  # Variability
  A.Dn.var <- matrix(c(2,1,-1,-2,0,-1,0,1,1,1,-1,-1,1,0),ncol = 2, byrow = TRUE)
  b.Dn.var <- c(1,-1, -xi.Dn[3]+e.Dn,xi.Dn[3]+e.Dn, -xi.Dn[4]+e.Dn+1,
                xi.Dn[4]+e.Dn-1,0.001)
  Dn.var=list("Diurnal-nocturnal Var",A.Dn.var,b.Dn.var)     
  
  #################################
  #Diurnal-cathemeral Hypotheses
  # Threshold
  A.Dc.th <- matrix(c(1,-1,-1,-2,-1,0,1,1,0,-1),ncol = 2, byrow = TRUE)
  b.Dc.th <- c(0,-1,xi.Dc[2],1+xi.Dc[2],-xi.Dc[1])
  Dc.th=list("Diurnal-cathemeral Threshold",A.Dc.th,b.Dc.th)     
  
  # Maximizing
  A.Dc.max <- matrix(c(1,-1,-1,-2, -1, 0,1,1),ncol = 2, byrow = TRUE)
  b.Dc.max <- c(0,-1,-xi.Dc[2],xi.Dc[2]+1)
  Dc.max=list("Diurnal-cathemeral Max",A.Dc.max,b.Dc.max)     
  
  # Variability
  A.Dc.var <- matrix(c(1,-1,-1,-2,-1,0,1,1,0,1,0,-1,1,0,-1,0,-1,-1,1,1),ncol = 2, byrow = TRUE)
  b.Dc.var <- c(0,-1, -xi.Dc[2],xi.Dc[2]+1, xi.Dc[3]+e.Dc,
                -xi.Dc[3]+e.Dc,xi.Dc[4]+e.Dc,-xi.Dc[4]+e.Dc,xi.Dc[4]+e.Dc-1,
                -xi.Dc[4]+e.Dc+1)
  Dc.var=list("Diurnal-cathemeral Var",A.Dc.var,b.Dc.var)     
  
  #################################
  #Diurnal-crepuscular Hypotheses
  # Threshold
  A.Dcr.th <- matrix(c(-2,-1,1,-1,0,-1,1,0,-1,-1),ncol = 2, byrow = TRUE)
  b.Dcr.th <- c(-1,0,-xi.Dcr[1],xi.Dcr[2],0.01-1)
  Dcr.th=list("Diurnal-crepuscular Threshold",A.Dcr.th,b.Dcr.th)     
  
  # Maximizing
  A.Dcr.max <- matrix(c(-2,-1, 1,-1,-1,-1),ncol = 2, byrow = TRUE)
  b.Dcr.max <- c(-1,0,0.01-1)
  Dcr.max=list("Diurnal-crepuscular Max",A.Dcr.max,b.Dcr.max)     
  
  # Variability
  A.Dcr.var <- matrix(c(-2,-1,1,-1,0,1,0,-1,-1,-1,1,1,-1,-1),ncol = 2, byrow = TRUE)
  b.Dcr.var <- c(-1, 0,xi.Dcr[3]+e.Dcr,-xi.Dcr[3]+e.Dcr,xi.Dcr[4]+e.Dcr-1,-xi.Dcr[4]+e.Dcr+1,0.01-1)
  Dcr.var=list("Diurnal-crepuscular Var",A.Dcr.var,b.Dcr.var)     
  
  #################################
  #################################
  #Nocturnal Hypotheses
  # Threshold
  A.N.th <- matrix(c(1,2,2,1,1,1),ncol = 2, byrow = TRUE)
  b.N.th <- c(1,1,-xi.N[1]+1)
  N.th=list("Nocturnal Threshold",A.N.th,b.N.th)     
  
  # Maximizing
  A.N.max <- matrix(c(1,2,2,1),ncol = 2, byrow = TRUE)
  b.N.max <- c(1,1)
  N.max=list("Nocturnal Max",A.N.max,b.N.max)     
  
  # Variability
  A.N.var <- matrix(c(1,2,2,1,-1,-1,1,1),ncol = 2, byrow = TRUE)
  b.N.var <- c(1, 1,xi.N[2]+e.N-1,-xi.N[2]+e.N+1)
  N.var=list("Nocturnal Var",A.N.var,b.N.var)     
  
  #################################
  #Nocturnal-diurnal Hypotheses
  # Threshold
  A.Nd.th <- matrix(c(1,2,1,-1,1,1,0,1,1,0),ncol = 2, byrow = TRUE)
  b.Nd.th <- c(1,0,-xi.Nd[1]+1,xi.Nd[2],0.01)
  Nd.th=list("Nocturnal-diurnal Threshold",A.Nd.th,b.Nd.th)     
  
  # Maximizing
  A.Nd.max <- matrix(c(1,2,1,-1,1,0),ncol = 2, byrow = TRUE)
  b.Nd.max <- c(1,0,0.01)
  Nd.max=list("Nocturnal-diurnal Max",A.Nd.max,b.Nd.max)     
  
  # Variability
  A.Nd.var <- matrix(c(1,2,1,-1,-1,-1,1,1,0,1,0,-1,1,0),ncol = 2, byrow = TRUE)
  b.Nd.var <- c(1,0,xi.Nd[3]+e.Nd-1,-xi.Nd[3]+e.Nd+1,xi.Nd[3]+e.Nd,-xi.Nd[4]+e.Nd,0.01)
  Nd.var=list("Nocturnal-diurnal Var",A.Nd.var,b.Nd.var)     
  
  #################################
  #Nocturnal-cathemeral Hypotheses
  # Threshold
  A.Nc.th <- matrix(c(1,2,2,1,0,-1,-1,0,1,1),ncol = 2, byrow = TRUE)
  b.Nc.th <- c(1,1,-xi.Nc[2],-xi.Nc[2],-xi.Nc[1]+1)
  Nc.th=list("Nocturnal-cathemeral Threshold",A.Nc.th,b.Nc.th)     
  
  # Maximizing
  A.Nc.max <- matrix(c(1,2,2,1,0,-1,-1,0),ncol = 2, byrow = TRUE)
  b.Nc.max <- c(1,1,-xi.Nc[2],-xi.Nc[2])
  Nc.max=list("Nocturnal-cathemeral Max",A.Nc.max,b.Nc.max)     
  
  # Variability
  A.Nc.var <- matrix(c(1,2,2,1,0,-1,-1,0,1,1,-1,-1,-1,0,1,0),ncol = 2, byrow = TRUE)
  b.Nc.var <- c(1,1,-xi.Nc[2],-xi.Nc[2],-xi.Nc[3]+e.Nc+1,xi.Nc[3]+e.Nc-1,
                -xi.Nc[4]+e.Nc,xi.Nc[4]+e.Nc)
  Nc.var=list("Nocturnal-cathemeral Var",A.Nc.var,b.Nc.var)     
  
  #################################
  #Nocturnal-crepuscular Hypotheses
  # Threshold
  A.Ncr.th <- matrix(c(-1,1,2,1,1,1,1,0,0,1),ncol = 2, byrow = TRUE)
  b.Ncr.th <- c(0,1,xi.Ncr[1]+1,xi.Ncr[2],0.001)
  Ncr.th=list("Nocturnal-crepuscular Threshold",A.Ncr.th,b.Ncr.th)     
  
  # Maximizing
  A.Ncr.max <- matrix(c(-1,1,2,1,0,1),ncol = 2, byrow = TRUE)
  b.Ncr.max <- c(0,1,0.001)
  Ncr.max=list("Nocturnal-crepuscular Max",A.Ncr.max,b.Ncr.max)     
  
  # Variability
  A.Ncr.var <- matrix(c(-1,1,2,1,-1,-1,1,1,1,0,-1,0,0,1),ncol = 2, byrow = TRUE)
  b.Ncr.var <- c(0,1,xi.Ncr[3]+e.Ncr-1,-xi.Ncr[3]+e.Ncr+1,
                 xi.Ncr[4]+e.Ncr,-xi.Ncr[4]+e.Ncr,0.001)
  Ncr.var=list("Nocturnal-crepuscular Var",A.Ncr.var,b.Ncr.var)     
  
  
  #################################
  #################################
  #Crepuscular Hypotheses
  # Threshold
  A.CR.th <- matrix(c(-1,1,-2,-1,-1,0),ncol = 2, byrow = TRUE)
  b.CR.th <- c(0,-1,-xi.CR[1])
  CR.th=list("Crepuscular Threshold",A.CR.th,b.CR.th)     
  
  # Maximizing
  A.CR.max <- matrix(c(-1,1,-2,-1),ncol = 2, byrow = TRUE)
  b.CR.max <- c(0,-1)
  CR.max=list("Crepuscular Max",A.CR.max,b.CR.max)     
  
  # Variability
  A.CR.var <- matrix(c(-1,1,-2,-1,-1,0,1,0),ncol = 2, byrow = TRUE)
  b.CR.var <- c(0,-1,-xi.CR[2]+e.CR,xi.CR[2]+e.CR)
  CR.var=list("Crepuscular Var",A.CR.var,b.CR.var)     
  
  #################################
  #Crepuscular-diurnal Hypotheses
  # Threshold
  A.CRd.th <- matrix(c(-1,-2,-1,1,-1,0,0,1,-1,-1),ncol = 2, byrow = TRUE)
  b.CRd.th <- c(-1,0,-xi.CRd[1],xi.CRd[2],-0.99)
  CRd.th=list("Crepuscular-diurnal Threshold",A.CRd.th,b.CRd.th)     
  
  # Maximizing
  A.CRd.max <- matrix(c(-1,-2,-1,1,-1,-1),ncol = 2, byrow = TRUE)
  b.CRd.max <- c(-1,0,-0.99)
  CRd.max=list("Crepuscular-diurnal Max",A.CRd.max,b.CRd.max)     
  
  # Variability
  A.CRd.var <- matrix(c(-1,-2,-1,1,-1,0,1,0,0,-1,0,1,-1,-1),ncol = 2, byrow = TRUE)
  b.CRd.var <- c(-1, 0,-xi.CRd[3]+e.CRd,xi.CRd[3]+e.CRd,-xi.CRd[4]+e.CRd,xi.CRd[4]+e.CRd,-0.99)
  CRd.var=list("Crepuscular-diurnal Var",A.CRd.var,b.CRd.var)     
  
  #################################
  #Crepuscular-nocturnal Hypotheses
  # Threshold
  A.CRn.th <- matrix(c(1,2,-2,-1,-1,0,-1,-1,0,1),ncol = 2, byrow = TRUE)
  b.CRn.th <- c(1,-1,xi.CRn[1],xi.CRn[2]-1,0.001)
  CRn.th=list("Crepuscular-nocturnal Threshold",A.CRn.th,b.CRn.th)     
  
  # Maximizing
  A.CRn.max <- matrix(c(1,2,-2,-1,0,1),ncol = 2, byrow = TRUE)
  b.CRn.max <- c(1,-1,0.001)
  CRn.max=list("Crepuscular-nocturnal Max",A.CRn.max,b.CRn.max)     
  
  # Variability
  A.CRn.var <- matrix(c(1,2,-2,-1,-1,0,1,0,1,1,-1,-1,0,1),ncol = 2, byrow = TRUE)
  b.CRn.var <- c(1,-1,-xi.CRn[3]+e.CRn,xi.CRn[3]+e.CRn,-xi.CRn[4]+e.CRn+1,xi.CRn[4]+e.CRn-1,0.001)
  CRn.var=list("Crepuscular-nocturnal Var",A.CRn.var,b.CRn.var)     
  
  #################################
  #Crepuscular-cathemeral Hypotheses
  # Threshold
  A.CRc.th <- matrix(c(-1,1,-2,-1,0,-1,1,1,-1,0),ncol = 2, byrow = TRUE)
  b.CRc.th <- c(0,-1,-xi.CRc[2],-xi.CRc[2]+1,-xi.CRc[1])
  CRc.th=list("Crepuscular-cathemeral Threshold",A.CRc.th,b.CRc.th)     
  
  # Maximizing
  A.CRc.max <- matrix(c(-1,1,-2,-1,0,-1,1,1),ncol = 2, byrow = TRUE)
  b.CRc.max <- c(0,-1,-xi.CRc[2],-xi.CRc[2]+1)
  CRc.max=list("Crepuscular-cathemeral Max",A.CRc.max,b.CRc.max)     
  
  # Variability
  A.CRc.var <- matrix(c(-1,1,-2,-1,0,-1,1,1,-1,0,1,0,1,1,-1,-1,0,-1,0,1),ncol = 2, byrow = TRUE)
  b.CRc.var <- c(0,-1,-xi.CRc[2],-xi.CRc[2]+1,-xi.CRc[3]+e.CRc,
                 xi.CRc[3]+e.CRc,-xi.CRc[4]+e.CRc+1,xi.CRc[4]+e.CRc-1,
                 -xi.CRc[4]+e.CRc,xi.CRc[4]+e.CRc)
  CRc.var=list("Crepuscular-cathemeral Var",A.CRc.var,b.CRc.var)     
  
  #################################
  #################################
  #Even Cathemeral Hypotheses
  # Threshold
  A.EC.th <- matrix(c(-1,-1,0,1,1,0,1,1,0,-1,-1,0),ncol = 2, byrow = TRUE)
  b.EC.th <- c(0.43-1,0.43,0.43,-0.23+1,-0.23,-0.23)
  EC.th=list("Even Cathemeral Threshold",A.EC.th,b.EC.th)     
  
  # Maximizing
  # NA
  
  # Variation
  A.EC.var <- matrix(c(1,0,-1,0,0,1,0,-1,-1,-1,1,1),ncol = 2, byrow = TRUE)
  b.EC.var <- c(xi.EC[1]+e.EC,-xi.EC[1]+e.EC,xi.EC[1]+e.EC,
                -xi.EC[1]+e.EC,xi.EC[1]+e.EC-1,-xi.EC[1]+e.EC+1)
  EC.var=list("Even Cathemeral Var",A.EC.var,b.EC.var)     
  
  #################################
  #################################
  #Available Cathemeral Hypotheses
  
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
  AC.var=list("Available Cathemeral Var",A.AC.var,b.AC.var)     
#################################
#################################
#General Cathemeral Hypotheses

# Threshold
  A.C.th <- matrix(c(1,1,
                       0, -1,
                       -1, 0),ncol = 2, byrow = TRUE)
  b.C.th <- c(-0.2+1,-0.2,-0.2)
  C.th=list("Cathemeral Threshold",A.C.th,b.C.th)     


  
# Maximizing
# NA
  
# Variation
##################################################

  ##################################################
  #Package Inputs
  inputs=list(e.D=e.D, e.Dn=e.Dn, e.Dc=e.Dc, e.Dcr=e.Dcr, 
              e.N=e.N, e.Nd=e.Nd, e.Nc=e.Nc, e.Ncr=e.Ncr, 
              e.CR=e.CR, e.CRd=e.CRd, e.CRn=e.CRn, e.CRc=e.CRc,
              e.EC=e.EC, e.AC=e.AC,
              xi.D=xi.D, xi.Dn=xi.Dn, xi.Dc=xi.Dc,xi.Dcr=xi.Dcr,
              xi.N=xi.N,xi.Nd=xi.Nd,xi.Nc=xi.Nc,xi.Ncr=xi.Ncr,
              xi.CR=xi.CR,xi.CRd=xi.CRd,xi.CRn=xi.CRn,xi.CRc=xi.CRc,
              xi.EC=xi.EC, p.avail=p.avail)  
  
  #package outputs  
  diel.hyp=list(   
    D.th=D.th, D.max=D.max,D.var=D.var,
    Dn.th=Dn.th,Dn.max=Dn.max,Dn.var=Dn.var,
    Dc.th=Dc.th, Dc.max=Dc.max,Dc.var=Dc.var,
    Dcr.th=Dcr.th,Dcr.max=Dcr.max,Dcr.var=Dcr.var,
    N.th =N.th,N.max=N.max,N.var=N.var,
    Nd.th=Nd.th, Nd.max=Nd.max,Nd.var=Nd.var,
    Nc.th=Nc.th,Nc.max=Nc.max,Nc.var=Nc.var,
    Ncr.th=Ncr.th,Ncr.max=Ncr.max,Ncr.var=Ncr.var,
    CR.th=CR.th,CR.max=CR.max,CR.var=CR.var,
    CRd.th=CRd.th,CRd.max=CRd.max,CRd.var=CRd.var,
    CRn.th=CRn.th,CRn.max=CRn.max,CRn.var=CRn.var,
    CRc.th=CRc.th,CRc.max=CRc.max,CRc.var=CRc.var,
    EC.th=EC.th,EC.var=EC.var,AC.var=AC.var, C.th=C.th,
    inputs=inputs
  )
  
  #output from function
  diel.hyp
  
  
} #end function 