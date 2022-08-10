#' Inequality Setup
#'
#' Multinomial model inequalities for diel hypotheses
#' @param e Default is 0.05. A single value of variation for probabilities. If specified, it will be applied to all hypotheses, regardless of whether individual epsilon hypotheses values are specified. 
#' @param e.D Default is 0.05. A single value of variation for the Diurnal hypothesis.
#' @param e.N Default is 0.05. A single value of variation for the Nocturnal hypothesis.
#' @param e.CR Default is 0.05. A single value of variation for the Crepuscular hypothesis.
#' @param e.EC  Default is 0.05. A single value of variation for the Evan Cathemeral hypothesis.
#' @param e.AV   Default is 0.10. A single value of variation for the Available Cathemeral hypothesis.
#' @param xi.D Default c(0.90,0.95). A vector of the lower threshold value and the most likely value, respectively for the Diurnal hypothesis.
#' @param xi.N  Default c(0.80,0.90). A vector of the lower threshold value and most likely value, respectively for the Nocturnal hypothesis.
#' @param xi.CR Default c(0.80,0.90). A vector of the lower threshold value and most likely value, respectively for the Crepuscular hypothesis.
#' @param xi.C Default c(0.2). Lower value threshold to be cathemeral
#' @param xi.EC Default c(0.33). A single value of the available amount of time in all three diel periods.
#' @param xi.min.dom Default c(0.8). A single value of the min prob for a a dominant category 
#' @param separation Default is 0. However, if you want to separate the hypotheses hyp.max, General2, and General 3, this will create separation and empty space
#' @param p.avail Default c(0.166666,0.4166667). A vector of the available time in the periods of crepuscular and diurnal. Nighttime availability is found by subtraction.
#' 
#' @return diel.hyp A list of diel hypotheses as multinomial inequalities.
#' \item{inputs}{Includes all inputted values; epsilon, xi, and p.avail.} 
#' \item{D.th, N.th, CR.th, EC.th, C.th, D.max, N.max, CR.max, D.var, N.var, CR.var, C.var, AC.var, AV.var,
#'    Uncon, C.max, EC, AV.EQ}{Each is a list of three elements: Hypotheis Descriptive Name, A matrix, and b vector.} 
#' @examples 
#' diel.ineq()
#' diel.ineq(e=0.01) #To replace all epsilon values with 0.01.
#' #To replace the default values with a new epsilon value and a new xi value.
#' diel.ineq(e.Dn=0.2, xi.Dn=c(0.80,0.20,0.90,0.10)) 
#' @export
############################


#################################
#################################
# Input variables

# e  -  lower and upper difference to calculate range of probability values.

# e varies by hypothesis, so:      e.D, 
#                                  e.N, 
#                                  e.CR,
#                                  e.EC, e.AV

# p.avail - (Available Cathemeral; length 2)
#        [1] Available proportion of time in twilight time period
#        [2] Available proportion of time in daytime period

# xi.D - (Diurnal Hyp; length 2) 
#        [1] lowest probability of p_d , 
#        [2] most likely probability of p_d.


# xi.N - (Nocturnal Hyp; length 2) 
#        [1] lowest probability of p_n , 
#        [2] most likely probability of p_n.


# xi.CR - (Crepuscular Hyp; length 2) 
#        [1] lowest probability of p_c , 
#        [2] most likely probability of p_c.


# xi.EC - (Even Cathemeral; length 1)
#        [1] most likely probability, which is applied to all
#           diel probs.

#start function
diel.ineq=function(e=NULL,
                   e.D=NULL, 
                   e.N=NULL, 
                   e.CR=NULL,
                   e.EC=NULL, 
                   e.AV=NULL,
                   xi.D=NULL, 
                   xi.N=NULL,
                   xi.CR=NULL,
                   xi.EC=NULL, 
                   p.avail=NULL,
                   xi.min.dom=NULL, 
                   separation=NULL){
  
  #Default epsilon values for VARIATION hypotheses
  if(is.null(e.D)){e.D=0.05}
  if(is.null(e.N)){e.N=0.05}
  if(is.null(e.CR)){e.CR=0.05}
  if(is.null(e.EC)){e.EC=0.05}
  if(is.null(e.AV)){e.AV=0.1}
  
  # If e is entered then this forces all e's to be the same.  
  if(!is.null(e)){e.D=e.N=e.CR=e.EC=e.AV=e}
  
  # Defaults for threshold and variation models     
  if(is.null(xi.D)){xi.D=c(0.80,0.9)} #lower threshold value, most likely value
  if(is.null(xi.N)){xi.N=c(0.8,0.9)}  #lower threshold value, most likely value
  if(is.null(xi.CR)){xi.CR=c(0.8,0.9)}#lower threshold value, most likely value
  if(is.null(xi.EC)){xi.EC  = c(0.33)}#Even cathemerality probability
  if(is.null(p.avail)){p.avail  = c(0.166666,0.4166667)} #crepuscular availability and diurnal availability
  if(is.null(xi.min.dom)){xi.min.dom  = c(0.8,0.1)}else{xi.min.dom  = c(xi.min.dom,(1-xi.min.dom)/2)}
  if(is.null(separation)){separation  = c(0)}
  small.num=0.0001  

  #################################
  #################################
  #Traditional - 4 hypotheses
  #Diurnal
  A.D <- matrix(c(0,-1),ncol = 2, byrow = TRUE)
  b.D <- c(-xi.min.dom[1])
  D=list(Name="Diurnal",A=A.D,b=b.D,func="bf_multinom")     

  #Nocturnal
  A.N <- matrix(c(1,1),ncol = 2, byrow = TRUE)
  b.N <- c(-xi.min.dom[1]+1)
  N=list(Name="Nocturnal",A=A.N,b=b.N,func="bf_multinom")     
  
  #Crepuscular
  A.CR <- matrix(c(-1,0),ncol = 2, byrow = TRUE)
  b.CR <- c(-xi.min.dom[1])
  CR=list(Name="Crepuscular",A=A.CR,b=b.CR,func="bf_multinom")     

  #Cathemeral
  A.C <- matrix(c(0,1,1,0,-1,-1),ncol = 2, byrow = TRUE)
  b.C <- c(xi.min.dom[1]-small.num-separation,xi.min.dom[1]-small.num-separation,xi.min.dom[1]-small.num-1-separation)
  C=list(Name="Cathemeral",A=A.C,b=b.C,func="bf_multinom")   

#################################
#################################
  #GENERAL - Hypotheses 7
  
  #Dirunal
  #Same as D above

  #Nocturnal 
  #Same as N above  
  
  #Crepuscular
  #Same as CR above
  
  #Cathemeral
  A.C <- matrix(c(0,-1,1,1,-1,0,0,1,-1,-1),ncol = 2, byrow = TRUE)
  b.C <- c(-xi.min.dom[2]-separation,-xi.min.dom[2]+1-separation,-xi.min.dom[2]-separation,xi.min.dom[1]-small.num-separation,xi.min.dom[1]-small.num-1-separation)
  C2=list(Name="Cathemeral",A=A.C,b=b.C,func="bf_multinom")   
  
 
  #CRD
  A.CR.D <- matrix(c(-1,-1,-1,0,1,0,0,-1,0,1),ncol = 2, byrow = TRUE)
  b.CR.D <- c(xi.min.dom[2]-small.num-1,-xi.min.dom[2]-separation,xi.min.dom[1]-small.num-separation,-xi.min.dom[2],xi.min.dom[1]-small.num-separation)
  CR.D=list(Name="CR.D",A=A.CR.D,b=b.CR.D,func="bf_multinom")   

  #DN
  A.D.N <- matrix(c(1,0,0,-1,0,1,1,1,-1,-1),ncol = 2, byrow = TRUE)
  b.D.N <- c(xi.min.dom[2]-small.num,-xi.min.dom[2]-separation,xi.min.dom[1]-small.num-separation,-xi.min.dom[2]+1,xi.min.dom[1]-small.num-1-separation)
  D.N=list(Name="D.N",A=A.D.N,b=b.D.N,func="bf_multinom")   
  
  #CRN
  A.CR.N <- matrix(c(0,1,1,1,-1,-1,-1,0,1,0),ncol = 2, byrow = TRUE)
  b.CR.N <- c(xi.min.dom[2]-small.num,-xi.min.dom[2]+1-separation,xi.min.dom[1]-small.num-1-separation,-xi.min.dom[2],xi.min.dom[1]-small.num-separation)
  CR.N=list(Name="CR.N",A=A.CR.N,b=b.CR.N,func="bf_multinom")   
  
  
#################################
#################################
  #THRESHOLD - 5 hypotheses
  
  #Diurnal 
  A.D.th <- matrix(c(1,-1,-1,-2,0,-1),ncol = 2, byrow = TRUE)
  b.D.th <- c(0,-1,-xi.D[1])
  D.th=list(Name="Diurnal Threshold",A=A.D.th,b=b.D.th,func="bf_multinom")
  
  #Nocturnal 
  A.N.th <- matrix(c(1,2,2,1,1,1),ncol = 2, byrow = TRUE)
  b.N.th <- c(1,1,-xi.N[1]+1)
  N.th=list(Name="Nocturnal Threshold",A=A.N.th,b=b.N.th,func="bf_multinom")     

  #Crepuscular 
  A.CR.th <- matrix(c(-1,1,-2,-1,-1,0),ncol = 2, byrow = TRUE)
  b.CR.th <- c(0,-1,-xi.CR[1])
  CR.th=list(Name="Crepuscular Threshold",A=A.CR.th,b=b.CR.th,func="bf_multinom")     
  
  #Even Cathemeral 
  A.EC.th <- matrix(c(-1,-1,0,1,1,0,1,1,0,-1,-1,0),ncol = 2, byrow = TRUE)
  b.EC.th <- c(0.43-1,0.43,0.43,-0.23+1,-0.23,-0.23)
  EC.th=list(Name="Even Cathemeral Threshold",A=A.EC.th,b=b.EC.th,func="bf_multinom")     

  #General Cathemeral Hypotheses
  #BDG- this needs to be dynamic with an input- right?
  A.C.th <- matrix(c(1,1,
                    0, -1,
                    -1, 0),ncol = 2, byrow = TRUE)
  #b.C.th <- c(-0.2+1,-0.2,-0.2)
  b.C.th <- c(-xi.C+1,-xi.C,-xi.C)
  C.th=list(Name="Cathemeral Threshold",A=A.C.th,b=b.C.th,func="bf_multinom")     

  
#################################
#################################
  #MAXIMIZING - 3 hypotheses
  
  #Diurnal 
  A.D.max <- matrix(c(1,-1,-1,-2),ncol = 2, byrow = TRUE)
  #b.D.max <- c(0,-1)
  b.D.max <- c(-small.num-separation,-small.num-1-separation)
  D.max=list(Name="Diurnal Max",A=A.D.max, b=b.D.max,func="bf_multinom")     

  #Nocturnal 
  A.N.max <- matrix(c(1,2,2,1),ncol = 2, byrow = TRUE)
  b.N.max <- c(-small.num+1-separation,-small.num+1-separation)
  N.max=list(Name="Nocturnal Max",A=A.N.max,b=b.N.max,func="bf_multinom")     

  #Crepuscular 
  A.CR.max <- matrix(c(-1,1,-2,-1),ncol = 2, byrow = TRUE)
  #b.CR.max <- c(0,-1)
  b.CR.max <- c(-small.num-separation,-small.num-1-separation)
  CR.max=list(Name="Crepuscular Max",A=A.CR.max,b=b.CR.max,func="bf_multinom")     
  
  #No Cathemeral Maximizing Hypothesis
  
#################################
#################################
  #VARIATION - 5 hypotheses
  #Diurnal
  A.D.var <- matrix(c(1,-1,-1,-2,0,-1,0,1),ncol = 2, byrow = TRUE)
  b.D.var <- c(0,-1,-xi.D[2]+e.D,xi.D[2]+e.D)
  D.var=list(Name="Diurnal Var",A=A.D.var,b=b.D.var,func="bf_multinom")     
  
  #Nocturnal 
  A.N.var <- matrix(c(1,2,2,1,-1,-1,1,1),ncol = 2, byrow = TRUE)
  b.N.var <- c(1, 1,xi.N[2]+e.N-1,-xi.N[2]+e.N+1)
  N.var=list(Name="Nocturnal Var",A=A.N.var,b=b.N.var,func="bf_multinom")     

  #Crepuscular 
  A.CR.var <- matrix(c(-1,1,-2,-1,-1,0,1,0),ncol = 2, byrow = TRUE)
  b.CR.var <- c(0,-1,-xi.CR[2]+e.CR,xi.CR[2]+e.CR)
  CR.var=list(Name="Crepuscular Var",A=A.CR.var,b=b.CR.var,func="bf_multinom")     

  #Cathemeral 
  A.C.var <- matrix(c(1,0,-1,0,0,1,0,-1,-1,-1,1,1),ncol = 2, byrow = TRUE)
  b.C.var <- c(xi.EC[1]+e.EC,-xi.EC[1]+e.EC,xi.EC[1]+e.EC,
                -xi.EC[1]+e.EC,xi.EC[1]+e.EC-1,-xi.EC[1]+e.EC+1)
  C.var=list(Name="Cathemeral Var",A=A.C.var,b=b.C.var,func="bf_multinom")     

  # Available Activity Variation
  A.AV.var <- matrix(c(-1/p.avail[1],0,
                       1/p.avail[1],0,
                       0,-1/p.avail[2],
                       0,1/p.avail[2],
                       1/(1-sum(p.avail)),1/(1-sum(p.avail)),
                       -1/(1-sum(p.avail)),-1/(1-sum(p.avail))),ncol = 2, byrow = TRUE)
  b.AV.var <- c(-1+e.AV,1+e.AV,-1+e.AV,1+e.AV,
                -1+e.AV+(1/(1-sum(p.avail))),1+e.AV-(1/(1-sum(p.avail))))
  AV.var=list(Name="Available Var",A=A.AV.var,b=b.AV.var,func="bf_multinom")     

##################################
#################################
# SPECIAL Hypotheses
  
  #Unconstrained model
  #Constraints specify p_c and p_d have to be between 0 and 1
    A.uncon <- matrix(c(0,1,0,-1,1,0,-1,0),ncol = 2, byrow = TRUE)
    b.uncon <- c(1,0,1,0)
    Uncon=list(Name="Unconstrained",A=A.uncon,b=b.uncon,func="bf_multinom")

  #non-linear model for Hyp.max set
  #This complements the missing probs from the set of D.max, N.max, and CR.max when probabilities are equal  
    C.max =list(Name="C.max",func="bf_nonlinear",data=non.linear.data$hyp.C.max,
                    inside=  function(x){min(abs(x[1] - matrix(non.linear.data$hyp.C.max[,1]))+abs(x[2] - matrix(non.linear.data$hyp.C.max[,2])))<=0.005})

  #Even Cathemeral Hypotheses - Equality Hypothesis
  #A just forces p1 and p2 to be less than 1. Allows for consistency in code execution indiel.bf
    A.EC <- matrix(c(1,0,0,1),ncol = 2, byrow = TRUE)
    b.EC <- c(1,1)
    C.EC <- matrix(c(1,0,0,1),ncol = 2, byrow = TRUE)
    d.EC <- c(0.3333,0.3333)
    EC=list(Name="Even Cathemeral Equality",A=A.EC,b=b.EC,C=C.EC,d=d.EC,func="bf_equality") 

  #Available Hypotheses - Equality Hypothesis
  #A just forces p1 and p2 to be less than 1. Allows for consistency in code execution in diel.bf
    A.AV <- matrix(c(1,0,0,1),ncol = 2, byrow = TRUE)
    b.AV <- c(1,1)
    C.AV <- matrix(c(1,0,0,1),ncol = 2, byrow = TRUE)
    d.AV <- c(p.avail[1],p.avail[2])
    AV.EQ=list(Name="Available Equality",A=A.AV,b=b.AV,C=C.AV,d=d.AV,func="bf_equality") 

#################################
#################################

  #Package Inputs
  inputs=list(e.D=e.D, e.N=e.N, e.CR=e.CR, 
              e.EC=e.EC, e.AV=e.AV,
              xi.D=xi.D,xi.N=xi.N,xi.CR=xi.CR,xi.C=xi.C,
              xi.min.dom=xi.min.dom,
              xi.EC=xi.EC, p.avail=p.avail,separation=separation)  
  
  #package outputs  
  diel.hyp=list(   
    D=D,N=N,CR=CR,C=C,
    D.N=D.N,CR.N=CR.N,CR.D=CR.D,C2=C2,
    D.th=D.th, N.th =N.th, CR.th=CR.th,EC.th=EC.th,C.th=C.th,
    D.max=D.max,N.max=N.max, CR.max=CR.max,
    D.var=D.var,N.var=N.var,CR.var=CR.var,C.var=C.var,AV.var=AV.var, 
    EC=EC,
    AV.EQ=AV.EQ,
    Uncon=Uncon,
    C.max=C.max,
    inputs=inputs
    )
  
  #output from function
  diel.hyp
  
  
} #End function 
