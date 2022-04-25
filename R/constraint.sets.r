#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export
# Full Investment Constraints
cset.sum <- function(returns, sum=1){
  p = ncol(returns)
  constaint.sum <- rep(1,p)
  A = cbind(constaint.sum) # Constraint matrix
  colnames(A) <- "c.sum"
  b = sum                  # Constraint bound
  meq = 1
  # cset.lo = constraints(A,b,meq)
  return(list(A=A,b=b,meq=meq,cname="cset.sum"))
}


#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export

# Long-Only Constraints
cset.lo <- function(returns){
p = ncol(returns)
A = diag(rep(1,p)) # Constraint matrix
colnames(A) <- paste("c.long.only",1:ncol(A),sep="")
b = c(rep(0,p))                   # Constraint bound
meq = 0
# cset.lo = constraints(A,b,meq)
return(list(A=A,b=b,meq=meq,cname="cset.lo"))
}


#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export

# Box Constraints
cset.box <- function(returns,upper,lower){
p = ncol(returns)

if(length(upper)==1)
  upper <- rep(upper, p)


if(length(lower)==1)
  lower <- rep(lower, p)


constraint.box <-cbind(diag(rep(1,p)),diag(rep(-1,p)))
A = cbind(constraint.box)
colnames(A) <- paste("c.box",
                     c(paste(".low",rep(1:(ncol(constraint.box)/2)),sep=""),
                     paste(".up",rep(1:(ncol(constraint.box)/2)),sep="")),sep="")
b = c(lower,-upper)
meq = 0
# cset.box = constraints(A,b,meq)
return(list(A=A,b=b,meq=meq,cname="cset.box"))
}


#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export

# Group Constraints 
cset.groups <- function(returns=returns8,
                        group=c(1,1,1,2,3,3,4,4),
                        upper.group=rep(0.2,4),
                        lower.group=rep(-0.2,4)){
p = ncol(returns)


if(length(upper.group)==1)
  upper.group <- rep(upper.group, length(unique(group)))


if(length(lower.group)==1)
  lower.group <- rep(lower.group, length(unique(group)))


if(!all(length(group)==p,length(upper.group)==length(unique(group)), length(lower.group)==length(unique(group))))
  stop("any of the length of group/upper/lower bounds is not equal to the number of assets")

if(length(upper.group)==1)
  rep(upper.group, length(group))

if(length(lower.group)==1)
  rep(lower.group, length(group))
  
gnum <- length(unique(group))
glist <- lapply(unique(group),function(x) which(x == group))
names(glist) <- unique(group)
Ag <- t(sapply(glist, function(x) {
  re <- rep(0,p)
  re[x] <- 1
  re}))
Atp = rbind(Ag,-Ag)
constraint.groups = t(Atp)
A <- cbind(constraint.groups)
colnames(A) <-  paste("c.groups",
                      c(paste(".low",rep(1:(ncol(constraint.groups)/2)),sep=""),
                      paste(".up",rep(1:(ncol(constraint.groups)/2)),sep="")),sep="")
bglo = c(lower.group)
bgup = c(upper.group)
b = c(bglo,-bgup)
meq = 0
# cset.groups = constraints(A,b,meq)
return(list(A=A,b=b,meq=meq,cname="cset.groups"))
}


#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export

# Turnover Constraints

cset.turnover <- function(returns,toc,w.initial){
	p = ncol(returns)
	constraint.turnover <- c(rep(0,p),rep(-1,p),rep(1,p))
#	constraint.weights.initial <- rbind(diag(p),matrix(0,ncol=p,nrow=p*2))
#	constraint.weights.finalsum <- c(rep(1,p),rep(0,2*p))
	constraint.weights.initial <- rbind(diag(1,p),diag(-1,p),diag(-1,p))
	constraint.weights.positive <-
			rbind(matrix(0,ncol=2*p,nrow=p),diag(2*p))
	temp.index <- (p*3-p+1):(p*3)
	#need to flip sign for w_sell
	constraint.weights.positive[temp.index,]<-
			constraint.weights.positive[temp.index,]*-1
	
	A <- cbind(constraint.weights.initial,
			constraint.turnover, constraint.weights.positive)
	colnames(A) <- c(paste("c.winit",1:p,sep=""),
			"c.turnover",
			paste("c.wpos",1:ncol(constraint.weights.positive),sep=""))
	b <- c(w.initial,-toc,rep(0,2*p))
	meq <- p+1
	return(list(A=A,b=b,meq=meq,cname="cset.turnover"))
}


#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export

# Turnover Constraints made by hobbs
cset.turnover.hobbs <- function(returns,toc,w.initial){
	p = ncol(returns)
	constraint.turnover <- c(rep(0,p),rep(-1,p),rep(1,p))
	constraint.weights.initial <- rbind(diag(p),matrix(0,ncol=p,nrow=p*2))
	constraint.weights.positive <-
			rbind(matrix(0,ncol=2*p,nrow=p),diag(2*p))
	temp.index <- (p*3-p+1):(p*3)
	#need to flip sign for w_sell
	constraint.weights.positive[temp.index,]<-
			constraint.weights.positive[temp.index,]*-1
	
	A <- cbind(constraint.weights.initial,
			constraint.turnover, constraint.weights.positive)
	colnames(A) <- c( paste("c.winit",1:ncol(constraint.weights.initial),sep=""),
			"c.turnover",
			paste("c.wpos",1:ncol(constraint.weights.positive),sep=""))
	b <- c(w.initial,-toc,rep(0,2*p))
	meq <- p
	
	return(list(A=A,b=b,meq=meq,cname="cset.turnover"))
}





#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export

#Proportional Cost Constraints
cset.propcost<- function(returns, ptc,w.initial){
  p = ncol(returns)
  dvec <- rep(0,p*3) #no linear part in this problem
  #left hand side of constraints
  if(length(ptc)==1){
    ptc = rep(ptc,p)
  }
  if(length(ptc)!=p){
    stop("ptc must either be a single value, or the same length as the number of assets")
  }
  
  constraint.sum.prop <- c(rep(1,p),1+ptc,(1-ptc))
  constraint.weights.initial <- rbind(diag(p),matrix(0,ncol=p,nrow=p*2))
  constraint.weights.positive <-
    rbind(matrix(0,ncol=2*p,nrow=p),diag(2*p))
  temp.index <- (p*3-p+1):(p*3)
  #need to flip sign for w_sell
  constraint.weights.positive[temp.index,]<-
  constraint.weights.positive[temp.index,]*-1
  #put left hand side of constraints into constraint matrix
  A <- cbind(constraint.sum.prop, constraint.weights.initial,
             constraint.weights.positive)
  colnames(A) <- c("c.sum.prop",
                   paste("c.winit",1:ncol(constraint.weights.initial),sep=""),
                   paste("c.wpos",1:ncol(constraint.weights.positive),sep=""))
  #right hand side of constraints in this vector
  b <- c(1, w.initial,rep(0,2*p))
  n.eq = 1+ p
  return(list(A=A,b=b,meq=n.eq,cname="cset.propcost"))
}

#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export
# Target MU Constraints
cset.mu.target <- function(returns, mu.target){
  p = ncol(returns)
  mu = apply(returns,2, mean)
  A = cbind(mu)
  colnames(A) <- "c.mu.target"
  b = mu.target
  meq = 1
  return(list(A=A,b=b,meq=meq,cname="cset.mu.target"))
}


#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export

cset.fixed <- function(returns, fixed.cost, wmax, wmin){
    
  # TBA
}

#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export
# Modify constaints when turnover kicks in
turnover.hobbs.modify <- function(cset=cset.i){
  switch(cset$cname,
          "cset.sum" = {
            cset.new <- cset
            cset.new$A <- rbind(cset$A,cset$A,cset$A)
            colnames(cset.new$A) <- colnames(cset$A)
            cset.new$b <- cset$b
            cset.new$meq <- cset$meq
            cset.new           
          },
         
         "cset.lo" = { #may not be needed
           cset.new <- cset
           cset.new$A <- rbind(cset$A,cset$A,cset$A)
           colnames(cset.new$A) <- colnames(cset$A)
           cset.new$b <- cset$b
           cset.new$meq <- cset$meq
           cset.new           
         },
         
         "cset.box" = {
           cset.new <- cset
           cset.new$A <- rbind(cset$A,cset$A,cset$A)
           colnames(cset.new$A) <- colnames(cset$A)
           cset.new$b <- cset$b
           cset.new$meq <- cset$meq
           cset.new           
         },
         
         "cset.groups" = {
           cset.new <- cset
           cset.new$A <- rbind(cset$A,cset$A,cset$A)
           colnames(cset.new$A) <- colnames(cset$A)
           cset.new$b <- cset$b
           cset.new$meq <- cset$meq
           cset.new           
         },
         
        "cset.mu.target" = {
          cset.new <- cset
          cset.new$A <- rbind(cset$A,cset$A,cset$A)
          colnames(cset.new$A) <- colnames(cset$A)
          cset.new$b <- cset$b
          cset.new$meq <- cset$meq
          cset.new           
        }
      )
  
}
  


#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export

# Modify constaints when turnover kicks in
turnover.modify <- function(cset=cset.i){
	
	makenullmat <- function(A){matrix(0,nrow=nrow(A),ncol=ncol(A))}
	
	switch(cset$cname,
			"cset.sum" = {
				cset.new <- cset
				cset.new$A <- rbind(cset$A,
						makenullmat(cset$A),makenullmat(cset$A))
				colnames(cset.new$A) <- colnames(cset$A)
				cset.new$b <- cset$b
				cset.new$meq <- cset$meq
				cset.new           
			},
			
			"cset.lo" = { #may not be needed
				cset.new <- cset
				cset.new$A <- rbind(cset$A,
						makenullmat(cset$A),makenullmat(cset$A))
				colnames(cset.new$A) <- colnames(cset$A)
				cset.new$b <- cset$b
				cset.new$meq <- cset$meq
				cset.new           
			},
			
			"cset.box" = {
				cset.new <- cset
				cset.new$A <- rbind(cset$A,
						makenullmat(cset$A),makenullmat(cset$A))
				colnames(cset.new$A) <- colnames(cset$A)
				cset.new$b <- cset$b
				cset.new$meq <- cset$meq
				cset.new           
			},
			
			"cset.groups" = {
				cset.new <- cset
				cset.new$A <- rbind(cset$A,
						makenullmat(cset$A),makenullmat(cset$A))
				colnames(cset.new$A) <- colnames(cset$A)
				cset.new$b <- cset$b
				cset.new$meq <- cset$meq
				cset.new           
			},
			
			"cset.mu.target" = {
				cset.new <- cset
				cset.new$A <- rbind(cset$A,
						makenullmat(cset$A),makenullmat(cset$A))
				colnames(cset.new$A) <- colnames(cset$A)
				cset.new$b <- cset$b
				cset.new$meq <- cset$meq
				cset.new           
			}
	)
	
}

#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export

# Modify constaints when propcost kicks in
propcost.modify <- function(cset=cset.i){
  switch(cset$cname,
         "cset.sum" = {
           stop("sum constraint are not combinable with propcost constraint")
         },
         
         "cset.lo" = { #may not be needed
           cset.new <- cset
           cset.new$A <- rbind(cset$A,cset$A,cset$A)
           colnames(cset.new$A) <- colnames(cset$A)
           cset.new$b <- cset$b
           cset.new$meq <- cset$meq
           cset.new      
         },
         
         "cset.box" = {
           cset.new <- cset
           cset.new$A <- rbind(cset$A,cset$A,cset$A)
           colnames(cset.new$A) <- colnames(cset$A)
           cset.new$b <- cset$b
           cset.new$meq <- cset$meq
           cset.new           
         },
         
         "cset.groups" = {
           cset.new <- cset
           cset.new$A <- rbind(cset$A,cset$A,cset$A)
           colnames(cset.new$A) <- colnames(cset$A)
           cset.new$b <- cset$b
           cset.new$meq <- cset$meq
           cset.new           
         },
         
         "cset.mu.target" = {
           cset.new <- cset
           cset.new$A <- rbind(cset$A,cset$A,cset$A)+1
           colnames(cset.new$A) <- colnames(cset$A)
           cset.new$b <- cset$b+1
           cset.new$meq <- cset$meq
           cset.new           
         }
  )
  
}


#' Constraint specifications
#' @param NULL
#' @details 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @keywords constraint
#' @family constraints
#' @examples
#' @export
# Combine multiple constraints
combine.cset <- function(clist=c(  "sum", 
                                   "lo",
                                   "box",
                                   "groups",
                                   "mu.target",
                                   "turnover.hobbs",
								   								 "turnover",
                                   "propcost"),
                         returns=returns,
                         list.arg=
                           list(sum=NULL,
                                mu.target=NULL, 
                                w.initial=NULL, 
                                toc=NULL,
                                upper=NULL,
                                lower=NULL,
                                group=NULL,
                                upper.group=NULL,
                                lower.group=NULL,
                                ptc=NULL),
						verbose=T
                                ){
  
  clist <- unique(clist)
  
  if("turnover.hobbs" %in% clist)
    is.turnover.hobbs <- TRUE else is.turnover.hobbs <- FALSE
  if("propcost" %in% clist)
    is.propcost <- TRUE else is.propcost <- FALSE
  if("turnover" %in% clist)
	is.turnover <- TRUE else is.turnover <- FALSE

  clist.names <- clist
  arg.sum.list <- list.arg
  arg.sum.list$returns <- returns
  cset <- list(NA)
  
	if(!any(c("sum","propcost")%in% clist.names )){
		clist.names <- c("sum",clist.names)
		arg.sum.list$sum = 1
	}
  
  # loop through each constraint
  for ( i in 1:length(clist.names)){
	if(verbose)
    cat(clist.names[i],"\n")
    clist.names.i <- paste("cset.",clist.names[i],sep="")
    args.names <- names(formals(clist.names.i))
    
    # loop through each argument
    for (ii in 1:length(args.names))    {
      if(is.null(eval(parse(text=paste("arg.sum.list$",args.names[ii],sep="")))))
        stop(paste("you need to specify argument ",args.names[ii]," in order to use constraint ",clist.names[i],sep="" ))
    }
    
    cset.i <- do.call(clist.names.i,arg.sum.list[args.names])    
    
	# these three constraints cannot be together
    if (is.turnover.hobbs & !(clist.names.i %in% c("cset.turnover.hobbs","cset.turnover","cset.propcost"))){
     cset[[i]] <- turnover.hobbs.modify (cset.i)
    } 
	else if(is.turnover & !(clist.names.i %in% c("cset.turnover.hobbs","cset.turnover","cset.propcost")))
	cset[[i]] <- turnover.modify (cset.i)
	else if(is.propcost & !(clist.names.i %in% c("cset.turnover.hobbs","cset.turnover","cset.propcost")))
    {
     cset[[i]] <- propcost.modify(cset.i)
    } else
    cset[[i]] <- cset.i
  }

  # remove warning case
  index.rem <- which(sapply(cset, length)!=4)
  if(length(index.rem))
  cset[[index.rem]] <- NULL
  
  #   dimnames(Amat) <- NULL
  A.all <- do.call(cbind,lapply(cset,function(x)x$A))
  colnames(A.all) <- unlist(lapply(cset,function(x)colnames(x$A)))
  b.all <- do.call(c,lapply(cset,function(x)x$b))
  names(b.all) <- colnames(A.all)
  meq.all <- do.call(c,lapply(cset,function(x)x$meq))
  
  if(sum(meq.all)!=meq.all[1]){
  # shift meq in front
  meq.pos <- sapply(cset,function(x)ncol(x$A))
  a <- NULL
  a[1] <- 1
  for (k in 2:length(meq.pos))
  a[k] <- a[k-1]+meq.pos[k-1]
  sequ <- NULL
  for (kk in 1:length(meq.pos)){
    if(meq.all[kk]!=0)
      sequ <- c(sequ, a[kk]:(a[kk]+meq.all[kk]-1))
  }
 
  A.temp <- A.all
  b.temp <- b.all
  A.all <- cbind(A.temp[,c(sequ)],A.temp[,-c(sequ)])
  b.all <- c(b.temp[c(sequ)],b.temp[-c(sequ)])
  
  # delete duplicated constraints not implemented 
  INDEX <- NULL
  for (i in 1:ncol(A.all)){
  index <- which(apply(A.all, 2, function(x)all.equal(x,A.all[,i]))=="TRUE")
  if(length(index)>1){
  index <- index[-which(index==i)]
  INDEX <- c(INDEX,index[index>i]) 
  }
  }
  if(length(INDEX)){
  A.all <- A.all[,-INDEX]
  b.all <- b.all[-INDEX]}
  meq.all <- sum(meq.all)} else{
    meq.all <- sum(meq.all)
  } 
  

  return(list(A=A.all,
              b=b.all,
              meq=meq.all,
              clist.names= clist.names))
}





