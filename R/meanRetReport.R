meanRetReport <- function(x,robust = FALSE)
{
  if(robust == FALSE) {retMu <- mean(ret)} else
    {x <- RobStatTM::locScaleM(ret)
    retMu <- x$mu
  }
  logret <- log(ret+1)
  if(robust == FALSE) {logretMu <- mean(logret)} else
    {x <- RobStatTM::locScaleM(logret)
    logretMu <- x$mu
  }
  g <- exp(logretMu)-1
  if(robust == FALSE) {S_squared <- var(ret)} else
    {scale <- robustbase::scaleTau2(ret)
    S_squared <- scale^2
  }
  gApprox <- retMu - S_squared/2
  c(retMu,logretMu,g,gApprox)
}
  
  
 