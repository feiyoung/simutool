
rmixnorm <- function(n, P, vmu, vsigma){
  d <- length(P)
  res<- .C("rmixnorm", as.integer(n), as.double(P), as.integer(d),
           as.double(vmu), as.double(vsigma), result=double(n),
           DUP = TRUE, PACKAGE='simutool')
  return(res$result)
}

