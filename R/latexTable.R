latexTable <- function(res, paraname = 'beta',align=NULL, digits=NULL, nhline=3, lsub=as.character(1:(nrow(res)/nhline)), side=1){
  # paraname = 'beta'
  # align=NULL
  # digits=NULL
  # nhline=3
  # lsub=as.character(1:(nrow(res)/nhline))
  # side= 2: set how many parameters arrange in one line
  ##' @export
  if(!is.matrix(res)) stop('res must be a matrix!')
  if(mode(res)!='numeric') stop('res must be a numeric mode!')
  m <- ncol(res)
  n <- nrow(res)
  m1 <- m*side
  if(length(align)==m1 && !is.null(paraname)) align <- c('c','c', align,'c',align)
  if(length(align)==m1 && is.null(paraname)) align <- c('c', align, 'c', align)
  K <- n/nhline
  if(!is.character(lsub)) lsub <- as.character(lsub)
  if(length(lsub) != K){'Length of lsub must be equal to ncol(res)/nhline'}
  if(is.null(row.names(res))){
    row.names(res) <- paste0('row',1:n)
    warning('rownames of res is null, so I use row1,...,rown as rownames!')
  }
  if(is.null(colnames(res))){
    colnames(res) <- paste0('col',1:m)
    warning('rownames of res is null, so I use col1,...,colm as colnames!')
  }
  if(is.null(align) && !is.null(paraname)) align <- rep('c',m1+1+side)
  if(is.null(align) && is.null(paraname)) align <- rep('c',m1+side)
  if(is.null(digits)) digits <- rep(4,m1)
  if(length(digits) == 1) digits <- rep(digits, m1)
  if(length(digits) >1) digits <- c(digits, rep(digits, m1-length(digits)))
  colname <- ifelse(is.null(paraname), '', '& ')
  for(j in 1:m) colname <- paste(colname, colnames(res)[j], sep='&  ')
  k <- 1
 if(side == 1){
  for(i in 0:(n+1)){
    if(i == 0){
      cat("
\\begin{table}[H]
\\centering
\\begin{tabular}{",align,"}
\\hline\n",
          paste0(colname, '  \\\\'),'\\hline\n',
          "\n")
    }else if(i<=n){
      if(is.null(paraname)) tmp <- row.names(res)[i]
      if(!is.null(paraname)) tmp <- paste('& ', row.names(res)[i], sep=' ')
      for(j in 1:m){
        tmp <- paste(tmp, format(res[i,j],digits = digits[j], nsmall = digits[j]), sep='&  ')
      }
      if(i%%nhline==0){
        cat(paste0(tmp, '  \\\\ \\hline'),'\n')
      }else if(i %% nhline ==1 && !is.null(paraname)){
        cat(paste0('$\\',paraname,'_{',lsub[k],'}$',tmp, '  \\\\'),'\n')
        k <- k + 1 # count number
      }else{
        cat(paste0(tmp, '  \\\\'),'\n')
      }
    }
    else{
      cat("
\\end{tabular}
\\end{table} \n")
    }
  }
 }else{
    cat('side is not equal to 1 but ', side,'! \n')
   colname <- ifelse(is.null(paraname), '', '& ')
   for(s in 1:side){
     for(j in 1:m) colname <- paste(colname, colnames(res)[j], sep='&  ')
     if(s != side) colname <- paste0(colname, '& ' )
   }
   cat("
\\begin{table}[H]
\\centering
\\begin{tabular}{",align,"}
\\hline\n",
       paste0(colname, '  \\\\'),'\\hline\n',
       "\n")
   flag <- 1
   for(f in 1:(n/side/nhline)){
   nurow <- n / side

     if(is.null(paraname)) tmp <- row.names(res)[flag]
     if(!is.null(paraname)) tmp <- paste('& ', row.names(res)[flag], sep=' ')

     for(k in 1:nhline){
       tmp <- row.names(res)[k]
     for(j in 1:side){
       if(!is.null(paraname) && j==1){
         if(k == 1){
           tmp <- paste0('$\\',paraname,'_{',lsub[(f-1)*side + j],'}$','& ', tmp)
         }else{
           tmp <- paste0('&  ', tmp)
         }
       }
         for(l in 1:m){
         tmp <- paste(tmp, format(res[(j-1)*nhline+k+flag-1,l],digits = digits[(j-1)*m+m], nsmall = digits[(j-1)*m+m]), sep='&  ')
         }
       if(j != side && is.null(paraname)){
         tmp <- paste0(tmp, '&', row.names(res)[k])
       }
       if(!is.null(paraname) && j!=side){
         if(k == 1){
            tmp <- paste0(tmp,'& ', '$\\',paraname,'_{',lsub[(f-1)*side + j+1],'}$')
         }else{
           tmp <- paste0(tmp, '&  ')
         }
       }

     }
       if(k == nhline){
         cat(paste0(tmp, '  \\\\ \\hline'),'\n')
       }else{
         cat(paste0(tmp, '  \\\\'),'\n')
       }

     }
     flag <- flag + side*nhline
   }
   cat("
\\end{tabular}
\\end{table} \n")
 }

}
