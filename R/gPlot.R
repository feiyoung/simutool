
# This file includes the visualization tools I defined --------------------

# 绘制单幅折线图
single_line_Plot <- function(y, x=NULL, x.name="iter", y.name="logLik",
                             title.name="Title",title.col="black", title.size=20, x.axis.col='black', x.axis.size=14, y.axis.col='black', y.axis.size=14, xlab.size=18, ylab.size=18, line.size=2){
  library(ggplot2)
  if(is.null(x)) x <- 1:length(y)
  if(length(x) != length(y)) stop("x and y must have same length!")
  p1 <- ggplot(data=NULL,
               aes(x=x, y=y))

  p1 + geom_line(size=line.size) + ggtitle(title.name) + xlab(x.name) + ylab(y.name) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(size=x.axis.size,color=x.axis.col),
          axis.text.y=element_text(size=y.axis.size, color=y.axis.col),
          axis.title.x = element_text(size=xlab.size, color='black'),
          axis.title.y = element_text(size=ylab.size, color='black'),
          title= element_text(size=title.size, color=title.col)
    )
}

# 绘制多张子图的折线图

multi_line_Plot <- function(y, x=NULL, x.name="iter", y.name="logLik",
                            facet.scales='free', facet.name="criteria",
                             title.name="Title",title.col="black", title.size=20,
                            x.axis.col='black', x.axis.size=14, y.axis.col='black',
                            y.axis.size=14, xlab.size=18, ylab.size=18, line.size=2,
                            subplot.title.size=20){
  #x.name="iter"; y.name="logLik";facet.name="criteria";
  #title.name="Title";title.col="black"; title.size=20; x.axis.col='black'; x.axis.size=14; y.axis.col='black'; y.axis.size=14; xlab.size=18; ylab.size=18; line.size=2
  library(ggplot2)
  if(is.matrix(y)){
    name <- colnames(y)
    nr_y <- nrow(y)
    if(is.null(x)) x <- 1:nrow(y)
    if(is.vector(x) && length(x) == nr_y) x <- matrix(x, nr_y, ncol(y))
  }else if(is.vector(y)){
    name <- y.name
    nr_y <- length(y)
    if(is.null(x)) x <- 1: nr_y
  }
  if(is.matrix(x)) nr_x <- nrow(x)
  if(is.vector(x)) nr_x <- length(x)
  if(nr_x != nr_y) stop("dimension of x and y not match!")
  if(is.matrix(x) && is.matrix(y)){
    if(ncol(x) != ncol(y)) stop("ncol(x) and ncol(y) must be equal!")
  }

  dat <- as.data.frame(cbind(as.vector(x), as.vector(y)))
  if(is.matrix(y)) dat$method <- rep(name, each=nr_y)
  if(is.vector(y)) dat$method <- rep(y.name, nr_y)
  colnames(dat) <- c(x.name, y.name, facet.name)

  attach(dat)
  p1 <- ggplot(data=dat,
               aes(x=get(x.name), y=get(y.name)))
  p1 + geom_line(size=line.size) + facet_wrap(facets = vars(get(facet.name)),
                                              scales=facet.scales) +
  ggtitle(title.name) + xlab(x.name) + ylab(y.name) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(size=x.axis.size,color=x.axis.col),
          axis.text.y=element_text(size=y.axis.size, color=y.axis.col),
          axis.title.x = element_text(size=xlab.size, color='black'),
          axis.title.y = element_text(size=ylab.size, color='black'),
          title= element_text(size=title.size, color=title.col),
          strip.text = element_text(size=subplot.title.size)
    )
}
# hy <- matrix(1:40,10,4); colnames(hy) <- c('bic', 'aic','ebic','ic')
# multi_line_Plot(y=hy, x=1:10, title.name = 'Select number of cluster K',
#                 y.name = " ", x.name='K')

# 绘制多张子图的boxplot图
multi_boxPlot <- function(y, y.name="ARI", facet.name="method",facet.scales='free',
                            title.name="Title",title.col="black", title.size=20,
                            x.axis.col='black', x.axis.size=14, y.axis.col='black',
                            y.axis.size=14, xlab.size=18, ylab.size=18, line.size=2,
                            subplot.title.size=20){
  #x.name="iter"; y.name="logLik";facet.name="criteria";subplot.title.size=20
  # title.name="Title";title.col="black"; title.size=20; x.axis.col='black'; x.axis.size=14; y.axis.col='black'; y.axis.size=14; xlab.size=18; ylab.size=18; line.size=2
  library(ggplot2)
  if(is.matrix(y)){
    name <- colnames(y)
    nr_y <- nrow(y)
    dat <- as.data.frame(as.vector(y))
    dat$method <- rep(name, each=nr_y)
  }else if(is.list(y)){
    name <- names(y)
    dat <- as.data.frame(unlist(y))
    tmp <- unlist(lapply(y, length))
    nt <- length(y)
    method <- NULL
    for(i in 1:nt){
      method <- c(method, rep(name[i], tmp[i]))
    }
    dat$method <- method
  } else {
    stop("y must be a matrix or list!")
  }

  colnames(dat) <- c(y.name, facet.name)

  attach(dat)
  p1 <- ggplot(data=dat,
               aes(x=get(y.name), y=get(y.name)))
  p1 + geom_boxplot(size=line.size) + facet_wrap(facets = vars(get(facet.name)),
                                                 scales=facet.scales) +
    ggtitle(title.name) + xlab(" ") + ylab(y.name) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(size=x.axis.size,color=x.axis.col),
          axis.text.y=element_text(size=y.axis.size, color=y.axis.col),
          axis.title.x = element_text(size=xlab.size, color='black'),
          axis.title.y = element_text(size=ylab.size, color='black'),
          title= element_text(size=title.size, color=title.col),
          strip.text = element_text(size=subplot.title.size)
    )
}
# y <- list(simuHeterSp=1:10, PCGMM=1:20, simuHeterNosp=1:12)
# y <- matrix(1:90, 30,  3); colnames(y) <- c("simuHeterSp", "PCGMM", "simuHeterNosp")
# multi_boxPlot(y, facet.scales = 'free')

## 一张图上画多个boxplot
boxPlot <- function(y, y.name="ARI", facet.name="method",
                          title.name="Title",title.col="black", title.size=20,
                          x.axis.col='black', x.axis.size=14, y.axis.col='black',
                          y.axis.size=14, xlab.size=18, ylab.size=18, line.size=2,
                          subplot.title.size=20){
  #x.name="iter"; y.name="logLik";facet.name="criteria";subplot.title.size=20
  # title.name="Title";title.col="black"; title.size=20; x.axis.col='black'; x.axis.size=14; y.axis.col='black'; y.axis.size=14; xlab.size=18; ylab.size=18; line.size=2
  library(ggplot2)
  if(is.matrix(y)){
    name <- colnames(y)
    nr_y <- nrow(y)
    dat <- as.data.frame(as.vector(y))
    dat$method <- rep(name, each=nr_y)
  }else if(is.list(y)){
    name <- names(y)
    dat <- as.data.frame(unlist(y))
    tmp <- unlist(lapply(y, length))
    nt <- length(y)
    method <- NULL
    for(i in 1:nt){
      method <- c(method, rep(name[i], tmp[i]))
    }
    dat$method <- method
  } else {
    stop("y must be a matrix or list!")
  }

  colnames(dat) <- c(y.name, facet.name)

  attach(dat)
  p1 <- ggplot(data=dat,
               aes(x=get(facet.name), y=get(y.name), fill=get(facet.name)))
  p1 + geom_boxplot() +
    ggtitle(title.name) + xlab(" ") + ylab(y.name) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(size=x.axis.size,color=x.axis.col),
          axis.text.y=element_text(size=y.axis.size, color=y.axis.col),
          axis.title.x = element_text(size=xlab.size, color='black'),
          axis.title.y = element_text(size=ylab.size, color='black'),
          title= element_text(size=title.size, color=title.col),
          strip.text = element_text(size=subplot.title.size),
          legend.position = "none"
    )
}
# y <- matrix(1:90, 30,  3); colnames(y) <- c("simuHeterSp", "PCGMM", "simuHeterNosp")
# boxPlot(y)

## 一张图上画小提琴图
violinPlot <- function(y, y.name="ARI", facet.name="method",
                    title.name="Title",title.col="black", title.size=20,
                    x.axis.col='black', x.axis.size=14, y.axis.col='black',
                    y.axis.size=14, xlab.size=18, ylab.size=18, line.size=2,
                    subplot.title.size=20){
  #x.name="iter"; y.name="logLik";facet.name="criteria";subplot.title.size=20
  # title.name="Title";title.col="black"; title.size=20; x.axis.col='black'; x.axis.size=14; y.axis.col='black'; y.axis.size=14; xlab.size=18; ylab.size=18; line.size=2
  library(ggplot2)
  if(is.matrix(y)){
    name <- colnames(y)
    nr_y <- nrow(y)
    dat <- as.data.frame(as.vector(y))
    dat$method <- rep(name, each=nr_y)
  }else if(is.list(y)){
    name <- names(y)
    dat <- as.data.frame(unlist(y))
    tmp <- unlist(lapply(y, length))
    nt <- length(y)
    method <- NULL
    for(i in 1:nt){
      method <- c(method, rep(name[i], tmp[i]))
    }
    dat$method <- method
  } else {
    stop("y must be a matrix or list!")
  }

  colnames(dat) <- c(y.name, facet.name)

  attach(dat)
  p1 <- ggplot(data=dat,
               aes(x=get(facet.name), y=get(y.name), fill=get(facet.name)))
  p1 + geom_violin( trim = FALSE) +
    ggtitle(title.name) + xlab(" ") + ylab(y.name) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(size=x.axis.size,color=x.axis.col),
          axis.text.y=element_text(size=y.axis.size, color=y.axis.col),
          axis.title.x = element_text(size=xlab.size, color='black'),
          axis.title.y = element_text(size=ylab.size, color='black'),
          title= element_text(size=title.size, color=title.col),
          strip.text = element_text(size=subplot.title.size),
          legend.position = "none"
    )
}
# y <- matrix(1:90, 30,  3); colnames(y) <- c("simuHeterSp", "PCGMM", "simuHeterNosp")
# violinPlot(y)
