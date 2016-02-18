port.cc.ss <- function(data=ret.mon, rf=0.001, lint=FALSE){
  num.stock <- ncol(data)
  mean.ret <- apply(data, 2, mean)
  vcov.ret <- cov(data)
  sigma.ret <- rep(0,num.stock)
  for (i in 1:ncol(vcov.ret)){
    sigma.ret[i]<-sqrt(vcov.ret[i,i])
    }
  corr.ret <- cor(data)
  rho <- (sum(corr.ret)-num.stock)/(num.stock*(num.stock-1))
  x <- rep(0,5*num.stock)
  xx <- matrix(x,ncol=5,nrow=num.stock)
  stock <- seq(1,num.stock,1); Rbar <- rep(0,num.stock); Rbar_f <- rep(0,num.stock)
  sigma <- rep(0,num.stock); Ratio <- rep(0,num.stock); col1 <- rep(0,num.stock)
  col2 <- rep(0,num.stock); col3 <- rep(0,num.stock)
  Rbar <- mean.ret
  Rbar_f <- Rbar-rf
  for (i in 1:num.stock){
    sigma[i] <- sqrt(vcov.ret[i,i])
    }
  Ratio <- Rbar_f/sigma.ret
  xx <- (cbind(stock, Rbar, Rbar_f, sigma, Ratio))
  aaa <- xx[order(-Ratio),]
  for(i in (1:num.stock)){
    col1[i]<-rho/(1-rho+i*rho)
    col2[i]<-sum(aaa[,5][1:i])
    }
  for(i in (1:num.stock)){
    col3[i]<-col1[i]*col2[i]
    }
  table <- cbind(aaa, col1, col2, col3)
  z_cons <- (1/((1-rho)*table[,4]))*(table[,5]-table[,8][nrow(table)])
  lint <- lint
  ifelse(lint, x_cons <- (z_cons)/sum(abs(z_cons)), x_cons <- z_cons/sum(z_cons))
  R_cons_short <- t(aaa[,2]) %*% x_cons
  tempcorr <- matrix(rho, nc=num.stock, nr=num.stock)
  for(i in (1:num.stock)){
    tempcorr[i,i]<-1
    }
  sd_cons_short <- sqrt((t(x_cons*table[,4])) %*%
    tempcorr %*% (x_cons*table[,4]))
  table <- cbind(table[,1:5],z_cons,x_cons)
  return(list(table=table,stock=names(data[(table[,1])]),
  weights=x_cons,return=R_cons_short,risk=sd_cons_short))
}
