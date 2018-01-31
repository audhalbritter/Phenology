
print.parameters <- function(P, overall, response.var, title){
  xl2 <- min(P$X2.5.)
  xr2 <- max(P$X97.5.)
  xtra <- (xr2-xl2)*.1
  xl <- xl2 - (xr2-xl2)*.3  # add a bit of extra width
  xr <- xr2 + (xr2-xl2)*.1  # add a bit of extra width
  xx<- seq(xl,xr,by=.01)
  N <- dim(P)[1]
  treat.col='black'
  
  plot(range(xx), range(c(0,N+2)), xlab='', ylab='', bty='l', type='n', axes=FALSE)
  axis(1); axis(2, labels=FALSE, at=1:N)
  mtext(response.var, 1, 3, cex=1)
  mtext(P$species, 2, 1, at=N:1, cex=1, las=1)
  width.strip <-1
  for(i in N:1){
    p.temp <- pnorm(0,P$mean[i],P$sd[i])
    p1 <- min(p.temp, 1-p.temp)
    signif.symbol <- ifelse(p1 < .15 , "*"," ")
    signif.symbol <- ifelse(p1 < .10 , "**", signif.symbol)
    signif.symbol <- ifelse(p1 < .05 , "***", signif.symbol)
    text(xl, i, signif.symbol, cex=1.8, adj=0)
    # denstrip(x=xx, dens=dnorm(xx,P$mean[i],P$sd[i]),at=zz[i], colmax=treat.col, width=width.strip)  
    points(P$mean[i], i, pch=16)
    arrows(P$X2.5.[i], P$X97.5.[i], y0=i, y1=i, code=3,angle=90,lwd=1, length=.05)
  }
  points(overall$mean[i], N+1, pch=16, cex=1.5)
  arrows(overall$X2.5.[i], overall$X97.5.[i], y0=N+1, y1=N+1, code=3,angle=90,lwd=2, length=.05)
      p.temp <- pnorm(0, overall$mean[i], overall$sd[i])
      p1 <- min(p.temp, 1-p.temp)
      signif.symbol <- ifelse(p1 < .15 , "*"," ")
      signif.symbol <- ifelse(p1 < .10 , "**", signif.symbol)
      signif.symbol <- ifelse(p1 < .05 , "***", signif.symbol)
      text(xl, N+1, signif.symbol, cex=1.8, adj=0)
      mtext("Overall", 2, 1, at=N+1, cex=1, las=1)
      mtext(title, 3, 1, cex=1.5)
      abline(v=0, lty=2)
}

