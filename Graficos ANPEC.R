




{ ## 77777777777777777
  prev = 3                         # exportar PDF, 10 x 3 inch
  cex = 1
  ylim = c(0, 1)
  atcred = substr(tri,1,4)
  op = par(mar=c(1.2, 1.5, 0, 0), cex.axis=cex, cex.lab=1.1, mgp= c(0.6, 0.6, 0), mfcol=c(1,1), family = "serif")
  #+++++++++++++++++++++++++
  I = c(3, 5, 7, 9, 11, 13, 15)  # i: anos impares de 2009 a 2021
  vlty = c(1, seq(1,6))
  vcol = seq(1, 7)
  for(j in 1:length(I)) { 
    i = I[j]
    xc=NULL
    for(k in (min(nprev,i)-1):0){
      cta = atcred==anos[i-k]
      xc = c(xc, CRED[cta,(k+1)])
    }
    xc = c(rep(NA,12-length(xc)), xc)
    if(i==3) plot(xc, type="l", ylim=ylim, axes=FALSE, xlab="", ylab="Credibilidade", 
                  lwd=2, col=vcol[1], lty=vlty[1])
    else lines(xc, type="l", col=vcol[j], lty=vlty[j], lwd=2)
  }
  att2 = round(seq(ylim[1], ylim[2], ylim[2]/5),2)
  axis(2, pos=0.8, at=att2)
  legend(x=1.2, y=0.35, bty = "n", ncol=4, title = "", title.adj=0, 
         legend=c("y=2009 ", "y=2011 ", "y=2013 ", "y=2015 ", "y=2017 ", "y=2019 ", "y=2021 "), 
         xjust=0, y.intersp=2,  lty=vlty, col=vcol, cex = cex, lwd=2) 
  par(mgp= c(0.4, 0.6, 0), cex.axis=cex)
  latt = paste0(c(paste0("(y-",rep(2:1,each=4),")"), rep("y",4)), paste0("Q",1:4))
  axis(1, at=1:12, labels=latt, las=1, pos=-0.01)  
  
  par(op)
  rm(op, latt, cex, i, prev, k, xc, cta, atcred, ylim, att2)
}

















######## 66666666666666666666666666666666666
cex = 1
eixox = c(1, ntri)            # exportar PDF, 10 x 4 inch
eixoy = c(0, 1)

y = ts(CRED, start = c(2007,1), frequency = 4)
att = time(y)[cycle(y)==1 | cycle(y)==3]

op = par(mar=c(1, 2.2, 0, 0), cex.axis=1.3, cex.lab=1.5, mgp= c(1, 0.6, 0), family = "serif")

plot(y[,1],  ylab="Credibilidade", 
     xlab="", ylim = eixoy, axes=FALSE, lwd=2) 
# lines(y[,2], col=2)
# lines(y[,3], col=4)
y = ts(sCREDm, start = c(2007,1), frequency = 4)
lines(y[,1], col=2, lty=1, lwd=2)
# lines(y[,2], col=2, lty=2)

latt = substr(tri[cycle(y)==1 | cycle(y)==3], 3,6)
axis(1, at=att, labels=latt, pos=eixoy[1], tcl=0.2)
axis(1, at=att, labels=latt, pos=eixoy[1], lwd=0, lwd.ticks=1, tcl=-0.2)
att2 = seq(eixoy[1], eixoy[2], eixoy[2]/5)
axis(2, line=-1, at=att2)

legend(x=2006.7, y=0.29, bty = "n", ncol=1, title = "   (y+h)", title.adj=0, lwd=2,
       legend=c("h=0", "h=0 (prob)"), xjust=0, y.intersp=1,  lty=c(1,1), col=c(1,2), cex = 1.5)  








#+++++++++++++++++++
eixoy = c(0.3, 1)
eixoy = c(0, 1)
par(mar=c(1, 2.2, 0, 0), cex.axis=1.3, cex.lab=1.5, mgp= c(1, 0.6, 0))

y = ts(CRED, start = c(2007,1), frequency = 4)
plot(y[,1],  ylab="Credibilidade", type="n",
     xlab="", ylim = eixoy, axes=FALSE, lwd=2) 
lines(y[,2], col=1, lwd=2)
lines(y[,3], col=2, lwd=2)
y = ts(sCREDm, start = c(2007,1), frequency = 4)
# lines(y[,1], col=2, lty=1)
lines(y[,2], col=1, lty=2, lwd=2)
y = ts(CRED510y, start = c(2007,1), frequency = 4)
lines(y[,1], col=3, lty=1, lwd=2)
lines(y[,2], col=4, lty=1, lwd=2)

latt = substr(tri[cycle(y)==1 | cycle(y)==3], 3,6)
axis(1, at=att, labels=latt, pos=eixoy[1], tcl=0.2)
axis(1, at=att, labels=latt, pos=eixoy[1], lwd=0, lwd.ticks=1, tcl=-0.2)

# att2 = c(0.3, 0.4, 0.6, 0.8, 1.0)
# latt2 = c(" ",  format(att2[2:5], digits=1) )
# axis(2, line=-1, at=att2, labels=latt2)
att2 = seq(eixoy[1], eixoy[2], eixoy[2]/5)
axis(2, line=-1, at=att2)

legend(x=2008.50, y=0.22, bty = "n", ncol=4, title = "   Séries (y+h):", title.adj=0, 
       legend=c("h=1 ", "h=2 ", "h5 ", "h10 "), 
       xjust=0, y.intersp=1,  lty=c(1,1,1,1), col=c(1,2,3,4), cex = 1.5, lwd=2) 
legend(x=2018.20, y=0.22, bty = "n", ncol=1, title = " ", title.adj=0, 
       legend="h=1 (prob)", xjust=0, y.intersp=1, lty=2, col=1, cex = 1.5, lwd=2) 










#+++++++++++++++++++
eixoy = c(0, 1)
par(mar=c(1, 2.2, 0, 0))

y = ts(CREDtri, start = c(2007,1), frequency = 4)
plot(y[,1],  ylab="Credibilidade", 
     xlab="", ylim = eixoy, axes=FALSE, lwd=2) 
lines(y[,2], col=2, lwd=2)
lines(y[,3], col=3, lwd=2)
lines(y[,4], col=4, lty=1, lwd=2)
lines(y[,5], col="orange", lty=1, lwd=2)

latt = substr(tri[cycle(y)==1 | cycle(y)==3], 3,6)
axis(1, at=att, labels=latt, pos=eixoy[1], tcl=-0.2)
att2 = seq(eixoy[1], eixoy[2], eixoy[2]/5)
axis(2, line=-1, at=att2)

legend(x=2015.3, y=0.33, bty = "n", ncol=2, title = "   Séries (t+h):", title.adj=0, 
       legend=c("h=0 ", "h=1 ", "h=2 ", "h=3 ", "h=4 "), 
       xjust=0, y.intersp=1,  lty=c(1,1,1,1,1), col=c(1,2,3,4,"orange"), cex = 1.5, lwd=2) 





par(op)


