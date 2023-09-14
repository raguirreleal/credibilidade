

{
  cex = 1.3
  eixox = c(-2, 21)            # exportar PDF, 10 x 3.0 inch
  eixoy = c(0, 1)
  inf = 2
  sup = 8
  centro = (inf+sup)/2
  
  op = par(mar = c(2,1.7,0.5,0), cex.axis=cex, cex.lab=cex, mgp= c(0.6, 0.6, 0.2), 
           family = "serif", lwd = 2.5)
  
  f22 = function(E) {
    if(E >= 20) return(0)
    if(E <= centro) return(1)
    return( 1 - (E-centro)/(20-centro) )
  }
  f23 = function(E) {
    if(E >= sup) return(0)
    if(E <= inf) return(0)
    return( 1 - abs(E-centro)/(sup-centro) )
  }
  f24 = function(E) {
    if(E >= 20) return(0)
    if(E <= 0) return(0)
    if(E <= sup & E >= inf) return(1)
    if(E <= 20 & E >= sup) return(  1 - (E-sup)/(20-sup)  )
    return( 1 - (E-inf)/(-inf) )
  }
  f251 = function(E) {
    if(E >= sup) return(0)
    if(E <= inf) return(0)
    return( 1 - ((E-centro)/(sup-centro))^2 )
  }
  f252 = function(E) {
    if(E >= sup) return(0)
    if(E <= inf) return(0)
    return( 1 - sqrt(abs(E-centro)/(sup-centro)) )
  }
  
  x = seq(eixox[1], eixox[2], by = 0.01)
  
  plot(x, sapply(x, f251), type = "l",  ylab="Credibilidade  (índice)", 
       xlab="", ylim=eixoy, axes=FALSE, col=2)
  lines(x, sapply(x, f252), type = "l", col="orange")
  lines(x, sapply(x, f23), type = "l", col=4)
  lines(x, sapply(x, f24), type = "l", col=3)
  lines(x, sapply(x, f22), type = "l", col=1)
  
  att2 = seq(from=eixoy[1], to=eixoy[2], by = 0.2)
  axis(2, line=-1.3, at=att2)
  par(cex.axis=cex, mgp= c(0.7, 0.9, 0.2))
  att = seq(from=eixox[1], to=eixox[2], by = 1)
  latt = c(" ", 0, "inf", "centro", "sup", 20, " ")
  axis(1, at=c(eixox[1], 0, inf, centro, sup, 20, eixox[2]), labels=latt)
  
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.04)
  abline(v=0, lty=2, xpd=F, lwd = 1)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.03)
  abline(v=inf, lty=3, xpd=F)
  abline(v=centro, lty=3, xpd=F)
  abline(v=sup, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  
  legend(x=16, y=0.9, bty = "n", ncol=1, title = "    Índices:", title.adj=0,
         legend=c("C&K (2002)", "M (2007)", "M&S (2009)", "D&B (2014)a", "D&B (2014)b"), 
         xjust=0, y.intersp=1.8,  pt.lwd=1, lty=c(1,1,1,1,1), col=c(1,4,3,2,"orange"), cex = cex*0.8) 
  
  par(op)
  rm(op, cex, eixox, eixoy, att, att2, latt, f22, f23, f24, f251, f252, x, inf, sup, centro)
}





























