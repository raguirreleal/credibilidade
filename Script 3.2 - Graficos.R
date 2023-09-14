



# PARA SCRIPT 3.0 ========================================================================================

{
  op = par(mar = c(3,2.2,0,0), cex.axis=0.8, cex.lab=1, mgp= c(1.2, 0.6, 0), mfcol=c(1,2))
  
  plot(ttCRED[[1]]$prev3, type = "l", ylab="Credibilidade", xlab="",
       ylim = c(0,1), axes=FALSE, lwd=2)
  lines(ttCRED[[2]]$prev3, type = "l", col=2, lwd=2)
  lines(ttCRED[[3]]$prev3, type = "l", col=4, lwd=2)
  lines(ttCRED[[5]]$prev3, type = "l", col=3, lwd=2)
  
  #title(xlab = "Trimestres", mgp=c(2.4, 0.6, 0))
  
  # legend(x=11, y=0.9, bty = "n",, ncol=1, title = "Intervalo",
  #        legend=c("0.50", "0.25", "0.10", "0.01"), lwd=2,
  #        lty=c(1,1,1,1), col=c(1,2,4,3), cex=0.9)
  axis(2, line=-0.5, at=seq(0, 1, by=0.1))
  att = seq(from=1, to=43, by = 2)
  axis(1, at=att, labels=substring(tri[att], 3), las=3)
  
  #+++++++++++++++++++++ 
  
  topx = max(ttCRED[[1]]$prev3)
  plot(ttCRED[[1]]$prev3, type = "l", ylab="(várias escalas)", xlab="",
       ylim = c(0,1), axes=FALSE, lwd=2, lty=1, mgp= c(0.4, 0.6, 0.5))
  aj1 = topx / max(ttCRED[[2]]$prev3)
  aj2 = topx / max(ttCRED[[3]]$prev3)
  aj3 = topx / max(ttCRED[[5]]$prev3)
  lines(aj1*ttCRED[[2]]$prev3, type = "l", col=2, lwd=2, lty=1)
  lines(aj2*ttCRED[[3]]$prev3, type = "l", col=4, lwd=2, lty=1)
  lines(aj3*ttCRED[[5]]$prev3, type = "l", col=3, lwd=2, lty=1)
  
  # title(xlab = "Trimestres", mgp=c(2.4, 0.6, 0))
  
  legend(x=22, y=0.18, bty = "n", ncol=2, title = "Intervalos:",
         legend=c("0.50", "0.25", "0.10", "0.01"), lwd=2,
         lty=c(1,1,1,1), col=c(1,2,4,3), cex=1)
  axis(2, line=-0.5, at=c(0,2))
  att = seq(from=1, to=43, by = 2)
  axis(1, at=att, labels=substring(tri[att], 3), las=3)
  par(op)
}

{
  op = par(mar = c(4,2,1,0), cex.axis=0.8, cex.lab=0.9, mgp= c(0.8, 0.6, 0))
  plot(ttCRED[[1]]$prev3, type = "b", ylab="Credibilidade", xlab="",
       ylim = c(0,1), axes=FALSE, lwd=3, lty=3, pch=20)
  lines(ttCRED[[2]]$prev3, type = "o", col=2, lwd=2, lty=5)
  lines(ttCRED[[3]]$prev3, type = "o", col=4, lwd=2, pch=19)
  lines(ttCRED[[5]]$prev3, type = "o", col=3, lwd=2, pch=8)
  
  title(xlab = "Trimestres", mgp=c(2.4, 0.6, 0))
  
  legend(x=11, y=0.9, bty = "n",, ncol=2, title = "Intervalo",
         legend=c("0.50", "0.25", "0.10", "0.01"), lwd=2,
         lty=c(3,5,1,1), pch=c(20,1,19,8), col=c(1,2,4,3), cex=0.9)
  axis(2, line=-1, at=seq(0, 1, by=0.1))
  att = seq(from=1, to=43, by = 2)
  axis(1, at=att, labels=substring(tri[att], 3), las=3)
  
  par(op)
}



{
  k = 4
  i = 15
  cex = 0.75   # exportar PDF, 6.25 x 3.0 inch
  eixox = c(-0.5, 3.5)
  eixoy = c(0, 1.6)
  
  op = par(mar = c(1.7,2.3,0.5,0), cex.axis=0.6, cex.lab=cex, mgp= c(1.4, 0.7, 0), mfcol=c(1,2), family = "serif")
  
  plot(fDENS[[k]][i],  ylab="Densidade de Probabilidade", xlab="",   # 5 - 42
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000, lty=1)
  par(new = TRUE)
  plot(fDENS2[[k]][i], col=2, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000, lty=2)
  
  att = seq(from=eixox[1], to=eixox[2], by = 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  att2 = seq(from=eixoy[1], to=eixoy[2], by = 0.2)
  axis(2, line=0, at=att2)

  # legend(x=2.3, y=1, bty = "n", ncol=1, title = " Fdps:",
  #        legend=expression(sigma: OS, sigma: SJ), xjust=0, y.intersp=1.4,
  #        lty=c(1,2), col=c(1,2), cex = 0.75)
  
  text(2.7, 1.5, paste0("t' = ", tri[i]), cex = cex)
  text(2.7, 1.4, paste0("t'+ h = 2011Q2"), cex = cex)
  
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.06)
  abline(v=0, lty=2, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.75)
  abline(v=2, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  prev = (na.exclude(dados[dados$NAMEQ == tri[i],(k+4)]))[[1]]
  rug(prev, ticksize = 0.04, side = 1, lwd = 1, col="blue")
  
  #+++++++++++++++++++++ 
  
  k = 1
  i = 35
  par(mar = c(1.7,0.5,0.5,0))
  
  plot(fDENS[[k]][i],  ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000, lty=1)
  par(new = TRUE)
  plot(fDENS2[[k]][i], col=2, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000, lty=2)
  
  att = seq(from=eixox[1], to=eixox[2], by = 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  
  legend(x=2.3, y=1, bty = "n", ncol=1, title = "   Fdps:",
         legend=expression(sigma: OS, sigma: SJ), xjust=0, y.intersp=1.7,
         lty=c(1,2), col=c(1,2), cex = 0.75)
  
  text(2.7, 1.5, paste0("t' = ", tri[i]), cex = cex)
  text(2.7, 1.4, paste0("t'+ h = ", tri[i]), cex = cex)
  
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.06)
  abline(v=0, lty=2, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.75)
  abline(v=2, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  prev = (na.exclude(dados[dados$NAMEQ == tri[i],(k+4)]))[[1]]
  rug(prev, ticksize = 0.04, side = 1, lwd = 1, col="blue")
  
  par(op)
  rm(op, cex, eixox, eixoy, att, att2, prev)
}




{
  cex = 0.75
  eixox = c(1, ntri)            # exportar PDF, 6.25 x 3.0 inch
  eixoy = c(1, 2.2)
  att = time(tsj)[cycle(tsj)==1 | cycle(tsj)==3]
  latt = substr(tri[cycle(tsj)==1 | cycle(tsj)==3], 3,6)
  
  op = par(mar=c(2, 1.4, 0, 0), cex.axis=0.6, cex.lab=cex, mgp= c(0.6, 0.7, 0), mfcol=c(1,1), family = "serif")
  
  plot(tsj.seas$series$s12,  ylab="Esperança da expectativa de inflação", 
       xlab="", ylim = eixoy, axes=FALSE, lwd=1) 
  lines(tsj.seas2$series$s12, col=2) 
  lines(tsj.seas3$series$s12, col=3)
  lines(tsj.seas4$series$s12, col=4)
  lines(tsj.seas5$series$s12, col="orange")
  
  axis(1, at=att, labels=latt, las=3)
  att2 = seq(from=eixoy[1], to=eixoy[2], by = 0.2)
  axis(2, line=-1, at=att2)
  
  legend(x=2007.25, y=1.5, bty = "n", ncol=1, title = "   Séries:", title.adj=0,
         legend=c("h = 0", "h = 1", "h = 2", "h = 3", "h = 4"), 
         xjust=0, y.intersp=1.4,  lty=c(1,1,1,1,1), col=c(1,2,3,4,"orange"), cex = 0.75)  
  
  abline(h=2, lty=3, xpd=F)
  
  par(op)
  rm(op, cex, eixoy, att, att2, latt)
}




{  # 4444444444444444444444
  prev = 1
  i1 = 40
  i2 = 11 # 3   11
  ttm = 0.5                               # exportar PDF, 10 x 3.0 inch
  ylim = c(0,1.5)
  xlim = c(0,3.5)
  s = seq(xlim[1], xlim[2], 0.01)
  spoly = seq(meta-ttm, meta+ttm, 0.01)
  cex = 1
  op = par(mar=c(1.9, 2.5, 0, 0), cex.axis=0.8, cex.lab=cex, mgp= c(1.6, 0.7, 0), mfcol=c(1,2), 
           family = "serif", lwd=2)
  #++++++++++++
  y = eval.fd(s, sDENSm[[prev]][i1])
  y2 = eval.fd(s, DENSm[[prev]][[i1]])
  plot(s, y, type="n", ylab="Densidade de Probabilidade", xlab="", ylim=ylim, axes=FALSE)
  ypoly = eval.fd(spoly, sDENSm[[prev]][i1])
  ypoly2 = eval.fd(spoly, DENSm[[prev]][[i1]])
  polygon( c(spoly, rev(spoly)), c( ypoly2, rep(0, length(ypoly)) ), col=gray(0.8), border = NA)
  polygon( c(spoly, rev(spoly)), c( ypoly, rep(0, length(ypoly)) ), col=gray(0.6), border = NA)
  lines(s, y2, type="l")
  lines(s, y, type="l", col=2)
  
  text(0.5, 1.3, paste0("t' = ", tri[i1]), cex = cex)
  text(0.5, 1.2, paste0("y'+ h = ", as.double(substr(tri[i1],1,4))-1+prev ), cex = cex)
  
  att = seq(xlim[1], xlim[2], 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  att = seq(ylim[1], ylim[2], 0.5)
  axis(2, line=0, at=att)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-3.12)
  abline(v=meta, lty=3)
  abline(v=c(meta-ttm, v=meta+ttm), col=1)
  #++++++++++++
  par(mar=c(1.9, 0, 0, 0))
  y = eval.fd(s, fDENS[[prev]][i2])
  plot(s, y, type="l", ylab="Densidade de Probabilidade", xlab="", ylim=ylim, axes=FALSE)
  ypoly = eval.fd(spoly, fDENS[[prev]][i2])
  polygon( c(spoly, rev(spoly)), c( ypoly, rep(0, length(ypoly)) ), col=gray(0.7), border = NA)
  lines(s, y, type="l")
  
  text(0.5, 1.3, paste0("t' = ", tri[i2]), cex = cex)
  text(0.5, 1.2, paste0("y'+ h = ", as.double(substr(tri[i2],1,4))-1+prev ), cex = cex)
  
  att = seq(xlim[1], xlim[2], 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-3.12)
  abline(v=meta, lty=3)
  abline(v=c(meta-ttm, v=meta+ttm), col=1)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.13)
  abline(v=0, lty=2, xpd=F, lwd = 1)
  
  prev = (na.exclude(dados[dados$NAMEQ == tri[i2],(prev+4)]))[[1]]
  rug(prev, ticksize = 0.038, side = 1, lwd = 2, col="blue")
  #++++++++++++
  par(op)
  rm(op, att, cex, i1, i2, ypoly, s, y, spoly)
  rm(prev, ttm, ylim, xlim)
}




{
  prev = 3                         # exportar PDF, 6.25 x 4.0 inch
  cex = 0.75
  op = par(mar=c(1, 1.1, 0, 0), cex.axis=0.6, cex.lab=cex, mgp= c(0.4, 0.6, 0), mfcol=c(2,1), family = "serif")
  #+++++++++++++++++++++++++
  ttm = 0.5
  t  = 0.5^(1:3)
  ylim = c(0.2, 1)
  tt = sort(c(ttm, ttm*(1-t), ttm*(1+t)), T)
  imedtt = which(tt==median(tt))
  ttCRED = foreach(i = 1:length(tt), .combine = cbind) %do% func.CRED(tt[i])[,prev]  
  y = ts(ttCRED, start = c(2007,1), frequency = 4)
  plot(y[,imedtt], type="l", ylab="Credibilidade", xlab="", ylim = ylim, axes=FALSE)
  cinza = c(seq(from=0.4, by=0.4/(length(tt)-imedtt-1), length.out=(length(tt)-imedtt-1)),  0.8 )
  # cinza = seq(from=0.4, by=0.5/(length(tt)-1), length.out=length(tt))
  for(i in (imedtt-1):1 ) {
    par(new = T)
    polygon( c(time(y),rev(time(y))), c(y[,i], rev(y[,i+1])), col=gray(rev(cinza)[i]), border = NA)
  }
  for(i in (imedtt+1):length(tt) ) {
    par(new = T)
    polygon( c(time(y),rev(time(y))), c(y[,i], rev(y[,i-1])), col=gray(cinza[i-imedtt]), border = NA)
  }
  # for(i in 2:length(tt) ) {
  #   par(new = T)
  #   polygon( c(time(y),rev(time(y))), c(y[,i], rev(y[,i-1])), col=gray(cinza[i-1]), border = NA)
  # }
  lines(y[,imedtt], type="l", col=1, lwd=2 )
  
  att = time(y)[cycle(y)==1 | cycle(y)==3]
  latt = rep(" ", length(att))
  axis(1, at=att, labels=latt, pos=ylim[1]-0.07, tcl=0.2)
  axis(1, at=att, labels=latt, pos=ylim[1]-0.07, lwd=0, lwd.ticks=1, tcl=-0.2)
  att2 = seq(ylim[1], ylim[2], ylim[2]/5)
  axis(2, line=-0.85, at=att2)
  # clip(par("usr")[1], par("usr")[2]-0.4, par("usr")[3], par("usr")[4])
  # abline(h=1, lty=3)
  legend(x=2009.5, y=0.9, bty = "n", ncol=1, legend=expression(alpha == 0.5), lty=1, col=1, cex = cex, lwd=2)  
  #+++++++++++++++++++++++++
  ttm = 0.005
  t  = 0.5^(1:3)
  ylim = c(0, 0.028)
  par(mar=c(2, 1.1, 0, 0))
  tt = sort(c(ttm, ttm*(1-t), ttm*(1+t)), T)
  imedtt = which(tt==median(tt))
  ttCRED = foreach(i = 1:length(tt), .combine = cbind) %do% func.CRED(tt[i])[,prev]  
  y = ts(ttCRED, start = c(2007,1), frequency = 4)
  plot(y[,imedtt], type="l", ylab="Credibilidade", xlab="", ylim = ylim, axes=FALSE)
  cinza = (c(seq(from=0.4, by=0.4/(length(tt)-imedtt-1), length.out=(length(tt)-imedtt-1)),  0.8 ))
  for(i in (imedtt-1):1 ) {
    par(new = T)
    polygon( c(time(y),rev(time(y))), c(y[,i], rev(y[,i+1])), col=gray(rev(cinza)[i]), border = NA)
  }
  for(i in (imedtt+1):length(tt) ) {
    par(new = T)
    polygon( c(time(y),rev(time(y))), c(y[,i], rev(y[,i-1])), col=gray(cinza[i-imedtt]), border = NA)
  }
  lines(y[,imedtt], type="l", col=1, lwd=2 )
  
  att = time(y)[cycle(y)==1 | cycle(y)==3]
  latt = substr(tri[cycle(y)==1 | cycle(y)==3], 3,6)
  axis(1, at=att, labels=latt, las=3)
  #att2 = round(seq(ylim[1], ylim[2], ylim[2]/5),2)
  att2 = c(0, 0.007, 0.014, 0.021, 0.028)
  axis(2, line=-0.85, at=att2, labels=att2)
  legend(x=2009.5, y=0.025, bty = "n", ncol=1, legend=expression(alpha==0.005), lty=1, col=1, cex = cex, lwd=2)  
  
  par(op)
  rm(op, att, latt, cex, i, t, tt, y, imedtt, cinza, prev)
}



{ ###5555555555555555555555555
  prev = 3                         # exportar PDF, 10 x 3.0 inch
  cex = 0.9
  op = par(mar=c(1.1, 1.5, 0, 0), cex.axis=0.8, cex.lab=cex, mgp= c(0.6, 0.6, 0), mfcol=c(1,1), family = "serif")
  #+++++++++++++++++++++++++
  ttm = 0.5
  t  = 0.5^(1:3)
  ylim = c(0, 1)
  tt = sort(c(ttm, ttm*(1-t), ttm*(1+t)), T)
  imedtt = which(tt==median(tt))
  ttCRED = foreach(i = 1:length(tt), .combine = cbind) %do% func.CRED(tt[i])[,prev]  
  y = ts(ttCRED, start = c(2007,1), frequency = 4)
  plot(y[,imedtt], type="l", ylab="Credibilidade", xlab="", ylim = ylim, axes=FALSE)
  cinza = c(seq(from=0.4, by=0.4/(length(tt)-imedtt-1), length.out=(length(tt)-imedtt-1)),  0.8 )
  # cinza = seq(from=0.4, by=0.5/(length(tt)-1), length.out=length(tt))
  for(i in (imedtt-1):1 ) {
    par(new = T)
    polygon( c(time(y),rev(time(y))), c(y[,i], rev(y[,i+1])), col=gray(rev(cinza)[i]), border = NA)
  }
  for(i in (imedtt+1):length(tt) ) {
    par(new = T)
    polygon( c(time(y),rev(time(y))), c(y[,i], rev(y[,i-1])), col=gray(cinza[i-imedtt]), border = NA)
  }
  # for(i in 2:length(tt) ) {
  #   par(new = T)
  #   polygon( c(time(y),rev(time(y))), c(y[,i], rev(y[,i-1])), col=gray(cinza[i-1]), border = NA)
  # }
  lines(y[,imedtt], type="l", col=1, lwd=2 )
  
  att = time(y)[cycle(y)==1 | cycle(y)==3]
  latt = substr(tri[cycle(y)==1 | cycle(y)==3], 3,6)
  axis(1, at=att, labels=latt, pos=ylim[1], lwd.ticks=1)
  att2 = seq(ylim[1], ylim[2], ylim[2]/5)
  axis(2, line=-0.85, at=att2)
  # clip(par("usr")[1], par("usr")[2]-0.4, par("usr")[3], par("usr")[4])
  # abline(h=1, lty=3)

  par(op)
  rm(op, att, latt, cex, i, t, tt, y, imedtt, cinza, prev)
}



{ ######## 66666666666666666666666666666666666
  cex = 1
  eixox = c(1, ntri)            # exportar PDF, 10 x 4 inch
  eixoy = c(0, 1)
  
  y = ts(CRED, start = c(2007,1), frequency = 4)
  att = time(y)[cycle(y)==1 | cycle(y)==3]

  op = par(mar=c(0.5, 2.2, 0.3, 0), cex.axis=1.3, cex.lab=1.5, mgp= c(1, 0.6, 0), mfcol=c(3,1), family = "serif")
  
  plot(y[,1],  ylab="Credibilidade", 
       xlab="", ylim = eixoy, axes=FALSE, lwd=2) 
  # lines(y[,2], col=2)
  # lines(y[,3], col=4)
  y = ts(sCREDm, start = c(2007,1), frequency = 4)
  lines(y[,1], col=2, lty=1, lwd=2)
  # lines(y[,2], col=2, lty=2)
  
  latt = rep(" ", length(att))
  axis(1, at=att, labels=latt, pos=eixoy[1], tcl=0.2)
  axis(1, at=att, labels=latt, pos=eixoy[1], lwd=0, lwd.ticks=1, tcl=-0.2)
  att2 = seq(eixoy[1], eixoy[2], eixoy[2]/5)
  axis(2, line=-1, at=att2)
  
  legend(x=2006.7, y=0.55, bty = "n", ncol=1, title = "   Séries (y+h):", title.adj=0, lwd=2,
         legend=c("h=0", "h=0 (prob)"), xjust=0, y.intersp=1,  lty=c(1,1), col=c(1,2), cex = 1.5)  
  #+++++++++++++++++++
  eixoy = c(0.3, 1)
  eixoy = c(0, 1)
  par(mar=c(0.5, 2.2, 0, 0), cex.axis=1.3, cex.lab=1.5, mgp= c(1, 0.6, 0))
  
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
  
  latt = rep(" ", length(att))
  axis(1, at=att, labels=latt, pos=eixoy[1], tcl=0.2)
  axis(1, at=att, labels=latt, pos=eixoy[1], lwd=0, lwd.ticks=1, tcl=-0.2)

  # att2 = c(0.3, 0.4, 0.6, 0.8, 1.0)
  # latt2 = c(" ",  format(att2[2:5], digits=1) )
  # axis(2, line=-1, at=att2, labels=latt2)
  att2 = seq(eixoy[1], eixoy[2], eixoy[2]/5)
  axis(2, line=-1, at=att2)
  
  legend(x=2011.50, y=0.4, bty = "n", ncol=4, title = "   Séries (y+h):", title.adj=0, 
         legend=c("h=1 ", "h=2 ", "h5 ", "h10 "), 
         xjust=0, y.intersp=1,  lty=c(1,1,1,1), col=c(1,2,3,4), cex = 1.5, lwd=2) 
  legend(x=2017.60, y=0.4, bty = "n", ncol=1, title = " ", title.adj=0, 
         legend="h=1 (prob)", xjust=0, y.intersp=1, lty=2, col=1, cex = 1.5, lwd=2) 
  #+++++++++++++++++++
  eixoy = c(0, 1)
  par(mar=c(1.6, 2.2, 0, 0))
  
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
  
  legend(x=2011.50, y=0.4, bty = "n", ncol=5, title = "   Séries (t+h):", title.adj=0, 
         legend=c("h=0 ", "h=1 ", "h=2 ", "h=3 ", "h=4 "), 
         xjust=0, y.intersp=1,  lty=c(1,1,1,1,1), col=c(1,2,3,4,"orange"), cex = 1.5, lwd=2) 
  
  
  par(op)
  rm(op, cex, eixoy, eixox, att, att2, latt, y)
}



{
  prev = 3                         # exportar PDF, 6.25 x 4.0 inch
  cex = 0.75
  ylim = c(0, 1)
  atcred = substr(tri,1,4)
  op = par(mar=c(1, 1.5, 0, 0), cex.axis=0.6, cex.lab=cex, mgp= c(0.6, 0.6, 0), mfcol=c(2,1), family = "serif")
  #+++++++++++++++++++++++++
  for(i in 2:6) { # i: anos de 2008 a 2012
    xc=NULL
    for(k in (min(nprev,i)-1):0){
      cta = atcred==anos[i-k]
      xc = c(xc, CRED[cta,(k+1)])
    }
    xc = c(rep(NA,12-length(xc)), xc)
    if(i==2) plot(xc, type="l", ylim=ylim, axes=FALSE, xlab="", ylab="Credibilidade")
    else lines(xc, type="l", col=ifelse(i==6,"orange",i-1))
  }
  axis(1, at=1:12, labels=rep(" ",12), pos=ylim[1]-0.1, tcl=0.2)
  axis(1, at=1:12, labels=rep(" ",12), pos=ylim[1]-0.1, lwd=0, lwd.ticks=1, tcl=-0.2)
  att2 = seq(ylim[1], ylim[2], ylim[2]/5)
  axis(2, pos=0.8, at=att2)
  legend(x=1.2, y=0.38, bty = "n", ncol=3, title = "", title.adj=0, 
         legend=c("y'=2008     ", "y'=2009     ", "y'=2010     ", "y'=2011     ", "y'=2012     "), 
         xjust=0, y.intersp=1.5,  lty=c(1,1,1,1,1), col=c(1,2,3,4,"orange"), cex = 0.75) 
  #+++++++++++++++++++++++++
  ylim = c(0, 1)
  par(mar=c(1.4, 1.5, 0, 0))
  for(i in 7:11) {  # i: anos de 2013 a 2017
    xc=NULL 
    for(k in (min(nprev,i)-1):0){
      cta = atcred==anos[i-k]
      xc = c(xc, CRED[cta,(k+1)])
    }
    if(i==7) plot(xc, type="l", ylim=ylim, axes=FALSE, xlab="", ylab="Credibilidade")
    else lines(xc, type="l", col=ifelse(i==11,"orange",i-6))
  }
  att2 = round(seq(ylim[1], ylim[2], ylim[2]/5),2)
  axis(2, pos=0.8, at=att2)
  legend(x=1.2, y=0.45, bty = "n", ncol=3, title = "", title.adj=0, 
         legend=c("y'=2013     ", "y'=2014     ", "y'=2015     ", "y'=2016     ", "y'=2017     "), 
         xjust=0, y.intersp=1.5,  lty=c(1,1,1,1,1), col=c(1,2,3,4,"orange"), cex = 0.75) 
  par(mgp= c(0.4, 0.3, 0), cex.axis=0.7)
  latt = paste0(c(paste0("(y'-",rep(2:1,each=4),")"), rep("y'",4)), paste0("Q",1:4))
  axis(1, at=1:12, labels=latt, las=1)  
  
  par(op)
  rm(op, latt, cex, i, prev, k, xc, cta, atcred, ylim, att2)
}



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






