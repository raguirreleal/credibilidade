


# PARA SCRIPT 2.0 ========================================================================================

{
  k = 43  #40
  cex = 0.75   # exportar PDF, 6.25 x 3.0 inch
  eixox = c(-1, 5)
  eixoy = c(0, 1.6)
  
  op = par(mar = c(1.7,2.3,0.5,0), cex.axis=0.6, cex.lab=cex, mgp= c(1.4, 0.7, 0), mfcol=c(1,2), family = "serif")
  
  plot(AgDENS[[1]][[43]][2],  ylab="Densidade de Probabilidade", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000, lty=1)
  par(new = TRUE)
  plot(AgDENS[[1]][[43]][22], col="blue", ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000, lty=2)
  par(new = TRUE)
  plot(AgDENS[[1]][[43]][12], col=2, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000, lty=4)
  
  att = seq(from=eixox[1], to=eixox[2], by = 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  att2 = seq(from=eixoy[1], to=eixoy[2], by = 0.2)
  axis(2, line=0, at=att2)
  
  legend(x=3, y=1.43, bty = "n", ncol=1, title = "Agentes:",
         legend=c(paste0("i'=",1830), paste0("i'=",1843), paste0("i'=",1854)), xjust=0, y.intersp=2,
         lty=c(1,4,2), col=c(1,2,"blue"), cex = 0.75)
  
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.06)
  abline(v=0, lty=2, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.6)
  abline(v=2, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  
  #+++++++++++++++++++++ 
  
  par(mar = c(1.7,0.5,0.5,0))
  
  plot(DENSm[[1]][[k]],  ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  par(new = TRUE)

  n = ybase2$nbasis
  xeval = seq((ini.grid+intg/2), (fim.grid-intg/2), by=intg)
  B  = foreach(p = 1:nint, .combine=rbind) %:% foreach(q = 1:n, .combine=c) %do% eval.basis(xeval[p], ybase2)[,q]
  x  = eval.fd(evalarg=xeval, fdobj=DENSm[[1]][[k]])
  c  = ginv(t(B)%*%B) %*% t(B) %*% x
  fdens = fd(c, ybase2)
  
  plot(fdens, col=2, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1)
  
  att = seq(from=eixox[1], to=eixox[2], by = 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  
  text(1.75, 1.4, paste0("Agente Médio"), cex = cex)
  text(1.75, 1.3, paste0("t' = ", tri[43]), cex = cex)
  text(1.75, 1.2, paste0("y'+h = 2017"), cex = cex)
  
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.06)
  abline(v=0, lty=2, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.6)
  abline(v=2, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  
  par(op)
  rm(op, cex, eixox, eixoy, att, att2, fdens, n)
}

{
  cex = 0.75   # exportar PDF, 6.25 x 3.0 inch
  eixox = c(-1, 5)
  eixoy = c(0, 1.6)
  
  op = par(mar = c(1.7,2.3,0.5,0), cex.axis=0.6, cex.lab=cex, mgp= c(1.4, 0.7, 0), mfcol=c(1,2), family = "serif")
  
  plot(AgDENS[[1]][[43]][2],  ylab="Densidade de Probabilidade", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000, lty=1)
  par(new = TRUE)
  plot(AgDENS[[1]][[43]][22], col=3, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000, lty=2)
  par(new = TRUE)
  plot(AgDENS[[1]][[43]][12], col=2, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000, lty=4)
  
  att = seq(from=eixox[1], to=eixox[2], by = 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  att2 = seq(from=eixoy[1], to=eixoy[2], by = 0.2)
  axis(2, line=0, at=att2)
  
  legend(x=3, y=1.43, bty = "n", ncol=1, title = "Agentes:",
         legend=c(paste0("i'=",1830), paste0("i'=",1843), paste0("i'=",1854)), xjust=0, y.intersp=1.6,
         lty=c(1,4,2), col=c(1,2,3), cex = 0.75)
  
  # text(0.7, 1.5, paste0("i=1"), cex = cex)
  # text(2.25, 0.97, paste0("i=2"), cex = cex)
  # text(-0.25, 0.16, paste0("i=3"), cex = cex)
  
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.06)
  abline(v=0, lty=2, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.6)
  abline(v=2, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  
  #+++++++++++++++++++++ 
  
  par(mar = c(1.7,0.5,0.5,0))
  
  plot(DENSm[[1]][[43]],  ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  par(new = TRUE)
  fexemp143 = smooth.basis(grid.int$c, eval.fd(evalarg=grid.int$c, fdobj=DENSm[[1]][[43]]), yparam2)$fd 
  #plot(sDENSm[[1]][43], col=2, ylab="", xlab="",
  plot(fexemp143, col=2, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  
  att = seq(from=eixox[1], to=eixox[2], by = 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  
  text(1.75, 1.4, paste0("Agente Médio"), cex = cex)
  text(1.75, 1.3, paste0("t' = ", tri[43]), cex = cex)
  text(1.75, 1.2, paste0("y'+h = 2017"), cex = cex)
  
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.06)
  abline(v=0, lty=2, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.6)
  abline(v=2, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  
  par(op)
  rm(op, cex, eixox, eixoy, att, att2, fexemp143)
}

{
  cex = 0.75
  eixox = c(-1, 5)
  eixoy = c(0, 1.4)
  
  op = par(mar = c(1.7,2.3,0.5,0), cex.axis=0.6, cex.lab=cex, mgp= c(1.4, 0.7, 0), mfcol=c(1,2), family = "serif")
  
  plot(DENSm[[2]][[11]],  ylab="Densidade de Probabilidade", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  par(new = TRUE)
  plot(sDENSm[[2]][11], col=2, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  par(new = TRUE)
  plot(DENSm[[1]][[5]],  ylab="Densidade de Probabilidade", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000, lty=2)
  par(new = TRUE)
  plot(sDENSm[[1]][5], col=4, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  
  att = seq(from=eixox[1], to=eixox[2], by = 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  att2 = seq(from=eixoy[1], to=eixoy[2], by = 0.2)
  axis(2, line=0, at=att2)
  
  text(0, 0.6, paste0("t' = ", tri[9]), cex = cex)
  text(0, 0.5, paste0("y'+h = 2010"), cex = cex)
  
  text(3.5, 0.9, paste0("t' = ", tri[5]), cex = cex)
  text(3.5, 0.8, paste0("y'+h = 2008"), cex = cex)
  
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.06)
  abline(v=0, lty=2, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.4)
  abline(v=2, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  
  #+++++++++++++++++++++ 
  
  par(mar = c(1.7,0.5,0.5,0))
  
  plot(DENSm[[1]][[43]],  ylab="", xlab="",       #DENSm[[1]][[28]]
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  s = seq(1, 1.5, 0.0001)
  polygon( c(s,rev(s)),  #cord.x
           c(eval.fd(evalarg=s, fdobj=DENSm[[1]][[43]]), rev(eval.fd(evalarg=s, fdobj=sDENSm[[1]][43]))), #cord.y
           col=gray(0.7))
  par(new = TRUE)
  plot(sDENSm[[1]][43], col=2, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  
  att = seq(from=eixox[1], to=eixox[2], by = 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  
  text(1.75, 1.3, paste0("t' = ", tri[43]), cex = cex)
  text(1.75, 1.2, paste0("y'+h = 2017"), cex = cex)
  
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.06)
  abline(v=0, lty=2, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.4)
  abline(v=2, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  
  par(op)
  rm(op, cex, eixox, eixoy, att, att2, s)
}


{
  cex = 1
  eixox = c(-0.00001, 3.5)           # exportar PDF, 6.25 x 3.0 inch
  eixoy = c(0, 1.6)
  
  op = par(mar = c(1.9,2.9,0.5,0), cex.axis=0.85, cex.lab=1.1, mgp= c(1.7, 0.7, 0), mfcol=c(1,3), family = "serif")
  
  x  = eval.fd(evalarg=xeval, fdobj=DENSm[[1]][[40]])
  c  = ginv(tB%*%B) %*% tB %*% x
  fdens = fd(c, ybase2)
  
  plot(DENSm[[1]][[40]],  ylab="Densidade de Probabilidade", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  s = seq(0.5, 1, 0.0001)
  ss = seq(1.5, 2, 0.0001)
  polygon( c(s,rev(s)),  #cord.x
           c(eval.fd(evalarg=s, fdobj=DENSm[[1]][[40]]), rev(eval.fd(evalarg=s, fdens))), #cord.y
           col=gray(0.7))
  polygon( c(ss,rev(ss)),  #cord.x
           c(eval.fd(evalarg=ss, fdobj=DENSm[[1]][[40]]), rev(eval.fd(evalarg=ss, fdens))), #cord.y
           col=gray(0.7))
  par(new = TRUE)
  plot(fdens, col=2, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)

  att = seq(from=0, to=eixox[2], by = 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  att2 = seq(from=eixoy[1], to=eixoy[2], by = 0.2)
  axis(2, line=0, at=att2)
  
  text(1.75, 1.5, paste0("min SSE(c)"), cex = cex)
  text(1.75, 1.4, paste0("SAPE = 0,294"), cex = cex)

  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.6)
  abline(v=2, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  abline(h=0, lty=3, xpd=F)
  
  #+++++++++++++++++++++ 
  
  par(mar = c(1.9,0.5,0.5,0))
  
  x  = eval.fd(evalarg=xeval, fdobj=DENSm[[1]][[40]])
  c  = ginv(tBi%*%Bi) %*% tBi %*% (0.5*x)
  fdens = fd(c, ybase2)
  
  plot(DENSm[[1]][[40]],  ylab="", xlab="",       #DENSm[[1]][[28]]
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  polygon( c(s,rev(s)),  #cord.x
           c(eval.fd(evalarg=s, fdobj=DENSm[[1]][[40]]), rev(eval.fd(evalarg=s, fdens))), #cord.y
           col=gray(0.7))
  polygon( c(ss,rev(ss)),  #cord.x
           c(eval.fd(evalarg=ss, fdobj=DENSm[[1]][[40]]), rev(eval.fd(evalarg=ss, fdens))), #cord.y
           col=gray(0.7))
  par(new = TRUE)
  plot(fdens, col=2, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  
  att = seq(from=0, to=eixox[2], by = 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  
  text(1.75, 1.5, paste0("min SSPE(c)"), cex = cex)
  text(1.75, 1.4, paste0("SAPE = 0"), cex = cex)
  
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.6)
  abline(v=2, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.06)
  abline(v=0, lty=2, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  abline(h=0, lty=3, xpd=F)
  
  #+++++++++++++++++++++ 
  
  par(mar = c(1.9,0.5,0.5,0))
  
  plot(DENSm[[1]][[40]],  ylab="", xlab="",       #DENSm[[1]][[28]]
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  polygon( c(s,rev(s)),  #cord.x
           c(eval.fd(evalarg=s, fdobj=DENSm[[1]][[40]]), rev(eval.fd(evalarg=s, sDENSm[[1]][40]))), #cord.y
           col=gray(0.7))
  polygon( c(ss,rev(ss)),  #cord.x
           c(eval.fd(evalarg=ss, fdobj=DENSm[[1]][[40]]), rev(eval.fd(evalarg=ss, sDENSm[[1]][40]))), #cord.y
           col=gray(0.7))
  par(new = TRUE)
  plot(sDENSm[[1]][40], col=2, ylab="", xlab="",
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=15000)
  
  att = seq(from=0, to=eixox[2], by = 0.5)
  axis(1, at=att, labels=format(att, digits=1), las=3)
  
  text(1.75, 1.5, expression(min(SSPE(c) + lambda*R(c))), cex = cex)
  text(1.75, 1.4, paste0("SAPE = 0,105"), cex = cex)
  
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.6)
  abline(v=2, lty=3, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.06)
  abline(v=0, lty=2, xpd=F)
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
  abline(h=0, lty=3, xpd=F)
  
  par(op)
  rm(op, cex, eixox, eixoy, att, att2, s)
}

{
  cex = 0.75
  eixox = c(-2.5, 0.5)            # exportar PDF, 6.25 x 3.0 inch
  eixoy = c(0, 1)

  op = par(mar = c(2,1.5,0.5,0), cex.axis=0.6, cex.lab=cex, mgp= c(0, 0.7, 0), mfcol=c(1,2), family = "serif")

  plot(ybase2,  ylab="", xlab="", ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1)
  
  att = seq(from=eixox[1], to=eixox[2], by = 0.25)
  axis(1, at=att, labels=format(att, digits=3), las=3)
  att2 = seq(from=eixoy[1], to=eixoy[2], by = 0.2)
  axis(2, line=0, at=att2)
  
  #+++++++++++++++++++++ 
  
  eixox = c(3.5, 6.5)
  
  par(mar = c(2,0.5,0.5,0))
  
  plot(ybase2,  ylab="", xlab="", 
       ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1)
  
  att = seq(from=eixox[1], to=eixox[2], by = 0.25)
  axis(1, at=att, labels=format(att, digits=3), las=3)
  
  par(op)
  rm(op, cex, eixox, eixoy, att, att2)
}

{
  {
    cont=0   
    cex = 0.9                       # exportar PDF, 8.25 x 9.0 inch
    eixox = c(-1, 4.5)
    eixoy = c(0, 1.4)
    anos = matrix(c(anotri, as.double(anotri)+1), byrow = F, ncol = 2)
    contc = matrix(c(rep(1:nprev, each=ntri), rep(1:ntri, 2)), byrow = F, ncol = 2)     
  }
  for(vv in 1:2) {
    op = par(mar = c(1.7,2.4,0.5,0), cex.axis=0.6, cex.lab=1.1, mgp= c(1.6, 0.7, 0), mfrow=c(4,3), family = "serif")
    for(u in (1:11)) {
      if(cont==nprev*ntri) {par(op) ; break()}
      cont=cont+1
      
      par(mar = c(1.7,0.5,0.5,0))
      if(u==1|u==4|u==7|u==10|u==13|u==16) {par(mar = c(1.7,3,0.5,0)); xleg=2.9} else xleg=3.1
      if(cont!=1 & cont!=ntri+1) par(new = F)
      
      plot(sDENSm[[contc[cont,1]]][contc[cont,2]], col=2, xlab="", ylab="",
           ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=1500)
      
      if(u==1|u==4|u==7|u==10|u==13|u==16) {
        title(ylab="Densidade")
        att2 = seq(from=eixoy[1], to=eixoy[2], by = 0.2)
        axis(2, line=0, at=att2)
      }
      att = seq(from=eixox[1], to=eixox[2], by = 0.5)
      axis(1, at=att, labels=format(att, digits=1), las=3)
      
      clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-0.06)
      abline(v=0, lty=2, xpd=F)
      clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-1.4)
      abline(v=2, lty=3, xpd=F)
      clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4])
      
      legenda = NULL
      for(g in 1:min(4, nprev*ntri-cont+1, (ntri*contc[cont,1])-cont+1)) {
        legenda = c(legenda, paste0("t'=", substr(tri[contc[cont+(g-1),2]],3,6) ))
        leg=anos[ contc[cont+(g-1),2] , contc[cont+(g-1),1] ]
      }
      
      legend(x=xleg, y=1.4, bty = "n", ncol=1, y.intersp=0.7, title = paste0("y'+h = ",leg), pt.lwd=1,
             lty=c(1,1,1,1), col=c(2,3,4,1), cex=cex, lwd=1, yjust=1, xjust=0, legend=legenda)
      
      #+++++++++++++++++++++ 
      if(cont==nprev*ntri) {par(op) ; break()}
      if(cont==ntri) next()
      cont=cont+1
      
      par(new = TRUE)
      plot(sDENSm[[contc[cont,1]]][contc[cont,2]], col=3, xlab="", ylab="",
           ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=1500)
      
      #+++++++++++++++++++++ 
      if(cont==nprev*ntri) {par(op) ; break()}
      if(cont==ntri) next()
      cont=cont+1
      
      par(new = TRUE)
      plot(sDENSm[[contc[cont,1]]][contc[cont,2]], col=4, xlab="", ylab="", 
           ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=1500)
      
      #+++++++++++++++++++++ 
      if(cont==nprev*ntri) {par(op) ; break()}
      if(cont==ntri) next()
      cont=cont+1
      
      par(new = TRUE)
      plot(sDENSm[[contc[cont,1]]][contc[cont,2]], col=1, xlab="", ylab="", 
           ylim = eixoy, xlim = eixox, axes=FALSE, lwd=1, href=F, nx=1500)
      
    }
  }
  rm(op, cex, eixox, eixoy, att, att2, g, u, vv, leg, legenda, xleg)
}


{
  cex = 0.75
  eixox = c(1, ntri)            # exportar PDF, 6.25 x 3.0 inch
  eixoy = c(1, 2.4)
  att = time(tsj)[cycle(tsj)==1 | cycle(tsj)==3]
  latt = substr(tri[cycle(tsj)==1 | cycle(tsj)==3], 3,6)
 
  op = par(mar=c(2, 1.4, 0, 0), cex.axis=0.6, cex.lab=cex, mgp= c(0.6, 0.7, 0), mfcol=c(1,1), family = "serif")
  
  plot(tsj.seas$series$s12,  ylab="Esperança da expectativa de inflação", 
       xlab="", ylim = eixoy, axes=FALSE, lwd=1) 
  lines(tsj.seas2$series$s12, col="red") 
  lines(tsj.seas1.pontual$series$s12, lty=2)
  lines(tsj.seas2.pontual$series$s12, col="red", lty=2)
  lines(tsj.seas3.pontual$series$s12, col="blue", lty=2)
  lines(tsj.seas.5y$series$s12, col="green", lty=1)
  lines(tsj.seas.10y$series$s12, col="green", lty=4)
  
  axis(1, at=att, labels=latt, las=3)
  att2 = seq(from=eixoy[1], to=eixoy[2], by = 0.2)
  axis(2, line=-1, at=att2)

  legend(x=2007.25, y=1.6, bty = "n", ncol=1, title = "   Séries:", title.adj=0,
         legend=c("h = 0", "h = 1", "h = 0 (pontual)", "h = 1 (pontual)", "h = 2 (pontual)", "h5", "h10"), 
         xjust=0, y.intersp=1.4,  lty=c(1,1,2,2,2,1,4), pt.lwd=1, col=c(1,2,1,2,4,3,3), cex = 0.75)  
  
  abline(h=2, lty=3, xpd=F)

  par(op)
  rm(op, cex, eixoy, att, att2, latt)
}






