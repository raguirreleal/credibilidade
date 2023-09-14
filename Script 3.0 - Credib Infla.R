
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Script 3.0 - Credibilidade CORE-PCE
# Ricardo A. Leal 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(readxl); library(fda); library(KernSmooth); library(beepr)
library(doParallel); (ncores=detectCores())
library(foreach); library(iterators)
library(seasonal); library(seasonalview); library("forecast")
#library(ggplot2)


# > DADOS E VARIAVEIS GLOBAIS ====================================================================================

tipo.infla  = "COREPCE"  # "COREPCE" ou "CORECPI"
peri        = "ano"  #Trimestral ("tri") ou Anual ("ano")
ano.inicio  = 2007


#++++++++++++++

database = read_excel("dados/SPFmicrodata.xlsx", sheet = tipo.infla, col_types = c(rep("numeric", 13)), na = "NA")
database = database[database$YEAR >= ano.inicio, ]
database$NAMEQ = paste0(database$YEAR, "Q", database$QUARTER)  # Cria nova coluna para nome trimestres (no fim tabela)

tri = unique(database$NAMEQ)   # Vetor com os nomes dos trimestres
ntri = length(tri)  

anos = unique(database$YEAR)   # Vetor com os nomes dos anos
nanos = length(anos)
  
dados.tri = database[,-c(5,11:13)]
dados.ano = database[,-c(5:10)]

nprev.tri = 5
nprev.ano = 3
ndados = length(database$YEAR) 


if(peri=="tri") {dados = dados.tri; nprev = nprev.tri} else {dados = dados.ano; nprev = nprev.ano}


# > ESTIMACAO DAS DENSIDADES =====================================================================================


n.dens   = 1024*4  # gridsize
ini.dens = -2
end.dens =  6

DENS2 = DENS = fDENS = fDENS2 = vector(mode = "list", length = nprev)
  names(DENS) = names(fDENS) = paste0("prev",c(1:nprev))
  
  # Estimation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for(k in 1:nprev) {
  #..............
  MTRI = foreach(i = 1:ntri, .combine=cbind) %do% {
    prev = (na.exclude(dados[dados$NAMEQ == tri[i],(k+4)]))[[1]]
    deobj <- bkde( prev, kernel= "epanech", gridsize=n.dens, range.x=c(ini.dens, end.dens) )
    deobj$y
  }
  MTRI2 = foreach(i = 1:ntri, .combine=cbind) %do% {
    prev = (na.exclude(dados[dados$NAMEQ == tri[i],(k+4)]))[[1]]
    # bwk = bw.SJ(prev, nb=10000, method="dpi")
    # deobj <- density( prev, bw=bwk, kernel="gaussian", n=n.dens, from=ini.dens, to=end.dens )
    bwk = dpik(prev, scalest = "stdev", level = 2, kernel = "epanech", gridsize = 401*10)
    deobj <- bkde( prev, kernel= "epanech", bandwidth = bwk, gridsize=n.dens, range.x=c(ini.dens, end.dens) )
    deobj$y
  }
  # retorna MTRI, uma matriz (n.dens X ntri)
  #..............
  dimnames(MTRI) = dimnames(MTRI2) = list(deobj$x, tri)
  DENS[[k]] = MTRI
  DENS2[[k]] = MTRI2
}
x.dens = deobj$x
rm(MTRI, deobj, prev, i, k, MTRI2, bwk, n.dens)
# plot(DENS[[1]][,1], type = "l")

  # DENS[[k]] é a matriz (n.dens X ntri) com as densidades das previsoes nos ntri trimestres para k peridos 
  # a frente, onde cada coluna DENS[[k]][ ,i] contém o vetor com as densidades estimadas do trimestre i


# Funcoes densidade ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit <- function(x, y, ordem, nknots, lambda) {
  ybase = create.bspline.basis(rangeval=range(x), nbasis=(nknots+ordem), norder=ordem)
  yparam = fdPar(ybase, 2, lambda)           # lambda=0, total fitting
  fy = smooth.basis(x, y, yparam)$fd         # Observacoes funcionais de cada trimestre
}

ordem = 4         # ordem do spline [ = grau + 1 ]
nknots = 200      # qtd de nos, ou breaks
lambda = 1e-9

for(k in 1:nprev) {
  fDENS[[k]]  = fit(x.dens, DENS[[k]],  ordem, nknots, lambda)
  fDENS2[[k]] = fit(x.dens, DENS2[[k]], ordem, nknots, lambda)
}
rm(k, ordem, nknots, lambda)
# plot(fDENS[[1]][1])

{
  # conta=0
  # # op=par(ask=T)
  # for(k in 1:nprev) {
  #   for(i in 1:ntri) {
  #     prev = na.exclude(dados[dados$NAMEQ == tri[i],(k+4)])[[1]]
  #     print(length(prev)); conta=conta+length(prev)
  #     plot( fDENS2[[k]][i], col="red", main = paste("prev:",k, " - ", tri[i]," - ", i))
  #     lines(fDENS[[k]][i])
  #     rug(prev, ticksize = 0.03, side = 1, lwd = 2)
  #   }
  # }
  # # par(op)
  # rm(i, k, prev, conta)
}


# > CALCULO CREDIBILIDADE ========================================================================================

meta = 2
tol.meta = 0.5
interv = c(meta-tol.meta, meta+tol.meta)

func.CRED = function(tol.meta) {
  interv = c(meta-tol.meta, meta+tol.meta)
  CRED = foreach(k = 1:nprev, .combine=cbind) %:% foreach(i = 1:ntri, .combine=c) %do% {
      integrate(function(x) eval.fd(x, fdobj=fDENS[[k]][i]), lower=interv[1], upper=interv[2])$value
  }
  dimnames(CRED) = list(tri, names(DENS))
  as.data.frame(CRED)
}

if(peri=="ano") CRED = func.CRED(tol.meta) else CREDtri = func.CRED(tol.meta)



# > GRAFICOS  ====================================================================================================


prev = 1
ttm = 0.5
ylim = c(0,2)
xlim = c(-1,5)
{  
  cex = 0.75
  txtmain = paste0(" --  Credib p/ ", ifelse(peri=="tri", "trimestre ", "ano "), " de h=", prev-1)
  op = par(mar=c(2, 1.4,2, 0), cex.axis=0.6, cex.lab=cex, mgp= c(0.6, 0.7, 0), mfcol=c(1,1), family = "serif")
  s = x.dens[x.dens>=meta-ttm & x.dens<=meta+ttm ]
  sy = which(x.dens>=meta-ttm & x.dens<=meta+ttm)
  wdens = which(x.dens>=xlim[1] & x.dens<=xlim[2])
  for(i in 1:ntri) {
    plot(x.dens[wdens], DENS[[prev]][wdens,i], type="l", ylab="Densidade de Probabilidade", 
         xlab="", ylim=ylim, axes=FALSE, main=paste("Trimestre:", tri[i], txtmain ))
    ypoly = as.double(DENS[[prev]][sy, i])
    polygon( c(s, rev(s)), c( ypoly, rep(0, length(ypoly)) ), col=gray(0.7), border = NA)
    lines(x.dens[wdens], DENS[[prev]][wdens,i], type="l")
    
    att = seq(xlim[1], xlim[2], 0.5)
    axis(1, at=att, labels=format(att, digits=1), las=3)
    att = seq(ylim[1], ylim[2], 0.5)
    axis(2, line=-1, at=att)
    clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-2.08)
    abline(v=meta, lty=3)
    abline(v=c(meta-ttm, v=meta+ttm), col=2)
  }
  par(op)
  rm(op, att, cex, i, ypoly, txtmain, s, sy, wdens)
}

{  
  cex = 0.75
  txtmain = ifelse(peri=="tri", " --  Credib p/ trim atual", " --  Credib p/ ano atual")
  op = par(mar=c(2, 1.4, 0, 0), cex.axis=0.6, cex.lab=cex, mgp= c(0.6, 0.7, 0), mfcol=c(1,1), family = "serif")
  s = seq(xlim[1], xlim[2], 0.001)
  spoly = seq(meta-ttm, meta+ttm, 0.001)
  for(i in 1:ntri) {
    y = eval.fd(s, fDENS[[prev]][i])
    plot(s, y, type="l", ylab="Densidade de Probabilidade", 
         xlab="", ylim=ylim, axes=FALSE, main=paste("Trimestre:", tri[i], txtmain ))
    ypoly = eval.fd(spoly, fDENS[[prev]][i])
    polygon( c(spoly, rev(spoly)), c( ypoly, rep(0, length(ypoly)) ), col=gray(0.7), border = NA)
    lines(s, y, type="l")
    
    att = seq(xlim[1], xlim[2], 0.5)
    axis(1, at=att, labels=format(att, digits=1), las=3)
    att = seq(ylim[1], ylim[2], 0.5)
    axis(2, line=-1, at=att)
    clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]-2.08)
    abline(v=2, lty=3)
  }
  par(op)
  rm(op, att, cex, i, ypoly, txtmain, s, y)
  rm(prev, ttm, ylim, xlim)
}



{
  for(i in 1:nprev) {
    txtmain = ifelse(peri=="tri", paste((i-1), "trim a frente"), paste((i-1), "ano(s) a frente"))
    # plot(fit(1:ntri, CRED[,i], ordem=4, nknots=ntri, lambda=1e-9), 
    #      main=paste("Range:", intervalo, "   --   Credib", i, "trim a frente"), ylim = c(0,1), col=i, axes=FALSE)
    plot(CRED[,i], type = "l", 
         # main=paste("Credib para", tipo.infla, txtmain, "  (Intervalo:", interv[1], interv[2], ")"),
         ylim = c(0,1), col=i, axes=FALSE, xlab = "Trimestre", ylab = "Credibilidade")
    axis(2)
    axis(1, at=(1:ntri), labels=as.character(tri), las=1)
    box()
    par(new = T)
  }
  rm(i, txtmain); par(new = F)
}


#++++++++++++++++++++++

{
  for(i in nprev:ntri) {
    if(peri!="tri") stop("Plot para Credibilidade de Trimestres!")
    
    xc=NULL  
    for(k in 4:0){
      xc = c(xc, CRED[(i-k),(k+1)])    
    }
    plot(xc, type = "b", main=paste("Range:", tol.meta, " -  Credibilidade para", tri[i]), ylim = c(0,1), 
         axes=FALSE, xlab = "Trimestre(s) a frente", ylab = "Credibilidade")
    axis(2)
    axis(1, at=(1:nprev), labels=as.character(tri[(i-4):i]), las=1)
    box()
  }
  rm(i, k, xc)
}

#++++++++++++++++++++++

{
  atcred = substr(tri,1,4)  # vetor com ano de cada trimestre tri
  for(i in 1:nanos) {
    if(peri=="tri") stop("Plot para Credibilidade de Anos!")
    
    xc=labels=NULL 
    for(k in (min(nprev,i)-1):0){
      cta = atcred==anos[i-k]
      xc = c(xc, CRED[cta,(k+1)])
      labels = c(labels, tri[cta])
    }
    plot((xc), type = "b", main=paste("Range:", tol.meta, " -  Credibilidade para ano", anos[i]), ylim = c(0,1), 
         axes=FALSE, xlab = "Trimestre", ylab = "Credibilidade")
    axis(2); axis(1, at=1:length(labels), labels=(labels), las=1); box()
    
    
  }
  rm(i, k, xc, cta, atcred, labels)
}




# > CREDIBILIDADE E INTERVALOS  ==================================================================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ttm = 0.5
# t  = 0.85^(1:8)
t  = 0.5^(1:3)
ylim = c(0, 1)

# ttm = 0.01
# t  = 0.5^(1:3)
# ylim = c(0, 0.06)

tt = sort(c(ttm, ttm*(1-t), ttm*(1+t)), T)
imedtt = which(tt==median(tt))
cinza = (c(seq(from=0.4, by=0.4/(length(tt)-imedtt-1), length.out=(length(tt)-imedtt-1)),  0.8 ))
ttCRED = foreach(i = 1:length(tt), .combine = cbind) %do% func.CRED(tt[i])$prev1

y = ts(ttCRED, start = c(2007,1), frequency = 4)
{
  cex = 0.75
  op = par(mar=c(2, 1.2, 0, 0), cex.axis=0.6, cex.lab=cex, mgp= c(0.5, 0.7, 0), mfcol=c(1,1), family = "serif")
  plot(y[,imedtt], type="l", ylab="Credibilidade", xlab="", ylim = ylim, axes=FALSE)
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
  att2 = seq(ylim[1], ylim[2], ylim[2]/5)
  axis(2, line=-1, at=att2)
  clip(par("usr")[1], par("usr")[2]-0.4, par("usr")[3], par("usr")[4])
  abline(h=1, lty=3)
  par(op)
  rm(op, att, latt, cex, i)
}
rm(t, tt, y, imedtt, cinza)






  # Credibilidade calculada como a densidade ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

meta.centro = 2
interv.meta = 0.5
metas     = rep(meta.centro, ntri)
metas.int = as.data.frame(cbind((metas-interv.meta),(metas+interv.meta)))
dimnames(metas.int) = list(tri, c("LimInf", "LimSup"))

CREDd = CRED
for(k in 1:nprev) {
  credk = foreach(i = 1:ntri, .combine=c) %do% {
    eval.fd(evalarg=metas[i], fdobj=fDENS[[k]][i])
  }
  CREDd[ ,k] = credk
}
rm(credk, k ,i)

plot(CREDd$prev3, type = "b", main=paste("Densidade", "-", "prev3"))
plot(fit(1:ntri, CREDd$prev3, ordem=4, nknots=ntri, lambda=1e-9), main=paste("Densidade", " -", "prev3"))


# > FIM ---------












  
  
  








