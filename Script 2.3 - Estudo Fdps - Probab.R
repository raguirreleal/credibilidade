
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Script 2.3 - Credibilidade Inflação (Probabilidades)
# Ricardo A. Leal 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{
library(readxl); library(fda); library(KernSmooth); library(MASS); library(beepr)
library(openxlsx)  # precisa do rtolls instalado -- and add the R tools bin directory to the system path.
                   # se shell("zip") retorna ”zip is not recognised...”, então não está instalado
  # Sys.setenv("R_ZIPCMD" = "C:/Program Files/R/Rtools/bin/zip.exe")
library(doParallel); (ncores=detectCores())
library(foreach); library(iterators)
library(seasonal); library(seasonalview); library("forecast")# usar função seas()
#library(ggplot2)
}

cl <- makeCluster(ncores); registerDoParallel(cl)

# > DADOS E VARIAVEIS GLOBAIS =================================================================================

tipo.infla  = "PRCPCE"  # "PRCCPI" ou "PRCPCE"
ano.inicio  = 2007
meta.centro = 2
interv.meta = 0.5
nprev       = 2
  
# Dados +++++++++++++++++++

dados = read_excel("dados/SPFmicrodata.xlsx", sheet = tipo.infla, col_types = c(rep("numeric", 24)), na = "NA")
dados = dados[dados$YEAR >= ano.inicio, ]
dados$NAMEQ = paste0(dados$YEAR, "Q", dados$QUARTER)  # Cria nova coluna para nome trimestres (no fim tabela)
  ndados = length(dados$YEAR)
  
tri = unique(dados$NAMEQ)   # Vetor com os nomes dos trimestres
ntri = length(tri)  

anotri = substr(tri, 1, 4)   # Vetor com os nomes dos anos
nanos = length(unique(dados$YEAR))

IDs = sort(unique(dados$ID))
nIDs = length(IDs)
  
metas     = rep(meta.centro, ntri)
metas.int = as.data.frame(cbind((metas-interv.meta),(metas+interv.meta)))
  dimnames(metas.int) = list(tri, c("LimInf", "LimSup"))


# Grid +++++++++++++++++++++

ini.grid = - 2.5
fim.grid =   6.5
intg     =   0.5
rangeg   = c(ini.grid, fim.grid)

grid.int = as.data.frame(cbind( a=seq(ini.grid,(fim.grid-intg),intg) , b=seq((ini.grid+intg),fim.grid,intg) )) 
  grid.int = cbind(grid.int, c=(grid.int$a+grid.int$b)/2)
ajuste.prob = (1/intg)/100

nint     = length(grid.int$c)

# > FUNCOES DENSIDADES - NOS INTERVALOS PREVIOS ===============================================================


fit <- function(y, nomes) {
  breaks  = c(ini.grid, grid.int$b)
  ybase = create.bspline.basis(rangeval=rangeg, breaks=breaks, norder=1)  #  ordem 1 é a função cstte
  yparam = fdPar(ybase, lambda=1e-9)    # lambda->0, total fitting                       
  fAg = smooth.basis(grid.int$c, y, yparam, fdnames = nomes)$fd     # Observacao funcional
}

# função para verificar monotonicidade das probabilidades em torno da moda
monotona = function(x) { #recebe matriz com vetores de probabilidades
  qtd = 0
  M = round(t(as.matrix(dados[iAgk, cols[-c(1,10)]]))*ajuste.prob, 4)
  nc = dim(M)[2]  #qtd de previsões/agentes
  nl = 8          #opçoes/intervalos
  foreach(i = 1:nc) %do% {
    xvetor = as.double(M[,i])
    moda1 = which.max(xvetor)
    acima = xvetor[moda1:nl]
    abaixo = xvetor[1:moda1]
    qtd = qtd + !(identical(acima, sort(acima, decreasing = T)) & identical(abaixo, sort(abaixo)))
  }
  qtd
}

soma = conta = soma.max = soma.aj = naomonotona = 0
soma.min = Inf
conta.um = NULL

AgDENS = foreach(j = 1:nprev) %do% {
  foreach(k = 1:ntri) %do% {
    cols = c(rev(((j*10)-9):(j*10)+4))
    xxiAg=NULL
    iAgk = which(dados$NAMEQ==tri[k])
    wch.na = which(is.na(dados[iAgk,cols]), arr.ind = TRUE)
    iAgk = iAgk[ -unique(wch.na[,1]) ]
      if(length(iAgk)>soma.max) soma.max = length(iAgk); if(length(iAgk)<soma.min) soma.min = length(iAgk)
      soma = soma + length(iAgk); conta = conta + 1
    h1 = matrix( rep(0, 4*length(iAgk)),  nrow = 4)
      # vou incluir mais 4 intervalos antes e depois do grid
    yfitk = rbind(h1, round(t(as.matrix(dados[iAgk, cols]))*ajuste.prob, 4), h1)
    conta.um = c(conta.um, sum(foreach(g = 1:length(iAgk), .combine = c) %do% (sum(yfitk[,g]!=0))==1))
    naomonotona = naomonotona + monotona()
    # ajuste dos extremos ++++++++++++++++++++
    for(i in 1:length(iAgk)) {
      tt = 1
      prop = as.double(yfitk[5 ,i] / yfitk[6 ,i])
      prop2 = as.double(yfitk[14,i] / yfitk[13,i])

      if(!is.na(prop)){
        if(prop>3.15) { xxiAg=c(xxiAg,i); next() }
        if(prop > 1){
          soma.aj = soma.aj + 1; tt=0
          if(prop>2.05) aji=0.85 else if(prop>1.34) aji=0.75 else aji=0.65
          prop.f = prop
          for(u in 5:1) {
            ajf = (aji^(6-u))
            if(prop.f <= ajf) {
              yfitk[u,i] = yfitk[6,i] * prop.f
              break()
            }
            yfitk[u,i] = yfitk[(u+1),i] * aji
            prop.f = prop.f - ajf
          } 
        }
      }

      if(!is.na(prop2)){
        if(prop2>3.15) { xxiAg=c(xxiAg,i); next() }
        if(prop2 > 1){
          soma.aj = soma.aj + 1*tt
          if(prop2>2.05) aji=0.85 else if(prop2>1.34) aji=0.75 else aji=0.65
          prop.f = prop2
          for(u in 14:18) {
            ajf = (aji^(u-13))
            if(prop.f <= ajf) {
              yfitk[u,i] = yfitk[13,i] * prop.f
              break()
            }
            yfitk[u,i] = yfitk[(u-1),i] * aji
            prop.f = prop.f - ajf
          }
        }
      }
    }
    if(!is.null(xxiAg)) {
      yfitk = yfitk[ ,-xxiAg]
      iAgk = iAgk[-xxiAg]
      print(paste("Excluido fdp Ag: ",xxiAg))
    }
    # +++++++++++++++++++++++++++
    nomes = list(Inflacao="", ID=dados$ID[iAgk], funcao="Probabilidade")
    AgDENSjk = fit(yfitk, nomes)
} }
names(AgDENS) = paste0("prev",c(1:nprev))
rm(k, j, i, g, cols, iAgk, wch.na, AgDENSjk, nomes, yfitk, prop, prop2, prop.f, h1, u, ajf, aji, xxiAg, tt)
rm(soma, conta, soma.max, soma.aj, naomonotona, soma.min, conta.um, ajuste.prob)

    # > unlist(foreach(h = 1:3) %do% sum(0.65^(1:h)))
    # [1] 0.650000 1.072500 1.347125
    # > unlist(foreach(h = 1:4) %do% sum(0.75^(1:h)))
    # [1] 0.750000 1.312500 1.734375 2.050781
    # > unlist(foreach(h = 1:5) %do% sum(0.85^(1:h)))
    # [1] 0.850000 1.572500 2.186625 2.708631 3.152337



# > CALCULO CREDIBILIDADE ====================================================================================

cred = as.data.frame(matrix(NA, nIDs, ntri, dimnames=list(IDs, tri)) )
AgCRED = list()

for(j in 1:nprev) {
  for(k in 1:ntri) {
    xids = AgDENS[[j]][[k]]$fdnames$ID   # já exclui os que não passaram no teste prop<=3.15, no AgDENS
    inti = foreach(i = xids, .combine=c, .packages='fda') %do% {
      ifd = which(xids==i)
      int1 = integrate(function(x) eval.fd(evalarg=x, fdobj=AgDENS[[j]][[k]][ifd]), lower=metas.int$LimInf[k], 
                       upper=metas.int$LimSup[k])$value
      round(int1, 4)
    }
    cred[ which(IDs %in% xids) ,k] = inti
  }
  AgCRED[[j]]=cred
}
beep(1)
names(AgCRED) = paste0("prev",c(1:nprev))
rm(cred, inti, xids, j, k)



# > CALCULO ESPERANÇA ====================================================================================

esper = as.data.frame(matrix(NA, nIDs, ntri, dimnames=list(IDs, tri)) )
AgESPER = list()

for(j in 1:nprev) {
  for(k in 1:ntri) {
    xids = AgDENS[[j]][[k]]$fdnames$ID   # já exclui os que não passaram no teste prop<=3.15, no AgDENS
    inti = foreach(i = xids, .combine=c, .packages='fda') %do% {
      ifd = which(xids==i)
      int1 = integrate(function(x) eval.fd(evalarg=x, fdobj=AgDENS[[j]][[k]][ifd])*x, lower=-2.5, 
                       upper=0)$value
      int2 = integrate(function(x) eval.fd(evalarg=x, fdobj=AgDENS[[j]][[k]][ifd])*x, lower=0, 
                       upper=2)$value
      int3 = integrate(function(x) eval.fd(evalarg=x, fdobj=AgDENS[[j]][[k]][ifd])*x, lower=2, 
                       upper=4)$value
      int4 = integrate(function(x) eval.fd(evalarg=x, fdobj=AgDENS[[j]][[k]][ifd])*x, lower=4, 
                       upper=6.5)$value
      round(int1+int2+int3+int4, 4)
    }
    esper[ which(IDs %in% xids) ,k] = inti
  }
  AgESPER[[j]]=esper
}
beep(1)
names(AgESPER) = paste0("prev",c(1:nprev))
rm(esper, inti, xids, j, k)



#=========> REPRESENTATIVOS=====================================================================================
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # Densidade media +++++++++++++++++++++++++++++++++++

DENSm = foreach(j = 1:nprev) %do% {
  medias = foreach(k = 1:ntri) %do% {
    mean.fd(AgDENS[[j]][[k]])
  }
  names(medias) = tri
  medias
}
names(DENSm) = paste0("prev",c(1:nprev))
rm(medias)
# foreach(j=1:nprev,.combine=paste)%:%foreach(k=1:ntri)%do%plot(DENSm[[j]][[k]],ylim=c(0,1.4))

  # Densidade media suave +++++++++++++++++++++++++++++

breaks = c(ini.grid, grid.int$b)
# breaks = c(-2.5, seq(-2.5+1,  6.5-1,  by=intg), 6.5)  # tira um nó interno extremo cada lado (-2 e 6)
# mesmo que: breaks = seq(-2.5+intg,  6.5-intg,  by=intg)
nbreaks = length(breaks)
ybase2 = create.bspline.basis(rangeval=rangeg, breaks=breaks, norder=3)
yparam2 = fdPar(ybase2, 1, lambda=0)
n = ybase2$nbasis

s = seq(rangeg[1], rangeg[2], by=0.0001)
fB = smooth.basis(s,  eval.basis(s, ybase2), yparam2)$fd
dfB = (deriv.fd(fB))

fint <- function(z) {
  vtr = rep(0, length(z))
  for(i in 1:length(z)) {
    vdB = t(eval.fd(z[i], dfB)) # vetor coluna normal
    dB2 = vdB %*% t(vdB)
    vtr[i] = dB2[l,c]
  }
  as.double(vtr)
}

D = foreach(c = 1:n, .combine=cbind) %:% foreach(l = 1:n, .combine=c, .packages='fda') %dopar% {
  int = integrate( fint, lower=-2.5, upper=0  )$value +
    integrate( fint, lower= 0  , upper=2  )$value +
    integrate( fint, lower= 2  , upper=4  )$value +
    integrate( fint, lower= 4  , upper=6.5)$value
  1/int
}


ints = c(ini.grid, grid.int$b) 
xeval = seq((ini.grid+intg/2), (fim.grid-intg/2), by=intg)

B  = foreach(p = 1:nint, .combine=rbind) %:% foreach(q = 1:n, .combine=c) %do% eval.basis(xeval[p], ybase2)[,q]
Bi = foreach(p = 1:nint, .combine=rbind) %:% foreach(q = 1:n, .combine=c) %do%
  integrate(function(s) eval.basis(s, ybase2)[,q], lower=as.double(ints[p]), upper=as.double(ints[p+1]))$value

calcc = function(j, k, lambda) {
  c = cS%*%x   # c = round(cS%*%x, 3)
  c[c<0] = 0
  c[1:4] = c[17:20] = 0
  ff = fd(c, ybase2)
  intk = integrate( function(s) eval.fd(evalarg=s,fdobj=ff), lower=-2.5, upper=  0, subdivisions=1024 )$value +
         integrate( function(s) eval.fd(evalarg=s,fdobj=ff), lower=   0, upper=  2, subdivisions=1024 )$value +
         integrate( function(s) eval.fd(evalarg=s,fdobj=ff), lower=   2, upper=  4, subdivisions=1024 )$value +
         integrate( function(s) eval.fd(evalarg=s,fdobj=ff), lower=   4, upper=6.5, subdivisions=1024 )$value
  c = c/intk   # c = round(c/intk, 3)
}

tBi = t(Bi); tB = t(B)
lambdas = seq(0, 0.00110, by=0.000001)
vgcv = rep(NA, length(lambdas))
GCV = list()
GCV[[1]] = read.csv2("Tabela Script 2.0 - GCV[[1]].csv", header = TRUE)
GCV[[2]] = read.csv2("Tabela Script 2.0 - GCV[[2]].csv", header = TRUE)

# {
#   for(j in 1:nprev) {
#     Mgcv = foreach(k = 1:ntri, .combine=cbind, .packages=c('fda','MASS')) %dopar% {
#       vgcv = rep(NA, length(lambdas))
#       for(l in 1:length(lambdas)) {
#         x  = eval.fd(evalarg=xeval, fdobj=DENSm[[j]][[k]])
#         xx = c(0,x,0)
#         cS = (ginv( tBi%*%Bi + (lambdas[l]*D*as.double(xx>0)) )) %*% (intg*tBi)
#         # cS = (ginv( tBi%*%Bi + (lambdas[l]*D) )) %*% (intg*tBi)
#         c = calcc(j, k, lambdas[l])
#         SSPE = as.double(t((intg*x)-Bi%*%c)%*%((intg*x)-Bi%*%c))
#         traco = sum(diag( diag(nint) - Bi%*%cS ))
#         vgcv[l]=(SSPE/nint) / (traco/nint)^2
#       }
#       vgcv
#     }
#     GCV[[j]] = Mgcv
#   }
#   beep(1)
#   write.csv2(GCV[[1]], "Tabela Script 1.0 - GCV[[1]].csv", row.names=F)
#   write.csv2(GCV[[2]], "Tabela Script 1.0 - GCV[[2]].csv", row.names=F)
#   rm(j, k, l)
# }
        

salva.lambda = matrix(0, ntri, nprev)
sDENSm = foreach(j = 1:nprev) %do% {
  C = foreach(k = 1:ntri, .combine=cbind) %do% {
    transp = as.double(GCV[[j]][,k])
    posmin = which(transp==min(transp))
    lambda = lambdas[posmin]
    salva.lambda[k,j] = lambda
    # print(paste("k=",k,"trimestre:", tri[k], "  --  lambda:", lambda))
    x  = eval.fd(evalarg=xeval, fdobj=DENSm[[j]][[k]])
    # ------------
    xx = c(0,x,0)
    cS = (ginv( tBi%*%Bi + (lambda*D*as.double(xx>0)) )) %*% (intg*tBi)
    # cS = (ginv( tBi%*%Bi + (lambda*D) )) %*% (intg*tBi)
    c = calcc(j, k, lambda)
    if(j==1 & k==16) c[5] = 0
    c
  }
  fdens = fd(C, ybase2)
}
summary(salva.lambda)

SDAP = foreach(j = 1:nprev, .combine=cbind) %:% foreach(k = 1:ntri, .combine=c) %do% {
  z=0
  for(i in 1:nint) {
    intD=integrate(function(s) eval.fd(evalarg=s, fdobj=DENSm[[j]][[k]]),
                   lower=grid.int$a[i], upper=grid.int$b[i], subdivisions=1000)$value
    intsD=integrate(function(s) eval.fd(evalarg=s, fdobj=sDENSm[[j]][k]),
                    lower=grid.int$a[i], upper=grid.int$b[i], subdivisions=1000)$value
    z = z + abs(intsD-intD)
  }
  z   # SDAP: soma das diferenças absolutas de probabilidade
}
dimnames(SDAP) = list(tri, paste0("prev",c(1:nprev)))
# write.xlsx(SDAP, "Tabela Script 2.0 - SDAP.xlsx", rowNames=T, colNames=T)
# SDAP
colSums(SDAP); colMeans(SDAP); colMeans(SDAP)/18

rm(s, n, p, q, ints, xeval, x, c, cS, lambda, lambdas, yparam2, fB, dfB, breaks, nbreaks)
rm(tBi, tB, z, posmin, C, B, Bi, D, i, j, k, Mgcv, intD, intsD, vgcv, xx)

# j=1; k=40
# foreach(k = 1:ntri, .combine=cbind) %do% {
#   plot(sDENSm[[j]][k], main=paste(j, k, tri[k]))
#   lines(DENSm[[j]][[k]])
# }
# rm(j, k)



  # Esperança das densidades medias ==========================================================================


ESPER = foreach(j = 1:nprev, .combine=cbind) %do% {
  foreach(k = 1:ntri, .combine=c, .packages='fda') %do% { 
    int1=integrate(function(s) eval.fd(evalarg=s, fdobj=DENSm[[j]][[k]])*s, lower=-2.5, upper=0, 
                   subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(evalarg=s, fdobj=DENSm[[j]][[k]])*s, lower=0, upper=2, 
                   subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(evalarg=s, fdobj=DENSm[[j]][[k]])*s, lower=2, upper=4, 
                   subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(evalarg=s, fdobj=DENSm[[j]][[k]])*s, lower=4, upper=6.5, 
                   subdivisions=1024)$value
    int1+int2+int3+int4
}}
dimnames(ESPER) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); ESPER=as.data.frame(ESPER)
rm( j)


sESPER = foreach(j = 1:nprev, .combine=cbind) %do% {
  foreach(k = 1:ntri, .combine=c, .packages='fda') %do% { 
    int1=integrate(function(s) eval.fd(evalarg=s, fdobj=sDENSm[[j]][k])*s, lower=-2.5, upper=0, 
                   subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(evalarg=s, fdobj=sDENSm[[j]][k])*s, lower=0, upper=2, 
                   subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(evalarg=s, fdobj=sDENSm[[j]][k])*s, lower=2, upper=4, 
                   subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(evalarg=s, fdobj=sDENSm[[j]][k])*s, lower=4, upper=6.5, 
                   subdivisions=1024)$value
    int1+int2+int3+int4
}}
dimnames(sESPER) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); sESPER=as.data.frame(sESPER)
rm( j)

colMeans(ESPER - sESPER)
max(abs(ESPER - sESPER))
difESPER = as.double(rbind( (ESPER - sESPER)[,1],(ESPER - sESPER)[,2] ))
mean(difESPER)

t = mean(difESPER) / (sd(difESPER)/sqrt(length(difESPER))) 
pt( -abs(t), length(difESPER)-1 )*2  # probabilidade distribuição t (H0: media=0)


plot(sESPER[,2], type="l", col=2)
lines(ESPER[,2], type="l")

#~~~~~~~~~~

tsj = ts(sESPER[, 1], start = c(2007, 1), freq = 4)
plot(tsj)
monthplot(tsj)
tsj.seas = seas(
  x = tsj,
  transform.function = "none",
  regression.aictest = NULL,
  outlier = NULL,
  regression.variables = c("tl2007.01-2009.1", "ao2009.1"),
  arima.model = "(1 0 0)(0 0 0)"
)
# pp.test(window(tsj, start = c(2009, 1)))
# kpss.test(window(tsj, start = c(2009, 1)), "Level")
summary(tsj.seas )
# view(tsj.seas)
plot(tsj.seas$series$s12) 
monthplot(resid(tsj.seas))
# sem sazonalidade pelo SEATS


tsj2 = ts(sESPER[, 2], start = c(2007, 1), freq = 4)
plot(tsj2)
tsj.seas2 = seas(
  x = tsj2,
  transform.function = "log",
  regression.aictest = NULL,
  outlier = NULL,
  regression.variables = c("ao2009.1", "tl2007.01-2009.1"),
  arima.model = "(1 0 0)(0 1 1)"
)
# pp.test(window(tsj2, start = c(2009, 1)))
# kpss.test(window(tsj2, start = c(2009, 1)), "Level")
# view(tsj.seas2)
summary(tsj.seas2)
Pacf(resid(tsj.seas2),36)
Acf(resid(tsj.seas2),36)
plot(resid(tsj.seas2)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas2$series$s10); abline(h=1, lty=2, xpd=F)      
plot(tsj.seas2$series$s12) 
monthplot(tsj.seas2$series$s10); abline(h=1, lty=2, xpd=F)

plot(tsj)
lines(tsj2, col="red")

plot(tsj.seas$series$s12) 
lines(tsj.seas2$series$s12, col="red") 


 # Variância e Desvio padrao das densidades medias ===============================================================


VAR = foreach(j = 1:nprev, .combine=cbind) %do% {
  foreach(k = 1:ntri, .combine=c, .packages='fda') %do% {
    int1=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^2 ), 
                   lower=-2.5, upper=0, subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^2 ), 
                   lower=0, upper=2, subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^2 ), 
                   lower=2, upper=4, subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^2 ), 
                   lower=4, upper=6.5, subdivisions=1024)$value
    (int1+int2+int3+int4)
}}
dimnames(VAR) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); VAR=as.data.frame(VAR)
rm( j)


sVAR = foreach(j = 1:nprev, .combine=cbind) %do% {
  foreach(k = 1:ntri, .combine=c, .packages='fda') %do% {
    int1=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^2 ), 
                   lower=-2.5, upper=0, subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^2 ), 
                   lower=0, upper=2, subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^2 ), 
                   lower=2, upper=4, subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^2 ), 
                   lower=4, upper=6.5, subdivisions=1024)$value
    (int1+int2+int3+int4)
}}
dimnames(sVAR) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); sVAR=as.data.frame(sVAR)
rm( j)

colMeans(sVAR - VAR)
max(abs(sVAR - VAR))
colMeans((sVAR - VAR)/sVAR)
max((abs(sVAR - VAR))/sVAR)
summary(lm( as.double(rbind(VAR[,1],VAR[,2])) ~ as.double(rbind(sVAR[,1],sVAR[,2])) - 1 ))

difVAR = as.double(rbind( (sVAR-VAR)[,1],(sVAR-VAR)[,2] ))
mean(difVAR)
(t = mean(difVAR) / (sd(difVAR)/sqrt(length(difVAR))) )
pt( -abs(t), length(difVAR)-1 )*2  # probabilidade distribuição t (H0: media=0)

plot(sVAR[,1], type="l", col=2, ylim=c(0.1, 0.6))
lines(VAR[,1], type="l")


# Desvio-padrão:
DP = sqrt(VAR)
sDP = sqrt(sVAR)
colMeans(sDP - DP)
max(sDP - DP)

plot(sDP[,1], type="l", col=2, ylim=c(0.3, 0.8))
lines(DP[,1], type="l")

#+++++++++++++++++++++++++++++++++++++

tsj = ts(sDP[, 1], start = c(2007, 1), freq = 4)
plot(tsj)
monthplot(tsj)
tsj.seas = seas(x = tsj, transform.function = "log",
  regression.variables = c("ao2008.3", "tl2008.03-2012.1"),  arima.model = "(0 0 0)(0 1 1)" )
summary(tsj.seas )
    # view(tsj.seas)
    # gglagplot(tsj)
    # identify(tsj.seas)
    Pacf(resid(tsj.seas),36)
    Acf(resid(tsj.seas),36)
plot(resid(tsj.seas)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas$series$s10); abline(h=1, lty=2, xpd=F)      
plot(tsj.seas$series$s12) 
monthplot(tsj.seas$series$s10); abline(h=1, lty=2, xpd=F)

tsj2 = ts(sDP[, 2], start = c(2007, 1), freq = 4)
tsj.seas2 = seas(
  x = tsj2,
  transform.function = "log",
  regression.aictest = NULL,
  outlier = NULL,
  regression.variables = c("ao2008.3", "tl2008.03-2012.1"),
  arima.model = "(1 0 0)(0 1 1)"
)
# view(tsj.seas2)
summary(tsj.seas2)
Pacf(resid(tsj.seas2),36)
Acf(resid(tsj.seas2),36)
plot(resid(tsj.seas2)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas2$series$s10); abline(h=1, lty=2, xpd=F)      
plot(tsj.seas2$series$s12) 
monthplot(tsj.seas2$series$s10); abline(h=1, lty=2, xpd=F)




# Coef. Assimetria ===============================================================================================


ASSM = foreach(j = 1:nprev, .combine=cbind) %do% {
  Mom3 = foreach(k = 1:ntri, .combine=c, .packages='fda') %dopar% {  # 3º MOMENTO
    int1=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^3 ), 
                   lower=-2.5, upper=0, subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^3 ), 
                   lower=0, upper=2, subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^3 ), 
                   lower=2, upper=4, subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^3 ), 
                   lower=4, upper=6.5, subdivisions=1024)$value
    (int1+int2+int3+int4)
  }
  Mom3 / sqrt(VAR[,j])^3  # 3º momento / (dp)^3  = coef assimetria
}
dimnames(ASSM) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); ASSM=as.data.frame(ASSM)
sASSM = foreach(j = 1:nprev, .combine=cbind) %do% {
  Mom3 = foreach(k = 1:ntri, .combine=c, .packages='fda') %dopar% {  # 4º momento
    int1=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^3 ), 
                   lower=-2.5, upper=0, subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^3 ), 
                   lower=0, upper=2, subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^3 ), 
                   lower=2, upper=4, subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^3 ), 
                   lower=4, upper=6.5, subdivisions=1024)$value
    (int1+int2+int3+int4)
  }
  Mom3 / sqrt(sVAR[,j])^3
}
dimnames(sASSM) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); sASSM=as.data.frame(sASSM)
rm(j, Mom3)

plot(sASSM[,1], type="l", col=2, ylim=c(-0.7, 1.7))
lines(ASSM[,1], type="l")



#~~~~~~~~

tsj = ts(sASSM[, 1], start = c(2007, 1), freq = 4)
tsj.seas = seas(
  x = tsj,
  transform.function = "none",
  regression.aictest = NULL,
  outlier = NULL,
  regression.variables = c("ao2008.4", "tl2008.04-2011.4"),
  arima.model = "(0 0 0)(0 1 1)"
)
# view(tsj.seas)
plot(tsj.seas$series$s10); abline(h=0, lty=2, xpd=F) 
monthplot(tsj.seas$series$s10); abline(h=0, lty=2, xpd=F) 

#~~~~~~~~

tsj2 = ts(sASSM[, 2], start = c(2007, 1), freq = 4)
tsj.seas2 = seas(
  x = tsj2
)
# view(tsj.seas2)
#sem sazonalidade
monthplot(tsj2); abline(h=0, lty=2, xpd=F) 

# Coef. Curtose ==================================================================================================


CURT = foreach(j = 1:nprev, .combine=cbind) %do% {
  Mom4 = foreach(k = 1:ntri, .combine=c, .packages='fda') %dopar% {  # 4º MOMENTO
    int1=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^4 ), 
                   lower=-2.5, upper=0, subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^4 ), 
                   lower=0, upper=2, subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^4 ), 
                   lower=2, upper=4, subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(s, DENSm[[j]][[k]]) * ( (s-ESPER[k,j])^4 ), 
                   lower=4, upper=6.5, subdivisions=1024)$value
    (int1+int2+int3+int4)
  }
  Mom4 / VAR[,j]^2  # 4º momento / (var)^2  = coef CURTOSE
}
dimnames(CURT) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); CURT=as.data.frame(CURT)
sCURT = foreach(j = 1:nprev, .combine=cbind) %do% {
  Mom4 = foreach(k = 1:ntri, .combine=c, .packages='fda') %dopar% {  # 4º momento
    int1=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^4 ), 
                   lower=-2.5, upper=0, subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^4 ), 
                   lower=0, upper=2, subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^4 ), 
                   lower=2, upper=4, subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(s, sDENSm[[j]][k]) * ( (s-sESPER[k,j])^4 ), 
                   lower=4, upper=6.5, subdivisions=1024)$value
    (int1+int2+int3+int4)
  }
  Mom4 / sVAR[,j]^2
}
dimnames(sCURT) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); sCURT=as.data.frame(sCURT)
rm(j, Mom4)

plot(sCURT[,2], type="l", col=2, ylim=c(2, 7))
lines(CURT[,2], type="l")

tsj = ts(sCURT[, 1], start = c(2007, 1), freq = 4)
tsj.seas = seas(
  x = tsj,
  transform.function = "log",
  regression.aictest = NULL,
  outlier = NULL,
  regression.variables = "ao2010.4",
  arima.model = "(0 1 1)(0 1 1)"
)
# view(tsj.seas)
plot(tsj.seas$series$s10); abline(h=1, lty=2, xpd=F) 
monthplot(tsj.seas$series$s10) ; abline(h=1, lty=2, xpd=F) 


#+++++++++

tsj2 = ts(sCURT[, 2], start = c(2007, 1), freq = 4)
tsj.seas2 = seas(
  x = tsj2
)
# view(tsj.seas2)
# sem sazonalidade



 # Credibilidade por densidades medias ========================================================================


CREDm = foreach(j = 1:nprev, .combine=cbind) %do% {
  foreach(k = 1:ntri, .combine=c, .packages='fda') %dopar% {
    int1 = integrate(function(x) eval.fd(evalarg=x, fdobj=DENSm[[j]][[k]]), subdivisions=1024,
                     lower=metas.int$LimInf[k], upper=metas.int$LimSup[k])$value
}}
sCREDm = foreach(j = 1:nprev, .combine=cbind) %do% {
  foreach(k = 1:ntri, .combine=c, .packages='fda') %dopar% {
    int1 = integrate(function(x) eval.fd(evalarg=x, fdobj=sDENSm[[j]][k]), subdivisions=1024,
                     lower=metas.int$LimInf[k], upper=metas.int$LimSup[k])$value
}}
dimnames(sCREDm) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); sCREDm=as.data.frame(sCREDm)
dimnames(CREDm) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); CREDm=as.data.frame(CREDm)
rm( j)


plot(sCREDm[,1], type="l", col=2, ylim=c(0, 1))
lines(CREDm[,1], type="l")


tsj = ts(sCREDm[, 1], start = c(2007, 1), freq = 4)
tsj.seas = seas(x=tsj)
# view(tsj.seas)
plot(tsj.seas$series$d10)      # ou $s10
monthplot(tsj.seas$series$d10)
monthplot(tsj.seas$series$d10/sd(tsj), main="em qtd DP", ylim = c(-2, 2))

#++++

tsj = ts(sCREDm[, 2], start = c(2007, 1), freq = 4)
tsj.seas = seas(x=tsj)
# view(tsj.seas)
plot(tsj.seas$series$s10)      # ou $d10
monthplot(tsj.seas$series$s10)
monthplot(tsj.seas$series$s10/sd(tsj), main="em qtd DP", ylim = c(-2, 2))






#========= COMPARACAO MEDIAS C/ MEDIAS =======================================================================
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


if(tipo.infla=="PRCPCE") tipo.infla2="COREPCE" else tipo.infla2="CORECPI"

database = read_excel("dados/SPFmicrodata.xlsx", sheet = tipo.infla2, col_types = c(rep("numeric", 13)), na = "NA")
database = database[database$YEAR >= ano.inicio, ]
database$NAMEQ = paste0(database$YEAR, "Q", database$QUARTER)  # Cria nova coluna para nome trimestres (no fim tabela)


AgCOMPARA = cbind(NAMEQ=database$NAMEQ, ID=database$ID, database[ ,11:12],
                matrix(NA, nrow=dim(database)[1], ncol=nprev, dimnames=list(NULL,paste0("prev",c(1:nprev)))))

temp = foreach(j = 1:nprev) %do% {
  foreach(i = 1:dim(AgCOMPARA)[1]) %do% {
    xx = AgESPER[[j]] [ dimnames(AgESPER[[j]])[[1]]==AgCOMPARA$ID[i],
                        dimnames(AgESPER[[j]])[[2]]==AgCOMPARA$NAMEQ[i] ]
    if(!is.null(xx)) AgCOMPARA[i, (4+j)] = xx
}}
rm(temp, j, i, xx)

AgCOMPARA2 = na.exclude(AgCOMPARA)

# ~~~~~~~~~~~~~ Em conjunto:  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

amostraN = data.frame(prev=AgCOMPARA$prev1, COREPCE=AgCOMPARA$COREPCEA, ID=AgCOMPARA$ID, NAMEQ=AgCOMPARA$NAMEQ)
amostra  = data.frame(prev=AgCOMPARA$prev2, COREPCE=AgCOMPARA$COREPCEB, ID=AgCOMPARA$ID, NAMEQ=AgCOMPARA$NAMEQ)
amostraN = rbind(na.exclude(amostraN), na.exclude(amostra))

amostra = as.matrix(amostraN[,1:2])
  (ma2 = mean(amostra))

diferencas = as.double(amostra[,1] - amostra[,2])
difs.relat = diferencas*100/ma2
# difs.relat = diferencas*100/as.double(amostra[,2])

mean(diferencas); mean(difs.relat)  # em média, esperança maior que previsão pontual
median(diferencas); median(difs.relat)
range(diferencas); range(difs.relat)
sd(diferencas); sd(difs.relat)
round(quantile((diferencas), probs = sort(c(seq(0,1,0.1),0.95))),2)
round(quantile(abs(diferencas), probs = sort(c(seq(0,1,0.1),0.95))),2)
round(quantile(abs(difs.relat), probs = sort(c(seq(0,1,0.1),0.95))),2)
round(quantile((difs.relat), probs = sort(c(seq(0,1,0.1),0.95))),2)
mean(amostra[,1]); mean(amostra[,1]*100/ma2)
mean(amostra[,2]); mean(amostra[,2]*100/ma2)
summary(lm(amostra[,1] ~ amostra[,2] - 1))
100*sd(lm(amostra[,1] ~ amostra[,2] - 1)$residuals)/ma2



ids = sort(unique(amostraN$ID))
conID = matrix(NA, nrow = length(ids), ncol = 6)
colnames(conID) = c("ID", "QTD", "MEDIA", "SD", "MEDIA-abs", "SD-abs")
conID[,1] = ids
for(i in 1:length(ids)) {
  ami1 = amostraN[amostraN$ID==ids[i],1]
  ami2 = amostraN[amostraN$ID==ids[i],2]
  conID[i,2]    = length   ( (ami1-ami2)*100/ma2 )
  conID[i,3]    = mean     ( (ami1-ami2)*100/ma2 )
  conID[i,4]    = sd       ( (ami1-ami2)*100/ma2 )
  conID[i,5]    = mean( abs( (ami1-ami2)*100/ma2 ))
  conID[i,6]    = sd  ( abs( (ami1-ami2)*100/ma2 ))
}

round(quantile(conID[,3], probs = sort(c(seq(0,1,0.1),0.187,0.95))), 2)     # média
round(quantile(conID[,4], probs = sort(c(seq(0,1,0.1),0.187,0.95))), 2)     # dp
round(quantile(conID[,5], probs = sort(c(seq(0,1,0.1),0.87,0.98,0.53))), 2) # média absol
round(quantile(conID[,5], probs =  sort(c(seq(0,1,0.1),0.95))), 2)          # média absol
round(quantile(conID[,6], probs =  sort(c(seq(0,1,0.1),0.95))), 2)          # dp absol
sum(conID[,3]<0) / length(conID[,1]) 
summary(lm(conID[,3] ~ (conID[,4]) - 1))
summary(lm(conID[,5] ~ (conID[,6]) - 1))


hist(difs.relat, 120, freq = F, ylim=c(0,0.06), xlim=c(-80,80))
bwk = dpik(difs.relat, scalest = "stdev", level = 2, kernel = "normal")
pbkde = bkde(difs.relat, kernel = "normal", bandwidth = bwk)  # "epanech"
lines(pbkde, type = "l", col=2)

fy = smooth.basis(pbkde$x, pbkde$y, 
                  fdPar(create.bspline.basis(rangeval=range(pbkde$x), nbasis=300, norder=4), 0, 0))$fd 

dem = function(s) eval.fd(s, fy)*s
(mean.int=integrate(dem, lower=-60, upper=60, subdivisions=1024)$value)

de  = function(s) eval.fd(s, fy)
des = function(s) (eval.fd(s, fy)*(s - mean.int)^2)
var.int=integrate(des, lower=-60, upper=60, subdivisions=1024)$value
sqrt(var.int) # desvio padrão
(prob=integrate(de, lower=-sqrt(var.int), upper=sqrt(var.int), subdivisions=1024)$value)
(prob=integrate(de, lower=-sqrt(var.int)*2, upper=sqrt(var.int)*2, subdivisions=1024)$value)



#====== Testes de hipóstese
(t = mean(difs.relat) / (sd(difs.relat)/sqrt(length(difs.relat))) )
pt( -abs(t), length(difs.relat)-1 )*2  # probabilidade distribuição t (H0: media=0)
#~~~~~~~~
tID = conID[,c(2,3,4)]
tID = cbind(tID, t = tID[,2] / (sd(difs.relat)/sqrt(length(difs.relat))) )
tID = cbind(tID, pt=pt(-abs(tID[,4]), length(difs.relat)-1)*2 )
# View(tID)

sum(tID[,5]<0.10); sum(tID[,5]<0.10)/length(tID[,5])
sum(tID[,5]<0.05); sum(tID[,5]<0.05)/length(tID[,5])
sum(tID[,5]<0.01); sum(tID[,5]<0.01)/length(tID[,5])

#====== Regressão séries agentes

excID = conID[(conID[,2]<6), 1]
length(excID) # qtd excluidos
amostraN2 = amostraN[!(amostraN[,3]%in%excID),]
ids2 = sort(unique(amostraN2$ID))

coefs.reg.id = foreach(i = 1:length(ids2), .combine=rbind) %do% {
  ami1 = amostraN2[amostraN2$ID==ids2[i],1]
  ami2 = amostraN2[amostraN2$ID==ids2[i],2]
  xxx = lm(ami1 ~ ami2 - 1)
  # plot(xxx$residuals/ami2, type = "l", ylim = c(-0.6,0.6))
  # lines(xxx$residuals, col=2)
  cbind( summary(xxx)$coefficients, 
         R2aj       = summary(xxx)$adj.r.squared,
         dif.medias = ((mean(ami1)/mean(ami2))-1)*100,
         sd.resid.p = 100*sd(xxx$residuals)/ma2,
         mean.resid = 100*mean(xxx$residuals)/ma2
         )
}
coefs.reg.id = as.data.frame(cbind(coefs.reg.id, coef.perc=(coefs.reg.id[,1]-1)*100))
round(quantile(coefs.reg.id$dif.medias , probs = sort(c(seq(0,1,0.1),0.95))), 2)  
round(quantile(coefs.reg.id$Estimate   , probs = sort(c(seq(0,1,0.1),0.95))), 2)  
round(quantile(coefs.reg.id$`Pr(>|t|)` , probs = c(0.9,0.947,0.9904,1)), 8) 
round(quantile(coefs.reg.id$sd.resid.p , probs = sort(c(seq(0,1,0.1),0.95))), 2)  
summary( lm( abs(coefs.reg.id$coef.perc) ~ coefs.reg.id$sd.resid.p - 1) )


# Por trimestres ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

conTRI1 = matrix(NA, nrow = ntri, ncol = 7)
conTRI1 = as.data.frame(conTRI1)
colnames(conTRI1) = c("TRI", "QTD", "MEDIA", "SD", "MEDIA-abs", "SD-abs", "Q")
conTRI1[,1] = tri
conTRI1[,7] = substr(tri,5,6)
coefs.reg.tri1 = foreach(i = 1:ntri, .combine=rbind) %do% {
  amt1 = AgCOMPARA2$prev1[AgCOMPARA2$NAMEQ==tri[i]]
  amt2 = AgCOMPARA2$COREPCEA[AgCOMPARA2$NAMEQ==tri[i]]
  conTRI1[i,2]    = length   ( (amt1-amt2)*100/ma2 )
  conTRI1[i,3]    = mean     ( (amt1-amt2)*100/ma2 )
  conTRI1[i,4]    = sd       ( (amt1-amt2)*100/ma2 )
  conTRI1[i,5]    = mean( abs( (amt1-amt2)*100/ma2 ))
  conTRI1[i,6]    = sd  ( abs( (amt1-amt2)*100/ma2 ))
  
  xxx = lm(amt1 ~ amt2 - 1)
  # plot(xxx$residuals/ami2, type = "l", ylim = c(-0.6,0.6)); lines(xxx$residuals, col=2)
  cbind( summary(xxx)$coefficients, 
         R2aj       = summary(xxx)$adj.r.squared,
         dif.medias = ((mean(amt1)/mean(amt2))-1)*100,
         sd.resid.p = 100*sd(xxx$residuals)/ma2,
         mean.resid = 100*mean(xxx$residuals)/ma2
  )
}

round(quantile(conTRI1[,3], probs = sort(c(seq(0,1,0.1),0.187,0.95))), 2)     # média
round(quantile(conTRI1[,4], probs = sort(c(seq(0,1,0.1),0.187,0.95))), 2)     # dp
round(quantile(conTRI1[,5], probs =  sort(c(seq(0,1,0.1),0.95))), 2)          # média absol
round(quantile(conTRI1[,6], probs =  sort(c(seq(0,1,0.1),0.95))), 2)          # dp absol
sum(conTRI1[,3]<0) / ntri
summary(lm(conTRI1[,3] ~ (conTRI1[,4]) - 1))
summary(lm(conTRI1[,5] ~ (conTRI1[,6]) - 1))

coefs.reg.tri1 = as.data.frame(cbind(coefs.reg.tri1, coef.perc=(coefs.reg.tri1[,1]-1)*100))
round(quantile(coefs.reg.tri1$dif.medias , probs = sort(c(seq(0,1,0.1),0.95))), 2)  
round(quantile(coefs.reg.tri1$Estimate   , probs = sort(c(seq(0,1,0.1),0.95))), 2)  
round(quantile(coefs.reg.tri1$`Pr(>|t|)` , probs = c(0.9,0.947,0.9904,1)), 8) 
round(quantile(coefs.reg.tri1$sd.resid.p , probs = sort(c(seq(0,1,0.1),0.95))), 2)  
summary( lm( abs(coefs.reg.tri1$coef.perc) ~ coefs.reg.tri1$sd.resid.p - 1) )


#~~~~~~~~

conTRI2 = matrix(NA, nrow = ntri, ncol = 7)
conTRI2 = as.data.frame(conTRI2)
colnames(conTRI2) = c("TRI", "QTD", "MEDIA", "SD", "MEDIA-abs", "SD-abs", "Q")
conTRI2[,1] = tri
conTRI2[,7] = substr(tri,5,6)
coefs.reg.tri2 = foreach(i = 1:ntri, .combine=rbind) %do% {
  amt1 = AgCOMPARA2$prev2[AgCOMPARA2$NAMEQ==tri[i]]
  amt2 = AgCOMPARA2$COREPCEB[AgCOMPARA2$NAMEQ==tri[i]]
  conTRI2[i,2]    = length   ( (amt1-amt2)*100/ma2 )
  conTRI2[i,3]    = mean     ( (amt1-amt2)*100/ma2 )
  conTRI2[i,4]    = sd       ( (amt1-amt2)*100/ma2 )
  conTRI2[i,5]    = mean( abs( (amt1-amt2)*100/ma2 ))
  conTRI2[i,6]    = sd  ( abs( (amt1-amt2)*100/ma2 ))
  
  xxx = lm(amt1 ~ amt2 - 1)
  # plot(xxx$residuals/ami2, type = "l", ylim = c(-0.6,0.6)); lines(xxx$residuals, col=2)
  cbind( summary(xxx)$coefficients, 
         R2aj       = summary(xxx)$adj.r.squared,
         dif.medias = ((mean(amt1)/mean(amt2))-1)*100,
         sd.resid.p = 100*sd(xxx$residuals)/ma2,
         mean.resid = 100*mean(xxx$residuals)/ma2
  )
}
round(quantile(conTRI2[,3], probs = sort(c(seq(0,1,0.1),0.187,0.95))), 2)     # média
round(quantile(conTRI2[,4], probs = sort(c(seq(0,1,0.1),0.187,0.95))), 2)     # dp
round(quantile(conTRI2[,5], probs =  sort(c(seq(0,1,0.1),0.95))), 2)          # média absol
round(quantile(conTRI2[,6], probs =  sort(c(seq(0,1,0.1),0.95))), 2)          # dp absol
sum(conTRI2[,3]<0) / ntri
summary(lm(conTRI2[,3] ~ (conTRI2[,4]) - 1))
summary(lm(conTRI2[,5] ~ (conTRI2[,6]) - 1))

coefs.reg.tri2 = as.data.frame(cbind(coefs.reg.tri2, coef.perc=(coefs.reg.tri2[,1]-1)*100))
round(quantile(coefs.reg.tri2$dif.medias , probs = sort(c(seq(0,1,0.1),0.95))), 2)  
round(quantile(coefs.reg.tri2$Estimate   , probs = sort(c(seq(0,1,0.1),0.95))), 2)  
round(quantile(coefs.reg.tri2$`Pr(>|t|)` , probs = c(0.9,0.947,0.9904,1)), 8) 
round(quantile(coefs.reg.tri2$sd.resid.p , probs = sort(c(seq(0,1,0.1),0.95))), 2)  
summary( lm( abs(coefs.reg.tri2$coef.perc) ~ coefs.reg.tri2$sd.resid.p - 1) )











