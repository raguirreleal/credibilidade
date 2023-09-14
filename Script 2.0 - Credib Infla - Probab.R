
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Script 2.0 - Credibilidade Inflação (Probabilidades)
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
# Fdps uniformes por intervalos

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

soma = mediaAgs = soma.max = soma.aj = naomonotona = 0
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
      mediaAgs = mediaAgs + length(iAgk)
      soma = soma + length(iAgk)
    h1 = matrix( rep(0, 4*length(iAgk)),  nrow = 4)
      # vou incluir mais 4 intervalos antes e depois do grid
    yfitk = rbind(h1, round(t(as.matrix(dados[iAgk, cols]))*ajuste.prob, 4), h1)
      # se toda a probabilidade em apenas um intervalo:
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
    nomes = list(Inflacao="", ID=dados$ID[iAgk], funcao="Probabilidade")
    AgDENSjk = fit(yfitk, nomes)
} }
(mediaAgs = mediaAgs/ntri/2)
sum(conta.um)
names(AgDENS) = paste0("prev",c(1:nprev))
rm(k, j, i, g, cols, iAgk, wch.na, AgDENSjk, nomes, yfitk, prop, prop2, prop.f, h1, u, ajf, aji, xxiAg, tt)
rm(soma, soma.max, soma.aj, naomonotona, soma.min, conta.um, ajuste.prob)

    # > unlist(foreach(h = 1:3) %do% sum(0.65^(1:h)))
    # [1] 0.650000 1.072500 1.347125
    # > unlist(foreach(h = 1:4) %do% sum(0.75^(1:h)))
    # [1] 0.750000 1.312500 1.734375 2.050781
    # > unlist(foreach(h = 1:5) %do% sum(0.85^(1:h)))
    # [1] 0.850000 1.572500 2.186625 2.708631 3.152337



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
beep(1)


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

#rm(s, n, p, q, ints, xeval, x, c, cS, lambda, lambdas, fB, dfB, breaks, nbreaks)
#rm(tBi, z, posmin, C, B, Bi, D, i, j, k, intD, intsD, xx)

# j=1; k=40
# foreach(k = 1:ntri, .combine=cbind) %do% {
#   plot(sDENSm[[j]][k], main=paste(j, k, tri[k]))
#   lines(DENSm[[j]][[k]])
# }
# rm(j, k)








# > CALCULO CREDIBILIDADE ====================================================================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


cred = as.data.frame(matrix(NA, nIDs, ntri, dimnames=list(IDs, tri)) )
AgCRED = list()

# cl <- makeCluster(ncores); registerDoParallel(cl)
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
# stopCluster(cl); rm(cl); 
beep(1)
names(AgCRED) = paste0("prev",c(1:nprev))
rm(cred, inti, xids, j, k)




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
beep(1)
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

#++++

tsj = ts(sCREDm[, 2], start = c(2007, 1), freq = 4)
tsj.seas = seas(x=tsj)
# view(tsj.seas)
plot(tsj.seas$series$s10)      # ou $d10
monthplot(tsj.seas$series$s10)





stopCluster(cl); rm(cl)





