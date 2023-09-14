
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Script 3.0 - Credibilidade CORE-PCE
# Ricardo A. Leal 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(readxl); library(fda); library(KernSmooth)
library(doParallel); (ncores=detectCores())
library(foreach); library(iterators)
library(seasonal); library(seasonalview); library("forecast")# usar função seas()
#library(ggplot2)


# > DADOS E VARIAVEIS GLOBAIS ====================================================================================

tipo.infla  = c("PCE5YR", "PCE10")  # PCE
# tipo.infla  = c("CPI5YR", "CPI10")  # CPI
ano.inicio  = 2007

#++++++++++++++

database = read_excel("dados/SPFmicrodata.xlsx", sheet=tipo.infla[1], col_types=c(rep("numeric",5)), na="NA")
database = database[database$YEAR >= ano.inicio, ]

database2 = read_excel("dados/SPFmicrodata.xlsx", sheet=tipo.infla[2], col_types=c(rep("numeric",5)), na="NA")
database2 = database[database$YEAR >= ano.inicio, ]


anos = unique(database$YEAR)   # Vetor com os nomes dos anos
nanos = length(anos)

dados = cbind(NAMEQ=paste0(database$YEAR, "Q", database$QUARTER), database[5], database2[,5])
rm(database, database2)

tri = unique(dados$NAMEQ)   # Vetor com os nomes dos trimestres
ntri = length(tri)  

nprev = 2
ndados = length(dados$NAMEQ) 


# > ESTIMACAO DAS DENSIDADES =====================================================================================

# Definition of variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

n.dens   = 1024*4  # gridsize
ini.dens = -2
end.dens =  6.5

DENS510y2 = DENS510y = fDENS510y = fDENS510y2 = vector(mode = "list", length = nprev)
names(DENS510y) = names(fDENS510y) = paste0("prev",c(1:nprev))

# Estimation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for(k in 1:nprev) {
  #..............
  MTRI = foreach(i = 1:ntri, .combine=cbind) %do% {
    prev = as.double(na.exclude(dados[dados$NAMEQ == tri[i],(k+1)]))
    deobj <- bkde( prev, kernel= "epanech", gridsize=n.dens, range.x=c(ini.dens, end.dens) )
    deobj$y
  }
  MTRI2 = foreach(i = 1:ntri, .combine=cbind) %do% {
    prev = as.double(na.exclude(dados[dados$NAMEQ == tri[i],(k+1)]))
    # bwk = bw.SJ(prev, nb=10000, method="dpi")
    # deobj <- density( prev, bw=bwk, kernel="gaussian", n=n.dens, from=ini.dens, to=end.dens )
    bwk = dpik(prev, scalest = "stdev", level = 2, kernel = "epanech", gridsize = 401*10)
    deobj <- bkde( prev, kernel= "epanech", bandwidth = bwk, gridsize=n.dens, range.x=c(ini.dens, end.dens) )
    deobj$y
  }
  # retorna MTRI, uma matriz (n.dens X ntri)
  #..............
  dimnames(MTRI) = dimnames(MTRI2) = list(deobj$x, tri)
  DENS510y[[k]] = MTRI
  DENS510y2[[k]] = MTRI2
}
x.dens = deobj$x
rm(MTRI, deobj, prev, i, k, MTRI2)
# plot(DENS510y[[1]][,1], type = "l")

# DENS510y[[k]] é a matriz (n.dens X ntri) com as densidades das previsoes nos ntri trimestres para k peridos
# a frente, onde cada coluna DENS510y[[k]][ ,i] contém o vetor com as densidades estimadas do trimestre i



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
  fDENS510y[[k]]  = fit(x.dens, DENS510y[[k]],  ordem, nknots, lambda)
  fDENS510y2[[k]] = fit(x.dens, DENS510y2[[k]], ordem, nknots, lambda)
}
rm(k, ordem, nknots, lambda)


for(k in 1:nprev) {
  for(i in 1:ntri) {
    prev = as.double(na.exclude(dados[dados$NAMEQ == tri[i],(k+1)]))
    plot( fDENS510y2[[k]][i], col="red", main = paste("prev:",k, " - ", tri[i]," - ", i))
    lines(fDENS510y[[k]][i])
    rug(prev, ticksize = 0.03, side = 1, lwd = 2)
  }
}
rm(i, k, prev)




# > CALCULO CREDIBILIDADE ========================================================================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

meta = 2
tol.meta = 0.5
interv = c(meta-tol.meta, meta+tol.meta)

func.CRED510y = function(tol.meta) {
  interv = c(meta-tol.meta, meta+tol.meta)
  CRED = foreach(k = 1:nprev, .combine=cbind) %:% foreach(i = 1:ntri, .combine=c) %do% {
    integrate(function(x) eval.fd(x, fdobj=fDENS510y[[k]][i]), lower=interv[1], upper=interv[2])$value
  }
  dimnames(CRED) = list(tri, names(fDENS510y))
  as.data.frame(CRED)
}
CRED510y = func.CRED510y(tol.meta)

for(i in 1:nprev) plot(CRED510y[,i], type="l", ylim = c(0.3,1)); rm(i)















# > GRAFICOS  ====================================================================================================






  
  
  








