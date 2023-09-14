
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
meta.centro = 2
  intervalo = 0.5

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

metas     = rep(meta.centro, ntri)
metas.int = as.data.frame(cbind((metas-intervalo),(metas+intervalo)))
  dimnames(metas.int) = list(tri, c("LimInf", "LimSup"))


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



# Esperança das densidades -----------------------------------------------------------------------

cl <- makeCluster(ncores); registerDoParallel(cl)
ESPER510 = foreach(k = 1:nprev, .combine=cbind) %do% {
  foreach(i = 1:ntri, .combine=c, .packages='fda') %dopar% {
    int1=integrate(function(s) eval.fd(s, fDENS510y[[k]][i]) * s, 
                   lower=-2, upper=0, subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(s, fDENS510y[[k]][i]) * s, 
                   lower=0, upper=1.5, subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(s, fDENS510y[[k]][i]) * s, 
                   lower=1.5, upper=3, subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(s, fDENS510y[[k]][i]) * s, 
                   lower=3, upper=5, subdivisions=1024)$value
    (int1+int2+int3+int4)
  }}
stopCluster(cl); rm(cl)
dimnames(ESPER510) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); ESPER510=as.data.frame(ESPER510)
rm(k)

for(i in 1:nprev) plot(ESPER510[,i], type="l"); rm(i)

tsj = ts(ESPER510[, 1], start = c(2007, 1), freq = 4)
monthplot(tsj)
tsj.seas = seas(  x = tsj,  transform.function = "none",  regression.aictest = NULL,  outlier = NULL,
                  regression.variables = c("tl2007.01-2008.4", "ao2009.3", "ls2011.2"),  
                  arima.model = "(1 0 0)(0 1 0)"
)
tsj.seas.5y=tsj.seas
summary(tsj.seas )
# view(tsj.seas)
Pacf(resid(tsj.seas),36); Acf(resid(tsj.seas),36)
plot(resid(tsj.seas)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas$series$s10); abline(h=1, lty=2, xpd=F)      
plot(tsj.seas$series$s12); 
monthplot(tsj.seas$series$s10); abline(h=0, lty=2, xpd=F)
#++++++++++++++++++++++
tsj2 = ts(ESPER510[, 2], start = c(2007, 1), freq = 4)
monthplot(tsj2)
tsj.seas2 = seas(  x = tsj2,  transform.function = "none",  regression.aictest = NULL,  outlier = NULL,
                   regression.variables = c("tl2009.01-2011.1", "tc2017.1", "ao2009.3", "ao2012.1"),
                   arima.model = "(0 1 0)(1 0 1)"
)
tsj.seas.10y=tsj.seas2
summary(tsj.seas2 )
# view(tsj.seas2)
Pacf(resid(tsj.seas2),36); Acf(resid(tsj.seas2),36)
plot(resid(tsj.seas2)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas2$series$s10); abline(h=0, lty=2, xpd=F)      
plot(tsj.seas2$series$s12); 
monthplot(tsj.seas2$series$s10); abline(h=0, lty=2, xpd=F)


plot(tsj.seas$series$s12); lines(tsj.seas2$series$s12, col="red"); 



# Desvio padrao das densidades -----------------------------------------------------------------------

cl <- makeCluster(ncores); registerDoParallel(cl)
DP510 = foreach(k = 1:nprev, .combine=cbind) %do% {
  foreach(i = 1:ntri, .combine=c, .packages='fda') %dopar% {
    int1=integrate(function(s) eval.fd(s, fDENS510y[[k]][i]) * ( (s-ESPER510[i,k])^2 ), 
                   lower=-2, upper=0, subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(s, fDENS510y[[k]][i]) * ( (s-ESPER510[i,k])^2 ), 
                   lower=0, upper=1.5, subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(s, fDENS510y[[k]][i]) * ( (s-ESPER510[i,k])^2 ), 
                   lower=1.5, upper=3, subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(s, fDENS510y[[k]][i]) * ( (s-ESPER510[i,k])^2 ), 
                   lower=3, upper=5, subdivisions=1024)$value
    sqrt(int1+int2+int3+int4)
  }}
stopCluster(cl); rm(cl)
dimnames(DP510) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); DP510=as.data.frame(DP510)
rm( k)

for(i in 1:nprev) plot(DP510[,i], type="l", ylim = c(0.3,1.3)); rm(i)





  
  
  








