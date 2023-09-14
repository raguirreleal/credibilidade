
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Script 3.0 - Credibilidade CORE-PCE
# Ricardo A. Leal 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(readxl); library(fda); library(KernSmooth)
library(doParallel); (ncores=detectCores())
library(foreach); library(iterators)
library(seasonal); library(seasonalview); library("forecast")# usar função seas()
#library(ggplot2)

cl <- makeCluster(ncores); registerDoParallel(cl)

# > DADOS E VARIAVEIS GLOBAIS ====================================================================================

tipo.infla  = "COREPCE"  # "COREPCE" ou "CORECPI"
peri        = "ano"  #Trimestral ("tri") ou Anual ("ano")
ano.inicio  = 2007
meta.centro = 2
  intervalo = 0.5

#++++++++++++++

database = read_excel("dados/SPFmicrodata.xlsx", sheet = tipo.infla, col_types = c(rep("numeric", 13)), na = "NA")
database = database[database$YEAR >= ano.inicio, ]
database$NAMEQ = paste0(database$YEAR, "Q", database$QUARTER)  # Cria nova coluna p/ nome trim (no fim tabela)

tri = unique(database$NAMEQ)   # Vetor com os nomes dos trimestres
ntri = length(tri)  

anos = unique(database$YEAR)   # Vetor com os nomes dos anos
nanos = length(anos)
  
# dados.tri = within(database, rm("COREPCE1","COREPCEA","COREPCEB","COREPCEC"))
# dados.ano  = within(database, rm("COREPCE1","COREPCE2","COREPCE3","COREPCE4","COREPCE5","COREPCE6"))

dados.tri = database[,-c(5,11:13)]
dados.ano = database[,-c(5:10)]

nprev.tri = 5
nprev.ano = 3
ndados = length(database$YEAR) 

metas     = rep(meta.centro, ntri)
metas.int = as.data.frame(cbind((metas-intervalo),(metas+intervalo)))
  dimnames(metas.int) = list(tri, c("LimInf", "LimSup"))

if(peri=="tri") {dados = dados.tri; nprev = nprev.tri} else {dados = dados.ano; nprev = nprev.ano}


# > ESTIMACAO DAS DENSIDADES =====================================================================================

  # se temos previsoes/expectativas para 1, 2, 3 e 4 trimestres (anos) a frente, e essas 4 (nprev) previsoes foram 
  # coletadas em cada um dos ntri trimestres, entao contruimos uma lista de nprev=4 matrizes. Nessas matrizes temos
  # em cada coluna, os ntri trimestres e nas linhas os n.dens valores para a densidade de probabilidade

  # Definition of variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
nknots = 70      # qtd de nos, ou breaks
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


# Esperança das densidades -----------------------------------------------------------------------

ESPERp = foreach(k = 1:nprev, .combine=cbind) %do% {
  foreach(i = 1:ntri, .combine=c, .packages='fda') %dopar% {
    int1=integrate(function(s) eval.fd(s, fDENS[[k]][i]) * s, 
                   lower=-2, upper=0, subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(s, fDENS[[k]][i]) * s, 
                   lower=0, upper=1.5, subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(s, fDENS[[k]][i]) * s, 
                   lower=1.5, upper=3, subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(s, fDENS[[k]][i]) * s, 
                   lower=3, upper=5, subdivisions=1024)$value
    (int1+int2+int3+int4)
  }}
dimnames(ESPERp) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); ESPERp=as.data.frame(ESPERp)
rm(k)

for(i in 1:nprev) plot(ESPERp[,i], type="l", ylim = c(0.7,2.3)); rm(i)

#==========  TRIMESTRES =============
#====================================
tsj = ts(ESPERp[, 1], start = c(2007, 1), freq = 4)
monthplot(tsj)
tsj.seas = seas(  x = tsj,  transform.function = "log",  regression.aictest = NULL,  outlier = NULL,
                  regression.variables = c("tl2007.01-2008.4", "ao2015.1", "ls2011.2"),
                  arima.model = "(2 0 1)(2 1 0)"
)
summary(tsj.seas )
# view(tsj.seas)
Pacf(resid(tsj.seas),36); Acf(resid(tsj.seas),36)
plot(resid(tsj.seas)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas$series$s10); abline(h=1, lty=2, xpd=F)      
plot(tsj.seas$series$s12); 
monthplot(tsj.seas$series$s10); abline(h=1, lty=2, xpd=F)
#++++++++++++++++++++++
tsj2 = ts(ESPERp[, 2], start = c(2007, 1), freq = 4)
monthplot(tsj2)
tsj.seas2 = seas(  x = tsj2,  transform.function = "none",  regression.aictest = NULL,  outlier = NULL,
                   regression.variables = c("tl2007.01-2008.3", "tl2009.01-2011.1"), arima.model = "(1 1 1)(0 1 1)"
)
summary(tsj.seas2 )
# view(tsj.seas2)
Pacf(resid(tsj.seas2),36); Acf(resid(tsj.seas2),36)
plot(resid(tsj.seas2)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas2$series$s10); abline(h=0, lty=2, xpd=F)      
plot(tsj.seas2$series$s12); 
monthplot(tsj.seas2$series$s10); abline(h=0, lty=2, xpd=F)
#++++++++++++++++++++++
tsj3 = ts(ESPERp[, 3], start = c(2007, 1), freq = 4)
monthplot(tsj3)
tsj.seas3 = seas(  x = tsj3,  transform.function = "log",  regression.aictest = NULL,  outlier = NULL,
                   regression.variables = c("tl2009.01-2011.1"),  arima.model = "(0 1 0)(0 1 1)"
)
summary(tsj.seas3 )
# view(tsj.seas3)
Pacf(resid(tsj.seas3),36); Acf(resid(tsj.seas3),36)
plot(resid(tsj.seas3)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas3$series$s10); abline(h=0, lty=2, xpd=F)      
plot(tsj.seas3$series$s12); 
monthplot(tsj.seas3$series$s10); abline(h=0, lty=2, xpd=F)
#++++++++++++++++++++++
tsj4 = ts(ESPERp[, 4], start = c(2007, 1), freq = 4)
monthplot(tsj4)
tsj.seas4 = seas(  x = tsj4,  transform.function = "none",  regression.aictest = NULL,  outlier = NULL,
                   regression.variables = c("tl2009.01-2011.1"),  arima.model = "(1 0 0)(0 1 1)"
)
summary(tsj.seas4 )
# view(tsj.seas4)
Pacf(resid(tsj.seas4),36); Acf(resid(tsj.seas4),36)
plot(resid(tsj.seas4)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas4$series$s10); abline(h=0, lty=2, xpd=F)      
plot(tsj.seas4$series$s12); 
monthplot(tsj.seas4$series$s10); abline(h=0, lty=2, xpd=F)

plot(tsj.seas$series$s12); lines(tsj.seas2$series$s12, col="red"); 
lines(tsj.seas3$series$s12, col="green"); lines(tsj.seas4$series$s12, col="blue"); 
#++++++++++++++++++++++
tsj5 = ts(ESPERp[, 5], start = c(2007, 1), freq = 4)
monthplot(tsj5)
tsj.seas5 = seas(  x = tsj5,  transform.function = "log",  regression.aictest = NULL,  outlier = NULL,
                               regression.variables = c("tl2009.01-2011.1"),  arima.model = "(1 0 0)(0 1 1)"
)
summary(tsj.seas5 )
view(tsj.seas5)
Pacf(resid(tsj.seas5),36); Acf(resid(tsj.seas5),36)
plot(resid(tsj.seas5)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas5$series$s10); abline(h=0, lty=2, xpd=F)      
plot(tsj.seas5$series$s12); 
monthplot(tsj.seas5$series$s10); abline(h=0, lty=2, xpd=F)

plot(tsj.seas$series$s12); lines(tsj.seas2$series$s12, col="red"); 
lines(tsj.seas3$series$s12, col="green"); lines(tsj.seas4$series$s12, col="blue"); 
lines(tsj.seas5$series$s12, col="orange"); 



#==========  ANOS  =============
#===============================
tsj = ts(ESPERp[, 1], start = c(2007, 1), freq = 4)
monthplot(tsj)
tsj.seas = seas(  x = tsj,  transform.function = "none",  regression.aictest = NULL,  outlier = NULL,
                  regression.variables = c("tl2007.01-2008.4", "tc2009.1"),  arima.model = "(1 0 0)(0 1 1)"
)
tsj.seas1.pontual=tsj.seas
summary(tsj.seas )
# view(tsj.seas)
Pacf(resid(tsj.seas),36); Acf(resid(tsj.seas),36)
plot(resid(tsj.seas)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas$series$s10); abline(h=1, lty=2, xpd=F)      
plot(tsj.seas$series$s12); 
monthplot(tsj.seas$series$s10); abline(h=0, lty=2, xpd=F)
#++++++++++++++++++++++
tsj2 = ts(ESPERp[, 2], start = c(2007, 1), freq = 4)
monthplot(tsj2)
tsj.seas2 = seas(  x = tsj2,  transform.function = "none",  regression.aictest = NULL,  outlier = NULL,
                   regression.variables = c("tl2007.01-2008.3", "tc2009.1"),  arima.model = "(1 0 0)(0 1 1)"
)
tsj.seas2.pontual=tsj.seas2
summary(tsj.seas2 )
# view(tsj.seas2)
Pacf(resid(tsj.seas2),36); Acf(resid(tsj.seas2),36)
plot(resid(tsj.seas2)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas2$series$s10); abline(h=0, lty=2, xpd=F)      
plot(tsj.seas2$series$s12); 
monthplot(tsj.seas2$series$s10); abline(h=0, lty=2, xpd=F)
#++++++++++++++++++++++
tsj3 = ts(ESPERp[, 3], start = c(2007, 1), freq = 4)
monthplot(tsj3)
tsj.seas3 = seas(  x = tsj3,  transform.function = "none",  regression.aictest = NULL,  outlier = NULL,
                   regression.variables = c("ls2009.1"),  arima.model = "(1 0 0)(0 1 1)"
)
tsj.seas3.pontual=tsj.seas3
summary(tsj.seas3 )
# view(tsj.seas3)
Pacf(resid(tsj.seas3),36); Acf(resid(tsj.seas3),36)
plot(resid(tsj.seas3)); abline(h=0, lty=2, xpd=F) 
plot(tsj.seas3$series$s10); abline(h=0, lty=2, xpd=F)      
plot(tsj.seas3$series$s12); 
monthplot(tsj.seas3$series$s10); abline(h=0, lty=2, xpd=F)





# Desvio padrao das densidades -----------------------------------------------------------------------

DP = foreach(k = 1:nprev, .combine=cbind) %do% {
  foreach(i = 1:ntri, .combine=c, .packages='fda') %dopar% {
    int1=integrate(function(s) eval.fd(s, fDENS[[k]][i]) * ( (s-ESPERp[i,k])^2 ), 
                   lower=-2, upper=0, subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(s, fDENS[[k]][i]) * ( (s-ESPERp[i,k])^2 ), 
                   lower=0, upper=1.5, subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(s, fDENS[[k]][i]) * ( (s-ESPERp[i,k])^2 ), 
                   lower=1.5, upper=3, subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(s, fDENS[[k]][i]) * ( (s-ESPERp[i,k])^2 ), 
                   lower=3, upper=5, subdivisions=1024)$value
    sqrt(int1+int2+int3+int4)
  }}
dimnames(DP) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); DP=as.data.frame(DP)
rm( k)

for(i in 1:nprev) plot(DP[,i], type="l", ylim = c(0,1.3)); rm(i)





stopCluster(cl); rm(cl)










  
  
  








