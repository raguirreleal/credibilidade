

cl <- makeCluster(ncores); registerDoParallel(cl)

int = 0.5
breaks = seq(-2.5+int,  6.5-int,  by=int)
nbreaks = length(breaks)
ybase2 = create.bspline.basis(rangeval=rangeg, breaks=breaks, norder=3)
yparam2 = fdPar(ybase2, 1, lambda=0)
n = ybase2$nbasis

s = seq(rangeg[1], rangeg[2], by=0.0001)
fB = smooth.basis(s,  eval.basis(s, ybase2), yparam2)$fd
dfB = (deriv.fd(fB))
rm(s)

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
nint = 18
Bi = foreach(p = 1:nint, .combine=rbind) %:% foreach(q = 1:ybase2$nbasis, .combine=c) %do% {
  rg = ints[p:(p+1)]
  integrate(function(s) eval.basis(s, ybase2)[,q], 
            lower=as.double(rg[1]), upper=as.double(rg[2]), subdivisions=1024)$value
}
rm(p, q, rg, ints, nint)


xeval = seq((-2.5+int/2), (6.5-int/2), by=int)
nint = 18
B = foreach(p = 1:nint, .combine=rbind) %:% foreach(q = 1:ybase2$nbasis, .combine=c) %do% {
  eval.basis(xeval[p], ybase2)[,q]
}
rm(p, q)


j = 1
lambdas = seq(0.001, 0.05, by=0.005)
betas = seq(40, 100, by=10)
Mgcv = matrix(NA, nrow=length(betas), ncol=length(lambdas), dimnames=list(betas, lambdas) )

tBi = t(Bi); tB = t(B)
tempo.proc = system.time({
  GCV = foreach(k = 1:ntri, .packages=c('fda','MASS')) %dopar% {
    for(l in 1:length(lambdas)) {
      for(b in 1:length(betas)) {
        #--------------
        # x  = eval.fd(evalarg=xeval, fdobj=DENSm[[j]][[k]])
        # xi = int*x
        # inv = ginv( t(B)%*%B + betas[b]*t(Bi)%*%Bi + lambdas[l]*D )
        # cS = inv %*% (t(B) + 0.5*betas[b]*t(Bi))
        # c = cS %*% x
        # S = B %*% cS
        # I = diag(nint) # matriz identidade
        # traco = sum(diag(I-S)) # traço = soma diagonal principal
        # SSE  =  as.double(t(x - B%*%c)%*%(x - B%*%c))
        # #SSDP = as.double(t(xi-Bi%*%c)%*%(xi-Bi%*%c)) # "SSE" - soma dos quadrados das diferenças de probabilidade
        # #Mgcv[b,l] = (SSDP/nint) / (traco/nint)^2
        # Mgcv[b,l] = (SSE/nint) / (traco/nint)^2
        #--------------
        x  = eval.fd(evalarg=xeval, fdobj=DENSm[[j]][[k]])
        cS = (ginv( tB%*%B + betas[b]*tBi%*%Bi + lambdas[l]*D )) %*% (tB + 0.5*betas[b]*tBi)
        # cS = (ginv( tBi%*%Bi + lambdas[l]*D )) %*% (0.5*tBi)
        c  = round(cS%*%x, 3)
        c[c<0] = 0 
        for(r in 1:nint) {
          pmx = which.max(c)
          c[(pmx-1+min(which(c[pmx:nint]<0.005))):nint]=0
          c[1:(max(which(c[1:pmx]<0.005)))]=0
        }
        ff = fd(c, ybase2)
        intk = integrate(function(s) eval.fd(evalarg=s, fdobj=ff), lower=-2.5, upper=0, subdivisions=1024)$value +
          integrate(function(s) eval.fd(evalarg=s, fdobj=ff), lower=0, upper=2, subdivisions=1024)$value +
          integrate(function(s) eval.fd(evalarg=s, fdobj=ff), lower=2, upper=4, subdivisions=1024)$value +
          integrate(function(s) eval.fd(evalarg=s, fdobj=ff), lower=4, upper=6.5, subdivisions=1024)$value
        c <- round(c/intk, 3)
        #SSDP abaixo:
        Mgcv[b,l]=((as.double(t((int*x)-Bi%*%c)%*%((int*x)-Bi%*%c)))/nint) /
          ((sum(diag((diag(nint))-(Bi%*%cS))))/nint)^2
        #SSE abaixo:
        # Mgcv[b,l] = ((as.double(t(x-B%*%c)%*%(x-B%*%c)))/nint) / ((sum(diag((diag(nint))-(B%*%cS))))/nint)^2
      }  
    }
    Mgcv
  }
})[3]
beep(1)


C = foreach(k = 1:ntri, .combine=cbind) %do% {
  posmin = which(GCV[[k]]==min(GCV[[k]]), arr.ind = TRUE)
  lambda = lambdas[posmin[2]]
  beta = betas[posmin[1]]
  print(paste("k=",k,"trimestre:", tri[k], "  --  beta:", beta, "   lambda:", lambda))
  x  = eval.fd(evalarg=xeval, fdobj=DENSm[[j]][[k]])
  cS = ginv( tB%*%B + (beta*tBi)%*%Bi + (lambda*D) ) %*% (tB + 0.5*beta*tBi)
  # cS = (ginv( tBi%*%Bi + (lambda*D) )) %*% (0.5*tBi)
  c = round(cS%*%x, 3)
}
C[C<0] = 0     #abs(C)
fdens = fd(C, ybase2)
rm(x, c, lambda, beta)


SDAP = foreach(k = 1:ntri, .combine=c) %do% {
  for(r in 1:nint) {
    pmx = which.max(C[,k])
    C[(pmx-1+min(which(C[pmx:nint,k]<0.005))):nint, k]=0
    C[1:(max(which(C[1:pmx,k]<0.005))), k]=0
  }
  intk = integrate(function(s) eval.fd(evalarg=s, fdobj=fdens[k]), lower=-2.5, upper=0, subdivisions=1024)$value +
    integrate(function(s) eval.fd(evalarg=s, fdobj=fdens[k]), lower=0, upper=2, subdivisions=1024)$value +
    integrate(function(s) eval.fd(evalarg=s, fdobj=fdens[k]), lower=2, upper=4, subdivisions=1024)$value +
    integrate(function(s) eval.fd(evalarg=s, fdobj=fdens[k]), lower=4, upper=6.5, subdivisions=1024)$value
  fdens$coefs[,k] <- round(fdens$coefs[,k]/intk, 3)
  z=0
  for(i in 1:nint) {
    intD=integrate(function(s) eval.fd(evalarg=s, fdobj=DENSm[[j]][[k]]),
                   lower=grid.int$a[i], upper=grid.int$b[i], subdivisions=1000)$value
    intsD=integrate(function(s) eval.fd(evalarg=s, fdobj=fdens[k]),
                    lower=grid.int$a[i], upper=grid.int$b[i], subdivisions=1000)$value
    z = z + abs(intsD-intD)
  }
  z   # SDAP: soma das diferenças absolutas de probabilidade
}

sum(SDAP) 
# (SDAP)/18

foreach(k = 1:ntri, .combine=cbind) %do% {
  plot(fdens[k], main=paste(j, k))
  lines(DENSm[[j]][[k]]) 
}
#rm(j, k)



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




best.yaj = difDsD = matrix(Inf, length(tri), nprev)
dimnames(best.yaj) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); best.yaj=as.data.frame(best.yaj)
dimnames(difDsD) = list(trimestre=tri, prev=paste0("prev",c(1:nprev))); difDsD=as.data.frame(difDsD)

{
  x = grid.int$c  
  breaks = c(ini.grid, grid.int$b) 
  ybase2 = create.bspline.basis(rangeval=rangeg, breaks=breaks, norder=3)  # quadrático
  #yparam2 = fdPar(ybase2, 2, lambda=0.001)    # lambda->0, total fitting   1e-1
  yparam2 = fdPar(ybase2, 1, lambda=.02)    # .02
  nomes = list(Inflacao="", tri=tri, funcao="Probab (media suave)")
}
for(u in 6:50) { #10:50
  yaj = 1 + u/20  #yaj = 3  #2.6 
  sDENSm = foreach(j = 1:nprev) %do% {
    Y = foreach(k = 1:ntri, .combine=cbind) %do% eval.fd(evalarg=x, fdobj=DENSm[[j]][[k]])
    Y = yaj^Y   #aumenta o peso, de forma nao-linear, dos valores mais altos
    Y = round(Y, 5) - 1
    
    fds = smooth.basis(x, Y, yparam2, fdnames = nomes)$fd     # Observacao funcional
    
    ss = foreach(k = 1:ntri, .combine=c, .packages='fda') %dopar% {
      int1=integrate(function(s) eval.fd(evalarg=s, fdobj=fds[k]), lower=-2.5, upper=0, subdivisions=1024)$value
      int2=integrate(function(s) eval.fd(evalarg=s, fdobj=fds[k]), lower=0, upper=2, subdivisions=1024)$value
      int3=integrate(function(s) eval.fd(evalarg=s, fdobj=fds[k]), lower=2, upper=4, subdivisions=1024)$value
      int4=integrate(function(s) eval.fd(evalarg=s, fdobj=fds[k]), lower=4, upper=6.5, subdivisions=1024)$value
      intk = int1+int2+int3+int4
      fds$coefs[,k] <- abs(fds$coefs[,k]/intk)
      z=0
      for(i in 1:length(grid.int$c)) {
        intD=integrate(function(s) eval.fd(evalarg=s, fdobj=DENSm[[j]][[k]]),
                       lower=grid.int$a[i], upper=grid.int$b[i], subdivisions=100)$value
        intsD=integrate(function(s) eval.fd(evalarg=s, fdobj=fds[k]),
                        lower=grid.int$a[i], upper=grid.int$b[i], subdivisions=100)$value
        z = z + abs(intsD-intD)
      }
      z   # SAE  --  soma erros absolutos
      #z/length(grid.int$c)  # MAE -- erro absoluto médio
    }
    best.yaj[ (ss<difDsD[,j]) ,j] = yaj
    difDsD[ (ss<difDsD[,j]) ,j] = ss[(ss<difDsD[,j])]
    fds
  } }
beep(1)

sDENSm = foreach(j = 1:nprev) %do% {
  Y = foreach(k = 1:ntri, .combine=cbind) %do% eval.fd(evalarg=x, fdobj=DENSm[[j]][[k]])
  for(i in 1:ntri) Y[,i]=best.yaj[i,j]^Y[,i]
  Y = round(Y, 2) - 1
  fds = smooth.basis(x, Y, yparam2, fdnames = nomes)$fd
  foreach(k = 1:ntri) %do% {
    int1=integrate(function(s) eval.fd(evalarg=s, fdobj=fds[k]), lower=-2.5, upper=0, subdivisions=1024)$value
    int2=integrate(function(s) eval.fd(evalarg=s, fdobj=fds[k]), lower=0, upper=2, subdivisions=1024)$value
    int3=integrate(function(s) eval.fd(evalarg=s, fdobj=fds[k]), lower=2, upper=4, subdivisions=1024)$value
    int4=integrate(function(s) eval.fd(evalarg=s, fdobj=fds[k]), lower=4, upper=6.5, subdivisions=1024)$value
    intk = int1+int2+int3+int4
    fds$coefs[,k] <- abs(fds$coefs[,k]/intk)
  }
  fds
}
# write.xlsx(difDsD, "Tabela Script 2.0 - difDsD.xlsx", rowNames=T, colNames=T)
rm(nomes, x, Y, yparam2, j, k, i, u, fds, breaks, ss, yaj, intk, int1, int2, int3, int4, difDsD) #;rm(ybase2)

plot(sDENSm[[1]])

foreach(j = 1:nprev) %:% foreach(k = 1:ntri) %do% {
  plot(DENSm[[j]][[k]], ylim=c(-0.1, 1.2), main=paste(j, k))
  lines(sDENSm[[j]][k])
}




# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





calcc = function(j, k, lambda) {
  c = round(cS%*%x, 3)
  for(i in 1:nint) {
    if(c[i]<0) {
      s = seq(grid.int$a[i], grid.int$b[i], by=0.005)
      repeat {
        ff = fd(c, ybase2)
        evalint = as.double(eval.fd(evalarg=s, fdobj=ff))
        if(sum(evalint<0)>0) {
          c[i]=c[i]+0.001 
        }
        else break()
      }
    }
  }
  ff = fd(c, ybase2)
  intk = integrate( function(s) eval.fd(evalarg=s,fdobj=ff), lower=-2.5, upper=  0, subdivisions=1024 )$value +
    integrate( function(s) eval.fd(evalarg=s,fdobj=ff), lower=   0, upper=  2, subdivisions=1024 )$value +
    integrate( function(s) eval.fd(evalarg=s,fdobj=ff), lower=   2, upper=  4, subdivisions=1024 )$value +
    integrate( function(s) eval.fd(evalarg=s,fdobj=ff), lower=   4, upper=6.5, subdivisions=1024 )$value
  c = round(c/intk, 3)
}


ints = c(ini.grid, grid.int$b) 
xeval = seq((ini.grid+intg/2), (fim.grid-intg/2), by=intg)
Bi = foreach(p = 1:nint, .combine=rbind) %:% foreach(q = 1:n, .combine=c) %do% {
  integrate(function(s) eval.basis(s, ybase2)[,q], lower=as.double(ints[p]), upper=as.double(ints[p+1]))$value
}
tBi = t(Bi); tB = t(B)

sDENSm = foreach(j = 1:nprev) %do% {
  C = foreach(k = 1:ntri, .combine=cbind) %do% {
    x  = eval.fd(evalarg=xeval, fdobj=DENSm[[j]][[k]])
    cS = (ginv( tBi%*%Bi )) %*% (intg*tBi)
    c = calcc(j, k, lambda)
  }
  fdens = fd(C, ybase2)
}




stopCluster(cl); rm(cl); print("OK")










