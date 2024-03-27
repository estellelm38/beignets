rm(list=ls())
donnees <- read.table("beignets.txt",header=TRUE)

head(donnees)

X=donnees[,"Gras"]
Groupe=factor(donnees[,"Gr"])

#normalité
tapply(X, Groupe, shapiro.test)

#on ne rejette la normalité sur aucun groupe

#anova
anova(lm(donnees))

#contrastes
lambda.i=c(1,1,-1,-1)
xbar.i=tapply(X,Groupe,mean)
n=length(X)
n.i=tapply(rep(1,n),Groupe,sum)
a=length(levels(Groupe))
Q.i=tapply(X,Groupe,var)*(n.i-1)
SCE=sum(Q.i)
dl=n-a
CME=SCE/dl

phi=sum(xbar.i*lambda.i)
s.phi=sqrt(CME*sum(lambda.i^2/n.i))
tvalue=phi/s.phi
pvaleur=pt(tvalue,dl)
pvaleur=2*min(pvaleur,1-pvaleur)
list(c(phi.hat=phi,s.phi=s.phi),c(t=tvalue,p.value=pvaleur),dl=dl)

#nature de la matière grasse influe

contrast.test=function(X,Groupe,lambda.i,phi0=0){
  xbar.i=tapply(X,Groupe,mean)
  n=length(X)
  a=lentgh(leves(Groupe))
  n.i=tapply(rep(1,n),Groupe,sum)
  Q.i=tapply(X,Groupe,var)*(n.i-1)
  SCE=sum(Q.i)
  dl=n-a
  CME=SCE/dl
  phi=sum(xbar.i*lambda.i)
  s.phi=sqrt(CME*sum(lambda.i^2/n.i))
  tvalue=(phi-phi0)/s.phi
  pvaleur=pt(tvalue,dl)
  pvaleur=2*min(pvaleur,1-pvaleur)
  return(list(c(phi.hat=phi,s.phi=s.phi),c(t=tvalue,p.value=pvaleur),dl=dl))
}

contrast.test(X,Groupe,c(1,1,-1,-1),phi0=0)

#tableau ANOVA

a <- length(levels(Groupe))
n <- length(X)
n.i <- tapply(rep(1,n),Groupe,sum)
xbar.i <- tapply(X,Groupe,mean)
Q.I <- tapply(X,Groupe,var) * (n.i-1)

SCT <- (n-1)*var(X)
SCT
SCE <- sum(Q.i)
SCE
SCM <- SCT-SCE
SCM
dlE <- n-a
dlM <- a-1
CMM <- SCM/dlM
CME <- SCE/dlE
F.obs <- CMM/CME
seuil.crit<- 1-pf(F.obs,dlM,dlE)

Anova.tableau <- matrix(
  c(SCM,SCE,SCT,dlM,dlE,NA,CMM,CME,NA,F.obs,NA,NA,seuil.crit,NA,NA),
  nrow=3,dimnames=list(c("Inter groupe","Intra groupe","Totale"),
                       c("SC","dl","CM","F.obs","Pr(F)")
                       )
)
Anova.tableau