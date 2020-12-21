rm(list=ls())  # Careful! This clears all of R's memory!
require(rjags)               # Must have previously installed package rjags.

A <- read.csv(file.choose(),header=T)
dataList = list(    # Put the information into a list.
  AADT = A[,2],
  HV = A[,3],
  Len = A[,4],
  Lwid = A[,6],
  Func = A[,7],
  SH = A[,8],
  Medmark = A[,15],
  curv = A[,18],
  Medsp = A[,22],
  Crash = A[,36]
)


# Define the model:
jagsString="
model {

for( i in 1 : 903) {

log(mu[i]) <- beta[1] + beta[2]*log(AADT[i]) + beta[3]*HV[i] + beta[4]*log(Len[i]) + beta[5]*Lwid[i] + 
beta[6]*Func[i] + beta[7]*SH[i] + beta[8]*Medmark[i] + beta[9]*curv[i] + beta[10]*Medsp[i]

Crash[i] ~ dnegbin(z[i],phi)
z[i] <- phi/(phi + mu[i])

}

for (j in 1:10) {beta[j] ~ dnorm(0,0.01)}
phi ~ dexp(1)

}
"
###???????????
writeLines(jagsString, con="model1.jags")

# Run the chains:
jagsModel <- jags.model(file="model1.jags",data=dataList,n.chains = 2, n.adapt=2000, quiet=FALSE)
update(model.jags,n.iter=10000)

#plot(as.mcmc(jagsModel))
#autocorr.diag(as.mcmc(jagsModel))
#print(jagsModel)

codaSamples = coda.samples(model.jags,variable.names=c("beta[1]","beta[2]","beta[3]","beta[4]","beta[5]"),n.iter=5000)

# Examine the chains:
# Convergence diagnostics:
diagMCMC(codaObject=codaSamples,parName=("beta[1]"))










