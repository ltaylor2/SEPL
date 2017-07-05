source("Scripts/Init_SEPL.R")

jags.data <- list(nyears=nyears,
				  marray.F=marray.F, marray.M=marray.M,
				  r.F=r.F, r.M=r.M, y.F=y.F, y.M=y.M, R=R,
				  numChicks=numChicks)

jags.inits <- function() {
  list(m.phi1.F=runif(1,0,1), m.phia.F=runif(1,0,1),
       m.recap.1.F=runif(1,0,1), m.recap.a.F=runif(1,0,1),
       m.phi1.M=runif(1,0,1), m.phia.M=runif(1,0,1),
       m.recap.1.M=runif(1,0,1), m.recap.a.M=runif(1,0,1),
       m.omega.F=runif(1,0,10), m.omega.M=runif(1,0,10),
       m.prod= runif(1,0,5),
       N.imm.F=c(NA, round(runif(nyears-1,0,10))), N.imm.M=c(NA, round(runif(nyears-1,0,10))))
}

params <- c("phi1.F", "phia.F", "m.phi1.F","m.phia.F",
            "recap.1.F","recap.a.F","m.recap.1.F", "m.recap.a.F",
            "phi1.M", "phia.M", "m.phi1.M","m.phia.M",
            "recap.1.M","recap.a.M","m.recap.1.M","m.recap.a.M",
            "omega.F", "omega.M", "m.omega.F", "m.omega.M",
            "immRate.F", "immRate.M",
            "N.1.F", "N.1.M",
            "N.sad.F", "N.sad.M",
            "N.imm.F", "N.imm.M",
            "Ntot", "b.F", "b.M",
            "prod", "m.prod",
            "lambda", "m.lambda")

ipm <- jags(data=jags.data, inits=jags.inits, parameters.to.save=params, model.file="Scripts/ipm.SEPL.bug",
                     n.chains=3, n.thin=10, n.iter=100000, n.burnin=50000, parallel=TRUE)
