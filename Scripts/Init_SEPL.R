require(plyr)
require(dplyr)
require(lubridate)
require(tidyr)
require(jagsUI)
require(ggplot2)
require(gridExtra)

nestFiles <- list.files(path="Data/Nests/")
chickFiles <- list.files(path="Data/Chicks/")

nests <- ldply(as.list(nestFiles),
	function(filename) {
		df <- read.csv(paste("Data/Nests/", filename, sep=""), header=TRUE, stringsAsFactors=FALSE)
		df <- bind_cols(df, data.frame(Year=rep(as.numeric(substr(filename,6,9)), times=nrow(df))))
		return(df)
})

chicks <- ldply(as.list(chickFiles),
	function(filename) {
		df <- read.csv(paste("Data/Chicks/", filename, sep=""), header=TRUE, stringsAsFactors=FALSE)
		return(df)
})

nests <- nests %>%
			unite(Male.ID, Male.P, Male.S, sep=".") %>%
			unite(Female.ID, Female.P, Female.S, sep=".")

nests[nests$Male.ID=="NA.NA", "Male.ID"] <- NA
nests[nests$Female.ID=="NA.NA", "Female.ID"] <- NA

chicks <- unite(chicks, ID, Prefix, Suffix, sep=".")

nyears <- max(nests$Year) - min(nests$Year) + 1 

mIDS <- unique(nests$Male.ID)
fIDS <- unique(nests$Female.ID)
chickIDS <- unique(chicks$ID)

sexes <- rep("", times=length(mIDS) + length(fIDS) + length(chickIDS))
names(sexes) <- unique(c(mIDS, fIDS, chickIDS))[-1]
for (i in 1:length(sexes)) {
	if (names(sexes)[i] %in% nests$Female.ID) { 
		sexes[i] <- "F"
	} else if (names(sexes)[i] %in% nests$Male.ID) {
		sexes[i] <- "M"
	} else {
		if (runif(1,0,1) < 0.5) {
			sexes[i] <- "F"
		} else {
			sexes[i] <- "M"
		}
	}
}

sexes <- sexes[!is.na(names(sexes))]

obs <- matrix(0, nrow = length(mIDS) + length(fIDS) + length(chickIDS), ncol=nyears)
rownames(obs) <- c(mIDS, fIDS, chickIDS)

for ( i in 1:nrow(nests)) {
	if (!is.na(nests$Male.ID[i]) & nests$Male.ID[i] != "") {
		obs[nests$Male.ID[i], nests$Year[i]-2009] <- 1
	}
	if (!is.na(nests$Female.ID[i]) & nests$Female.ID[i] != "") {
		obs[nests$Female.ID[i], nests$Year[i]-2009] <- 1
	}
}

obs <- obs[rownames(obs)!="" & !is.na(rownames(obs)),]

createMArray <- function(obs) {
	nind <- dim(obs)[1]
	n.years <- dim(obs)[2]

	m.array <- matrix(0, nrow=n.years, ncol=n.years+1)
	for (t in 1:n.years) { m.array[t,1] <- sum(obs[,t]) }
	for ( i in 1:nind) {
		pos <- which(obs[i,] != 0)
		for (z in 1:length(pos) - 1) {
			m.array[pos[z],pos[z+1]] <- m.array[pos[z], pos[z+1]] + 1
		}
	}
	for (t in 1:n.years) {
		m.array[t, n.years+1] <- m.array[t,1] - sum(m.array[t,2:n.years])
	}
	out <- m.array[1:(n.years-1), 2:(n.years+1)]
	return(out)
}

adObs.F <- obs[sexes[rownames(obs)] == "F" & !(rownames(obs) %in% chickIDS),]
adObs.M <- obs[sexes[rownames(obs)] == "M" & !(rownames(obs) %in% chickIDS),]

jObs.F <- obs[sexes[rownames(obs)] == "F" & (rownames(obs) %in% chickIDS),]
for (i in 1:nrow(jObs.F)) {
	nestYear <- chicks[chicks$ID == rownames(jObs.F)[i], "Year"]
	jObs.F[i, nestYear-2009] <- 1
}

jObs.M <- obs[sexes[rownames(obs)] == "M" & (rownames(obs) %in% chickIDS),]
for (i in 1:nrow(jObs.M)) {
	nestYear <- chicks[chicks$ID == rownames(jObs.M)[i], "Year"]
	jObs.M[i, nestYear-2009] <- 1
}

marray.a.F <- createMArray(adObs.F)
marray.a.M <- createMArray(adObs.M)
marray.1.F <- createMArray(jObs.F)
marray.1.M <- createMArray(jObs.M)

# formulate final m-arrays
marray.F <- rbind(marray.1.F, marray.a.F)
marray.M <- rbind(marray.1.M, marray.a.M)

# number of released individuals during each year, simple sum across m-array row
r.F <- rowSums(marray.F)
r.M <- rowSums(marray.M)

# pure population count (two for each nest that's not a re-nest)
nestCount <- nests[!endsWith(nests$Nest, "R"),] %>%
				group_by(Year) %>%
				count()

y.F <- y.M <- nestCount$n

# productivity is, for now, the count of chicks banded each year
numChicks <- sapply(min(chicks$Year):max(chicks$Year),
			function(y) {
				yearChicks <- chicks[chicks$Year == y & (chicks$Nest %in% nests$Nest),]
				return(nrow(yearChicks))
})
