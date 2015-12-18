######################
# Historical Method
######################

# Download data
aapl = data.matrix(read.table(file.choose(), header=TRUE, sep="\t"))
msft = data.matrix(read.table(file.choose(), header=TRUE, sep="\t"))
xom = data.matrix(read.table(file.choose(), header=TRUE, sep="\t"))

# Calculate weekly portfolio returns
returns = aapl # 1 asset
returns = (aapl+msft)/2 # 2 assets
returns = (aapl+msft+xom)/3 # 3 assets

# Calculate 1-week 95% VaR
qq = quantile(returns,probs=c(0,.05))
var = qq[2]
var

# Create histogram
hh = hist(returns, breaks=80)

cuts = cut(hh$breaks, c(-Inf, var, Inf))
# change name as necessary
plot(hh, col=c("red","blue")[cuts],
main="Historical Returns: AAPL+MSFT+XOM",
xlab="1 Week Return (%)")

abline(v=var)

###########################
# Monte-Carlo Simulation
###########################

## AAPL

# Download data
aapl = data.matrix(read.table(file.choose(), header=TRUE, sep="\t"))

# Calculate Cauchy distribution parameters
xx = median(aapl)
gam = IQR(aapl) / 2 

# Plot histogram with Cauchy distribution
pp = hist(aapl, breaks=80,
main="Historical Returns: AAPL", freq=F,
xlab="1 Week Return (%)", ylim=c(0,0.1))

xfit = seq(min(aapl),max(aapl),length=40)
yfit = 1 / (pi * gam) * ( gam ** 2 / ( ( xfit - xx) ** 2 + gam ** 2))

lines(xfit,yfit,col="red",lwd=2)

# Generate random sample
sim.aapl = rcauchy(10000, location = xx, scale = gam)
write.table(sim.aapl, file.choose())

# Calculate VaR
qq = quantile(sim.aapl,probs=c(0,.05))
var = qq[2]
var

# Create histogram
hh = hist(sim.aapl, breaks=20000)

cuts = cut(hh$breaks, c(-Inf, var, Inf))

plot(hh, col=c("red","blue")[cuts],
main="Simulated Returns: AAPL",
xlab="1 Week Return (%)",xlim=c(-100,100))

abline(v=var)

hist(sim.aapl, breaks=5000, xlim=c(-100,100),
main="Simulated Returns: AAPL", col="blue",
xlab="1 Week Return (%)")
abline(v=var)

## AAPL + MSFT

# Download data
aapl = data.matrix(read.table(file.choose(), header=TRUE, sep="\t"))
msft = data.matrix(read.table(file.choose(), header=TRUE, sep="\t"))

# Calculate Cauchy distribution parameters
xx.1 = median(aapl)
gam.1 = IQR(aapl) / 2 

xx.2 = median(msft)
gam.2 = IQR(msft) / 2

# Generate random sample
sim.aapl = rcauchy(10000, location = xx.1, scale = gam.1)
write.table(sim.aapl, file.choose())

sim.msft = rcauchy(10000, location = xx.2, scale = gam.2)
write.table(sim.msft, file.choose())

# Calculate simulated returns
returns = (sim.aapl + sim.msft)/2

# Calculate VaR
qq = quantile(returns,probs=c(0,.05))
var = qq[2]
var

# Create histogram
hh = hist(returns, breaks=20000)

cuts = cut(hh$breaks, c(-Inf, var, Inf))

plot(hh, col=c("red","blue")[cuts],
main="Simulated Returns: AAPL+MSFT",
xlab="1 Week Return (%)",xlim=c(-100,100))

abline(v=var)

## AAPL + MSFT + XOM

# Download data
aapl = data.matrix(read.table(file.choose(), header=TRUE, sep="\t"))
msft = data.matrix(read.table(file.choose(), header=TRUE, sep="\t"))
xom = data.matrix(read.table(file.choose(), header=TRUE, sep="\t"))

# Calculate Cauchy distribution parameters
xx.1 = median(aapl)
gam.1 = IQR(aapl) / 2 

xx.2 = median(msft)
gam.2 = IQR(msft) / 2

xx.3 = median(xom)
gam.3 = IQR(xom) / 2


# Generate random sample
sim.aapl = rcauchy(10000, location = xx.1, scale = gam.1)
write.table(sim.aapl, file.choose())

sim.msft = rcauchy(10000, location = xx.2, scale = gam.2)
write.table(sim.msft, file.choose())

sim.xom = rcauchy(10000, location = xx.3, scale = gam.3)
write.table(sim.xom, file.choose())

# Calculate simurcaulated returns
returns = (sim.aapl + sim.msft + sim.xom)/3

# Calculate VaR
qq = quantile(returns,probs=c(0,.05))
var = qq[2]
var

#

# Create histogram
hh = hist(returns, breaks=5000)

cuts = cut(hh$breaks, c(-Inf, var, Inf))

plot(hh, col=c("red","blue")[cuts],
main="Simulated Returns: AAPL+MSFT+XOM",
xlab="1 Week Return (%)",xlim=c(-100,100))

abline(v=var)

###########################
# Other Cauchy Graphs
###########################

# MSFT

# Plot histogram with Cauchy distribution
pp = hist(msft, breaks=80,
main="Historical Returns: MSFT", freq=F,
xlab="1 Week Return (%)")

xfit = seq(min(msft),max(msft),length=40)
yfit = 1 / (pi * gam.2) * ( gam.2 ** 2 / ( ( xfit - xx.2) ** 2 + gam.2 ** 2))

lines(xfit,yfit,col="red",lwd=2)

# XOM

# Plot histogram with Cauchy distribution
pp = hist(xom, breaks=80,
main="Historical Returns: XOM", freq=F,
xlab="1 Week Return (%)",ylim=c(0,0.2))

xfit = seq(min(xom),max(xom),length=40)
yfit = 1 / (pi * gam.3) * ( gam.3 ** 2 / ( ( xfit - xx.3) ** 2 + gam.3 ** 2))

lines(xfit,yfit,col="red",lwd=2)


##############################
# Variance-Covariance Method #
##############################

# Download data
analytical = read.csv(file.choose(), header=TRUE, sep=",")

#One Asset Only: AAPL
# 5% VAR.AAPL = Total.Market.Value * Z.at.95.alpha * vol.AAPL
return.AAPL = analytical$X1.Week.Return[2:1043];
vol.AAPL = sd(return.AAPL); 
VAR.AAPL = 1000000/3 * 1.645 * vol.AAPL

#Two Assets: AAPL and MSFT
# 5% VAR.AAPL.MSFT = Total.Market.Value * Z.at.95.alpha * vol.portfolio2
weight.2 = c(1,1)/2 #(assume equal weighting)
return.MSFT = analytical$X1.Week.Return.1[2:1043]; #we already defined AAPL's returns into a vector previously
cov.AAPL.MSFT = cov(return.AAPL, return.MSFT);
varcov.matrix2 = rbind(c(var(return.AAPL),cov.AAPL.MSFT),c(cov.AAPL.MSFT, var(return.MSFT)))
vol.portfolio2 =  (weight.2 %*% varcov.matrix2 %*% weight.2)^0.5
VAR.AAPL.MSFT = 2000000/3 * 1.645 * vol.portfolio2

#Three Assets: AAPL, MSFT, and XOM
# 5% VAR.AAPL.MSFT.XOM = Total.Market.Value * Z.at.95.alpha * vol.portfolio3
weight.3 = c(1,1,1)/3 #(assume equal weighting)
return.XOM = analytical$X1.Week.Return.2[2:1043] #we already defined AAPL's and MSFT's returns previously
cov.AAPL.XOM = cov(return.AAPL, return.XOM);
cov.MSFT.XOM = cov(return.MSFT, return.XOM);
varcov.matrix3 = rbind(c(var(return.AAPL),cov.AAPL.MSFT,cov.AAPL.XOM),c(cov.AAPL.MSFT,var(return.MSFT),cov.MSFT.XOM), c(cov.AAPL.XOM,cov.MSFT.XOM,var(return.XOM)))
vol.portfolio3 =  (weight.3 %*% varcov.matrix3 %*% weight.3)^0.5
VAR.AAPL.MSFT.XOM = 1000000 * 1.645 * vol.portfolio3
