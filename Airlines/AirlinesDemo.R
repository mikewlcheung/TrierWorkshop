## ------------------------------------------------------------------------
## Installing required R packages from the CRAN
for (i in c("dplyr", "lme4", "metaSEM")) {
  if (!(i %in% rownames(installed.packages()))) install.packages(i)
}

## ---- eval=FALSE---------------------------------------------------------
## library(R.utils)
## library(dplyr)
## library(readr)
## library(purrr)
## 
## ## Years of the data
## years <- 1987:2008
## 
## ## Create http addresses for download
## http.names <- paste("http://stat-computing.org/dataexpo/2009/",
##                     years, ".csv.bz2", sep="")
## 
## ## Show the first few items
## head(http.names)
## 
## ## Create file names to save in the local harddisk
## file.names <- paste(years, ".csv.bz2", sep="")
## 
## ## Show the first few items
## head(file.names)
## 
## ## Download the files
## ## This may take a while depending on the internet connectivity.
## for (i in 1:length(http.names)) {
##   download.file(http.names[i], file.names[i])
## }
## 
## ## Uncompress the files
## ## remove=FALSE: not to remove the compressed files
## for (i in 1:length(file.names)) {
##   bunzip2(file.names[i], overwrite=TRUE, remove=FALSE)
##   cat("Completed file: ", file.names[i], "\n")
## }
## 
## ## Randomly select 1 % of the data and save it in "AirlinesDemo.RData"
## 
## ## Set seed for replicability
## set.seed(39133)
## 
## # Randomly select 1% of the data
## size <- 0.01
## my.list <- list.files(pattern = "*.csv$")
## Airlines <- list()
## 
## for (i in seq_along(my.list)) {
##   Airlines[[i]] <- read_csv(my.list[i]) %>% group_by(Month) %>% sample_frac(size=size) %>%
##                        select(Year, Month, DayofMonth, DayOfWeek, ArrDelay, DepDelay,
##                               Origin, Dest, Distance)
##   cat("Completed dataset: ", my.list[i], "\n")
## }
## 
## Airlines <- bind_rows(Airlines)
## save(Airlines, file="AirlinesDemo.RData")

## ---- eval=TRUE----------------------------------------------------------
## Load the data
load("AirlinesDemo.RData")

library(dplyr)

## Calculate the means of ArrDelay, DepDelay, and total no. of flights
## by year and month
my.summary <- Airlines %>% group_by(Year, Month) %>% 
              summarise(arr_delay=mean(ArrDelay, na.rm = TRUE),
                        dep_delay=mean(DepDelay, na.rm = TRUE),
                        distance=mean(Distance, na.rm = TRUE),
                        flights=n())

## Sort it by Year and Month
my.summary <- arrange(my.summary, Year, as.numeric(Month))

## Display the summary and figures
# The red lines in the figures refer to the *September 11 attacks*.

## values for x axis
x <- 1:nrow(my.summary)

## Plot the no. of flights
plot(x, my.summary$flights, type="l", xaxt="n",
     xlab="Year", ylab="Numbers of flights per month",
     main="Numbers of flights (0.1% of the data) per month by years (1987-2008)")
abline(v=c(x[my.summary$Month=="1"],256), lty=2)
abline(v=168, lwd=3, col="red")
axis(1, at=c(-3, x[my.summary$Month=="6"]), labels=1987:2008)

## Plot the delay time
par(mfrow=c(3,1))
plot(x, my.summary$arr_delay, type="l", xaxt="n",
     xlab="Year", ylab="Arrival delay (min)",
     main="Arrival delay by years and months")
abline(v=c(x[my.summary$Month=="1"],256), lty=2)
abline(v=168, lwd=3, col="red")
axis(1, at=c(-3, x[my.summary$Month=="6"]), labels=1987:2008)

plot(x, my.summary$dep_delay, type="l", xaxt="n",
     xlab="Year", ylab="Departure delay (min)",
     main="Departure delay by years and months")
abline(v=c(x[my.summary$Month=="1"],256), lty=2)
abline(v=168, lwd=3, col="red")
axis(1, at=c(-3, x[my.summary$Month=="6"]), labels=1987:2008)

plot(x, with(my.summary, arr_delay-dep_delay), type="l", xaxt="n",
     xlab="Year", ylab="Departure delay (min)",
     main="Arrival minus departure delay by years and months")
abline(v=c(x[my.summary$Month=="1"],256), lty=2)
abline(v=168, lwd=3, col="red")
abline(h=0, lty=2)
axis(1, at=c(-3, x[my.summary$Month=="6"]), labels=1987:2008)

## Plot the scatter plot
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor=2, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    text(0.5, 0.5, txt, cex = cex.cor)
}

panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}

pairs(my.summary[, c("arr_delay", "dep_delay", "distance", "flights")],
      lower.panel = panel.smooth, upper.panel = panel.cor,
      diag.panel = panel.hist)

## Ecological analysis: Regression analysis on the aggregated means
## I(distance/1000): Distance is divided by 1000 to improve numerical stability.
summary( lm(arr_delay~dep_delay+I(distance/1000), data=my.summary) )

## ---- echo=FALSE, message=FALSE------------------------------------------
## Load the library for meta-analysis
library(metaSEM)

## Try to use multiple cores in OpenMx, which may speed up some of the analyses
mxOption(NULL, 'Number of Threads', (parallel::detectCores()-1))

my.model <- "ArrDelay~y1*DepDelay+y2*Distance"

## Display the graphica model
plot(my.model, col="yellow")

## ------------------------------------------------------------------------
## Function to fit regression analysis
## I(Distance/1000): Distance is divided by 1000 to improve numerical stability.
## y1 and y2: Regression coefficients from Distance and DepDelay.
## v11 to v22: Sampling covariance matrix of the parameter estimates
fun.reg <- function(dt) { 
           ## Run the analysis and capture the error
           fit <- try(lm(ArrDelay~DepDelay+I(Distance/1000), data=dt), silent=TRUE)

           ## If it is error, returns NaN
           if (is.element("try-error", class(fit))) {
               c(y1=NaN, y2=NaN, v11=NaN, v21=NaN, v22=NaN)
              } else {
               ## Regression coefficients excluding the intercept
               ## Remove the additional names 
               y <- unname(coef(fit))
               ## sampling variance covariance matrix excluding the intercept
               v <- vech(vcov(fit)[-1, -1])
               c(y1=y[2], y2=y[3], v11=v[1], v21=v[2], v22=v[3])
              }
}

## Run the analysis by Year
meta.df1 <- Airlines %>% group_by(Year) %>% do(mod=fun.reg(.))
meta.df1

meta.df1 <- data.frame(Year=meta.df1$Year,
                       t(apply(meta.df1, 1, function(x) x$mod)))
head(meta.df1)

## ---- eval=FALSE---------------------------------------------------------
## ## Data.frame to store output
## meta.df1 <- data.frame(Year=NA,y1=NA,y2=NA,v11=NA,v21=NA,v22=NA)
## 
## ## Years for the analyses
## Year <- unique(Airlines$Year)
## 
## ## It took about 9 minutes in our computer
## for (i in seq_along(Year)){
##     ## Fit regression model and store results
##     meta.df1[i, ] <- c(Year[i], fun.reg(Airlines[Airlines$Year==Year[i], ]))
##     cat("Completed year: ", Year[i], "\n")
## }
## head(meta.df1)

## ---- message=FALSE------------------------------------------------------
## Meta-analyze results by using a random-effects meta-analysis
## y1: regression coefficient of DepDelay
## y2: regression coefficient of Distance/1000
REM.reg <- meta(y=cbind(y1,y2), v=cbind(v11,v21,v22), data=meta.df1,
                model.name="Regression analysis REM")
summary(REM.reg)

## Variance components of the random effects
VarComp.reg <- vec2symMat(coef(REM.reg, select="random"))
VarComp.reg

## Correlation between the random effects
cov2cor(VarComp.reg)

## Plot the effect sizes
plot(REM.reg, axis.labels=c("Regression coefficient DepDelay",
                            "Regression coefficient Distance"),
     ylim=c(-2.5,0.7), xlim=c(0.65,1.2), study.min.cex = 0.6)

## Mixed effects meta-analysis with year as moderator
## year was centered before the analysis.
REM.reg_mod <- meta(y=cbind(y1,y2), v=cbind(v11,v21,v22),
                    x = scale(Year, scale=FALSE), data=meta.df1,
                    model.name="Regression analysis REM with year as moderator")
summary(REM.reg_mod)

## ---- eval=TRUE, cache=TRUE----------------------------------------------
library(lme4)

## Function to fit regression analysis  
## y1 to y3: Intercept, DepDelay and Distance/1000.
## v11 to v33: Sampling covariance matrix of the parameter estimates
fun.lmer <- function(dt) {  fit <- try(lmer(ArrDelay~DepDelay+I(Distance/1000)+
                                       (1|Month)+(1|DayofMonth)+(1|DayOfWeek)+
                                       (1|Origin)+(1|Dest),
                                       REML=FALSE, na.action="na.omit",
                                       data=dt), silent=TRUE)
                            if (is.element("try-error", class(fit))) {
                                c(y1=NaN, y2=NaN, v11=NaN, v21=NaN, v22=NaN)
                                } else {
                                ## regression coefficients excluding the intercept
                                y <- unname(fixef(fit)[-1])
                                ## sampling variance covariance matrix excluding the intercept
                                v <- vcov(fit)[-1, -1]
                                c(y1=y[1], y2=y[2], v11=v[1,1],v21=v[2,1],v22=v[2,2])
                                }
}

## Run the analysis by Year
## It may takes several minuates!
meta.df2 <- Airlines %>% group_by(Year) %>% do(mod=fun.lmer(.))
meta.df2

meta.df2 <- data.frame(Year=meta.df2$Year,
                       t(apply(meta.df2, 1, function(x) x$mod)))
head(meta.df2)

## ---- message=FALSE------------------------------------------------------
## Meta-analyze results by using a random-effects meta-analysis
## y1: regression coefficient of DepDelay
## y2: regression coefficient of Distance/1000
meta.rem <- meta(y=cbind(y1,y2), v=cbind(v11,v21,v22), data=meta.df2,
                 model.name="Random effects model")
summary(meta.rem)

## Variance component of the random effects
VarComp.lmer <- vec2symMat(coef(meta.rem, select="random"))
VarComp.lmer

## Correlation between the random effects
cov2cor(VarComp.lmer)

plot(meta.rem, axis.labels=c("Regression coefficient on DepDelay",
                             "Regression coefficient on Distance"),
     ylim=c(-2.5,0.7), xlim=c(0.65,1.2), study.min.cex = 0.6)

## ------------------------------------------------------------------------
## Meta-analyze results with a mixed-effects meta-analysis with year as a predictor
## It may be necessary to use better starting values
## since the variances of the variance components are very different.
meta.mem <- meta(y=cbind(y1,y2), v=cbind(v11,v21,v22), data=meta.df2,
                 x=scale(Year, scale=FALSE),
                 model.name="Mixed effects model with year as a predictor")
summary(meta.mem)

## ------------------------------------------------------------------------
sessionInfo()

