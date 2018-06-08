## ---- eval=FALSE---------------------------------------------------------
## ## You don't need to download the data. The data are stored in "WVS.Rdata"
## 
## library(dplyr)
## 
## ## Unzip the downloaded file
## unzip("WVS_Longitudinal_1981-2014_rdata_v_2015_04_18.zip")
## 
## ## Load the data into R
## load("WVS_Longitudinal_1981_2014_R_v2015_04_18.rdata")
## 
## ## Display the size of the dataset
## print(object.size(x=lapply(ls(), get)), units="Mb")
## 
## ## 1895.3 Mb
## 
## ## Rename the object for ease of data analyses
## WVS <- `WVS_Longitudinal_1981_2014_R_v2015_04_18`
## 
## ## Remove the old one to clean up memory
## rm("WVS_Longitudinal_1981_2014_R_v2015_04_18")
## 
## ## Set seed for reproducibility
## set.seed(391373)
## 
## # Randomly select 25% of the data for the workshop
## size <- 0.25
## 
## ## Group by S002 (Wave) and S003 (Region)
## ## Select 25% of the data
## WVS <- WVS %>% group_by(S002, S003) %>% sample_frac(size=size)
## 
## ## Save the data so that we do not need to read it from raw data again
## save(WVS, file="WVS.Rdata")

## ---- message=FALSE, cache=FALSE-----------------------------------------
## Load the libraries
library(dplyr)
library(lavaan)
library(metaSEM)

## Try to use multiple cores in OpenMx. It may speed up some of the analyses.
mxOption(NULL, 'Number of Threads', (parallel::detectCores()-1))

## Load the data
load("WVS.Rdata")

## Select the relevant variables to minimize the memory usage
WVS <- select(WVS, c(A009, A170, A173, C006, X001, X003, S002, S003))

## Data cleaning
## Reverse coding for A009
## Recode all negative values as NA
## Age (X003) is divided by 10 to improve the numerical stability.
WVS <- mutate(WVS,
              A009 = 5-ifelse(A009 < 0, yes=NA, no=A009),
              A170 =   ifelse(A170 < 0, yes=NA, no=A170),
              A173 =   ifelse(A173 < 0, yes=NA, no=A173),
              C006 =   ifelse(C006 < 0, yes=NA, no=C006),
              X001 =   ifelse(X001 < 0, yes=NA, no=X001),
              X003 =   ifelse(X003 < 0, yes=NA, no=X003/10))

## No. of pseudo studies
k <- 100

## Set seed for reproducibility
set.seed (871139100)

## Randomly split the data into 100 studies
Study <- sample(1:nrow(WVS)) %% k + 1

## Show the sample sizes in the studies
table(Study)

## Append "Study" into the dataset
WVS$Study <- Study

## Function to fit the regression analysis
## y1 to y5: Regression coefficients for A009, A173, C006, X001, and X003.
## v11 to v55: Sampling covariance matrix of the parameter estimates
## try(): Run an analysis and capture the potential errors
fun.reg <- function(dt) { fit <- try(lm(A170~A009+A173+C006+X001+X003, data=dt),
                                     silent=TRUE)

                          ## If there are errors during the analysis, it returns missing values.
                          if (is.element("try-error", class(fit))) {
                            c(y1=NA,y2=NA,y3=NA,y4=NA,y5=NA,
                              v11=NA,v21=NA,v31=NA,v41=NA,v51=NA,
                              v22=NA,v32=NA,v42=NA,v52=NA,v33=NA,
                              v43=NA,v53=NA,v44=NA,v54=NA,v55=NA)
                          } else {
                            ## Extract the regression coefficients by excluding the intercept
                            y <- unname(coef(fit))
                            ## Extract the sampling covariance matrix by excluding the intercept
                            v <- vech(vcov(fit)[-1,-1])
                            c(y1=y[2],y2=y[3],y3=y[4],y4=y[5],y5=y[6],
                                 v11=v[1],v21=v[2],v31=v[3],v41=v[4],v51=v[5],
                                 v22=v[6],v32=v[7],v42=v[8],v52=v[9],v33=v[10],
                                 v43=v[11],v53=v[12],v44=v[13],v54=v[14],v55=v[15])
                          }
}

########## Split the data by "Study" and analyze data with the fun.reg() function on each "Study"
FEM1.reg <- WVS %>% group_by(Study) %>% do(mod=(fun.reg(.)))
FEM1.reg

## Convert it into a data frame
FEM1.reg <- as.data.frame(t(sapply(FEM1.reg$mod, function(x) x)))
## Show part of the results
head(FEM1.reg)

########## Meta-analyze results with a multivariate fixed-effects meta-analysis:
########## Variance component is fixed at 0: RE.constraints=matrix(0, ncol=5, nrow=5)
FEM2.reg <- meta(y=cbind(y1,y2,y3,y4,y5),
                 v=cbind(v11,v21,v31,v41,v51,v22,v32,v42,v52,v33,v43,v53,v44,v54,v55),
                 data=FEM1.reg, RE.constraints=matrix(0, ncol=5, nrow=5),
                 model.name="Regression analysis FEM")
summary(FEM2.reg)

## ---- cache=FALSE--------------------------------------------------------
summary( lm(A170~A009+A173+C006+X001+X003, data=WVS) )

## ---- cache=FALSE--------------------------------------------------------
## Clear all objects in the work space
rm(list=ls())

## Load the data
load("WVS.Rdata")

## Sample sizes of S002 (Wave) and S003 (Country)
## Please refer to http://www.worldvaluessurvey.org/WVSDocumentationWVL.jsp
## for the country names.
table(WVS[, c("S002","S003")])

## Select the relevant variables to minimize memory usage
WVS <- select(WVS, c(A009, A170, A173, C006, X001, X003, S002, S003))

## Reverse coding for A009
## Recode all negative values as NA
## Age (X003) is divided by 10 to improve numerical stability.
WVS <- mutate(WVS,
              A009 = 5-ifelse(A009 < 0, yes=NA, no=A009),
              A170 =   ifelse(A170 < 0, yes=NA, no=A170),
              A173 =   ifelse(A173 < 0, yes=NA, no=A173),
              C006 =   ifelse(C006 < 0, yes=NA, no=C006),
              X001 =   ifelse(X001 < 0, yes=NA, no=X001),
              X003 =   ifelse(X003 < 0, yes=NA, no=X003/10))

## ---- cache=FALSE--------------------------------------------------------
## Function to fit a regression model
## y1 to y5: Regression coefficients from A170, A009, A173, C006, X001, and X003.
## v11 to v55: Sampling covariance matrix of the parameter estimates
fun.reg <- function(dt) { fit <- try(lm(A170~A009+A173+C006+X001+X003, data=dt),
                                     silent=TRUE)

                          ## If there are errors during the analysis, it returns missing values.
                          if (is.element("try-error", class(fit))) {
                              c(y1=NA,y2=NA,y3=NA,y4=NA,y5=NA,
                                v11=NA,v21=NA,v31=NA,v41=NA,v51=NA,
                                v22=NA,v32=NA,v42=NA,v52=NA,v33=NA,
                                v43=NA,v53=NA,v44=NA,v54=NA,v55=NA)
                          } else {
                            ## Extract the regression coefficients excluding the intercept
                              y <- unname(coef(fit))
                              ## Extract the sampling covariance matrix excluding the intercept
                              v <- lav_matrix_vech(vcov(fit)[-1,-1])
                              c(y1=y[2],y2=y[3],y3=y[4],y4=y[5],y5=y[6],
                                v11=v[1],v21=v[2],v31=v[3],v41=v[4],v51=v[5],
                                v22=v[6],v32=v[7],v42=v[8],v52=v[9],v33=v[10],
                                v43=v[11],v53=v[12],v44=v[13],v54=v[14],v55=v[15])
                          }
}

########## Split the data by Wave and Country and analyze with the fun.reg() function
## Set Wave and Country as key variables for fast reference
## S002: Wave (1 to 6)
## S003: Country
REM1.reg <- WVS %>% group_by(S002, S003) %>% do(mod=(fun.reg(.)))

## Convert it into a data frame and append S002 in the dataset
REM1.reg <- data.frame(S002=REM1.reg$S002,
                       as.data.frame(t(sapply(REM1.reg$mod, function(x) x))))

## Show part of the results
head(REM1.reg)

########## Meta-analyze results with a random-effects model
REM2.reg <- meta(y=cbind(y1,y2,y3,y4,y5),
                   v=cbind(v11,v21,v31,v41,v51,v22,v32,v42,v52,
                           v33,v43,v53,v44,v54,v55),
                   data=REM1.reg,
                   model.name="Regression analysis REM")
## Rerun the analysis to remove error code
# REM2.reg <- rerun(REM2.reg)
summary(REM2.reg)

## ------------------------------------------------------------------------
## Set the first wave as 0
REM1.reg$S002 <- REM1.reg$S002-1

REM3.reg <- meta(y=cbind(y1,y2,y3,y4,y5),
                 v=cbind(v11,v21,v31,v41,v51,v22,v32,v42,v52,v33,v43,v53,v44,v54,v55),
                 x=S002, data=REM1.reg,
                 model.name="Wave as a moderator")
summary(REM3.reg)

## ---- echo=FALSE, message=FALSE------------------------------------------
library(metaSEM)
my.model <- "A170 ~ b*A173 + c*A009
             A173 ~ a*A009"
plot(my.model, col="yellow", sizeMan=8, edge.label.cex=2)

## ---- results='hide', cache=FALSE----------------------------------------
## Function to fit a mediation model using sem() function in lavaan,
## where the path coefficients are labelled with "a", "b", and "c."
## y1 and y2: indirect (a*b) and direct effects (c)
## v11, v21, and v22: Sampling covariance matrix of the indirect and direct effects
fun.med <- function(dt) { model.med <- 'A170 ~ b*A173 + c*A009
                                        A173 ~ a*A009
                                        indirect := a*b
                                        direct := c'

                          ## If there are errors during the analysis, it returns missing values.
                          fit <- try(sem(model.med, data=dt), silent=TRUE)

                          if (is.element("try-error", class(fit))) {
                             c(y1=NA,y2=NA,v11=NA,v21=NA,v22=NA)
                          } else {
                             ## y: indirect effect and direct effect
                             y <- unname(fit@Model@def.function(.x.=fit@Fit@x))
                             ## x: all parameter estimates
                             x <- fit@Fit@x
                             ## Variance covariance matrix of the parameter estimates
                             VCOV <- vcov(fit)
                             ## Compute the jacobian for 'defined parameters'
                             JAC <- lavaan:::lavJacobianD(func=fit@Model@def.function, x=x)
                             ## Compute the sampling covariance matrix using delta method
                             v <- JAC %*% VCOV %*% t(JAC)
                             c(y1=y[1],y2=y[2],v11=v[1,1],v21=v[2,1],v22=v[2,2]) 
                         }
}

########## Split the data by Wave and Country and analyze with the fun.med() function
REM1.med <- WVS %>% group_by(S002, S003) %>% do(mod=(fun.med(.)))

## ---- cache=FALSE--------------------------------------------------------
REM1.med <- data.frame(S002=REM1.med$S002,
                       as.data.frame(t(sapply(REM1.med$mod, function(x) x))))
## Show part of the results
head(REM1.med)

########## Meta-analyze results with a random-effects meta-analysis
REM2.med <- meta(y=cbind(y1,y2), v=cbind(v11,v21,v22), data=REM1.med,
                 model.name="Mediation analysis REM")
summary(REM2.med)

## ---- fig.wide=8, fig.height=8, cache=FALSE------------------------------
plot(REM2.med, main="Multivariate meta-analysis",
     axis.label=c("Indirect effect", "Direct effect"),
     study.min.cex=0.6, randeff.ellipse.lty=2,
     randeff.ellipse.lwd=3)

## ---- cache=FALSE--------------------------------------------------------
########## Meta-analyze results with a mixed-effects meta-analysis
## by using "Wave" (S002) as a predictor
REM3.med <- meta(y=cbind(y1,y2), v=cbind(v11,v21,v22), x=S002, data=REM1.med,
                 model.name="Mediation analysis REM")
summary(REM3.med)

## ---- cache=FALSE--------------------------------------------------------
## Clear all objects in the work space
rm(list=ls())

## Load the data
load("WVS.Rdata")

## Select the relevant variables to minimize memory usage
WVS <- select(WVS, c(F114, F115, F116, F117, S002, S003))

## Reverse coding for A009
## Recode all negative values as NA
## Recode all negative values as NA
WVS <- mutate(WVS,
              F114 = ifelse(F114 < 0, yes=NA, no=F114),
              F115 = ifelse(F115 < 0, yes=NA, no=F115),
              F116 = ifelse(F116 < 0, yes=NA, no=F116),
              F117 = ifelse(F117 < 0, yes=NA, no=F117))

## Function to calculate the covariance matrices and sample sizes without any missing data
fun.cov <- function(dt) { 
  my.dt <- dt[, c("F114", "F115", "F116", "F117")] 
  ## Calculate the covariance matrix based on the complete data
  Cov <- try(suppressWarnings(cov(my.dt, use="complete.obs")), silent=TRUE)
  
  ## Calculate the sample sizes based on the complete cases
  n <- length(complete.cases(my.dt))
  
  ## Return NA when there are errors
  if (is.element("try-error", class(Cov))) {
    list(Cov=NA, n=NA)
  } else {
    ## regression coefficients excluding the intercept
    list(Cov=Cov, n=n)
  }
}

########## Split the data by Wave and Country and extract the correlation matrices
########## and sample size with the fun.cor() function
stage0.cov <- WVS %>% group_by(S002, S003) %>% do(mod=(fun.cov(.)))
stage0.cov

## Extract the covariance matrices and sample sizes
data.splitted <- split(stage0.cov$mod, 1:nrow(stage0.cov))
head(data.splitted, n=2)

## A list of covariance matrices
data.cov <- lapply(data.splitted, function(x) x[[1]]$Cov)
head(data.cov, n=2)

## A vector of sample sizes
data.n <- sapply(data.splitted, function(x) x[[1]]$n)
head(data.n)

## Remove groups without any data (n)
index.na <- is.na(data.n)
data.n <- data.n[!index.na]
data.cov <- data.cov[!index.na]

########## Meta-analyze results with the TSSEM random-effects model
## cor.analysis = FALSE: analysis of covariance matrices
REM1.cfa <- tssem1(data.cov, data.n, method="REM", RE.type="Diag",
                   cor.analysis = FALSE)
summary(REM1.cfa)

## Show the pooled covariance matrix
vec2symMat(coef(REM1.cfa, select="fixed"), diag=TRUE)

## Show the variance components of the random effects
Diag(coef(REM1.cfa, select="random"))

## Setup a one-factor CFA model
cfa.model <- "Fraud =~ F114 + F115 + F116 + F117"

plot(cfa.model, col="yellow")

## Convert it into RAM formulation
RAM <- lavaan2RAM(cfa.model, obs.variables = c("F114","F115","F116","F117"))
RAM

########## Fit a one-factor CFA model on the average correlation matrix
REM2.cfa <- tssem2(REM1.cfa, Amatrix=RAM$A, Smatrix=RAM$S, Fmatrix=RAM$F, 
                   model.name="One factor model REM Stage 2 analysis")
summary(REM2.cfa)

## Plot the unstandardized solutions
plot(REM2.cfa, color="yellow")

## Plot the standardized solutions
plot(REM2.cfa, color="green", what="stand")

## ---- cache=FALSE--------------------------------------------------------
## Function to extract coefficient alpha and its sampling variance
## y: estimated coefficient alpha
## v: sampling variance of coefficient alpha
fun.rel <- function(dt) { my.dt <- dt[, c("F114", "F115", "F116", "F117")]
                          ## Calculate the covariance matrix based on the complete data
                          Cov <- try(suppressWarnings(cov(my.dt, use="complete.obs")), 
                                     silent=TRUE)

                          ## Calculate the sample sizes based on the complete cases
                          n <- length(complete.cases(my.dt))

                          if (is.element("try-error", class(Cov))) {
                            c(y=NA,v=NA)
                          } else {
                            if (any(is.na(Cov))) {
                              c(y=NA,v=NA)
                            } else {
                              ## no. of items
                              q <- ncol(Cov)
                              var.item <- sum(diag(Cov))
                              var.scale <- sum(Cov)
                              ## y: coefficient alpha
                              y <- q*(1-var.item/var.scale)/(q-1)
                              ## Bonett (2010, Eq.5)
                              ## v: sampling variance of y 
                              v <- 2*q*(1-y)^2/((q-1)*(n-2))
                              c(y=y,v=v)
                            }
                          }
}

########## Split the data by Wave and Country and analyze data with the fun.rel() function
REM1.rel <- WVS %>% group_by(S002, S003) %>% do(mod=(fun.rel(.)))

## Adjust the scale so that Wave 1 is S002=0.
REM1.rel <- data.frame(S002=REM1.rel$S002-1,
                       as.data.frame(t(sapply(REM1.rel$mod, function(x) x))))
head(REM1.rel)

########## Meta-analyze results with a random-effects meta-analysis
REM2.rel <- meta(y=y, v=v, data=REM1.rel,
                 model.name="Reliability generalization REM")
summary(REM2.rel)

########## Meta-analyze results with a random-effects meta-analysis by using "Wave"" as a predictor
REM3.rel <- meta(y=y, v=v, x=S002, data=REM1.rel,
                 model.name="Reliability generalization REM")
summary(REM3.rel)

## ------------------------------------------------------------------------
sessionInfo()

