require(mgcv);require(survival); require(tidyverse)

## First define functions for producing Poisson model data frame

app <- function(x,t,to) {
  ## wrapper to approx for calling from apply...
  y <- if (sum(!is.na(x))<1) rep(NA,length(to)) else
    approx(t,x,to,method="constant",rule=2)$y
  if (is.factor(x)) factor(levels(x)[y],levels=levels(x)) else y
} ## app

tdpois <- function(dat,event="z",et="futime",t="day",status="status1",
                   id="id") {
  ## dat is data frame. id is patient id; et is event time; t is
  ## observation time; status is 1 for death 0 otherwise;
  ## event is name for Poisson response.
  if (event %in% names(dat)) warning("event name in use")
  require(utils) ## for progress bar
  te <- sort(unique(dat[[et]][dat[[status]]==1])) ## event times
  sid <- unique(dat[[id]])
  prg <- txtProgressBar(min = 0, max = length(sid), initial = 0,
                        char = "=",width = NA, title="Progress", style = 3)
  ## create dataframe for poisson model data
  dat[[event]] <- 0; start <- 1
  dap <- dat[rep(1:length(sid),length(te)),]
  for (i in 1:length(sid)) { ## work through patients
    di <- dat[dat[[id]]==sid[i],] ## ith patient's data
    tr <- te[te <= di[[et]][1]] ## times required for this patient
    ## Now do the interpolation of covariates to event times...
    um <- data.frame(lapply(X=di,FUN=app,t=di[[t]],to=tr))
    ## Mark the actual event...
    if (um[[et]][1]==max(tr)&&um[[status]]==1) um[[event]][nrow(um)] <- 1 
    um[[et]] <- tr ## reset time to relevant event times
    dap[start:(start-1+nrow(um)),] <- um ## copy to dap
    start <- start + nrow(um)
    setTxtProgressBar(prg, i)
  }
  close(prg)
  dap[1:(start-1),]
} ## tdpois

## The following takes too long for CRAN checking
## (but typically takes a minute or less).

## Convert pbcseq to equivalent Poisson form...
pbcseq$status1 <- as.numeric(pbcseq$status==2) ## death indicator
pb <- tdpois(pbcseq) ## conversion
pb$tf <- factor(pb$futime) ## add factor for event time



# Try to prepare my data for poisson format ----
load("browseDFs.RData")

## Stripped down version of my saplings_surv df ----

sap_surv_slim <- saplings_surv %>% select(-c(Time, Fate, StDMM:Y_Coord, 
                                             Date, now_browse, 
                                             bareGrnd:orgMat))

sap_surv_slim$Context <- factor(sap_surv_slim$Context)
sap_surv_slim$link <- factor(sap_surv_slim$link)

poisForm_surv <- tdpois(dat = sap_surv_slim, et = "tDeath", 
                        t = 'elapsedDays', status = 'dStatus', id = 'uniqID')

poisForm_surv$tf <- factor(poisForm_surv$tDeath)

## Fit poisson model to my converted dataset

test <- bam(z ~ tf - 1 + s(elapsedDays, k = 3, by = Treat) + 
              Context * Treat +
              Roo_site_mean + 
              Lagomrph_site_mean + 
              s(Site, bs = 're'), 
            family = stats::poisson, data = poisForm_surv, discrete = TRUE,
            nthreads = 2)

par(mfrow=c(2,3))
plot(test,scale=0)

## Fit Poisson model...
b <- bam(z ~ tf - 1 + sex + trt + s(sqrt(protime)) + s(platelet)+ s(age)+
           s(bili)+s(albumin), family=poisson,data=pb,discrete=TRUE,nthreads=2)

par(mfrow=c(2,3))
plot(b,scale=0)

## compute residuals...
chaz <- tapply(fitted(b),pb$id,sum) ## cum haz by subject
d <- tapply(pb$z,pb$id,sum) ## censoring indicator
mrsd <- d - chaz ## Martingale
drsd <- sign(mrsd)*sqrt(-2*(mrsd + d*log(chaz))) ## deviance


cumHaz <- tapply(fitted(test), poisForm_surv$uniqID, sum)
ci <- tapply(poisForm_surv$z, poisForm_surv$uniqID, sum)
nfi <- cumHaz - ci ## Martingale?  r2 dev?
rnfi <- sign(nfi) * sqrt(-2 * (nfi + ci * log(cumHaz))) ## deviance

## plot survivor function and s.e. band for subject 25
te <- sort(unique(pb$futime)) ## event times
di <- pbcseq[pbcseq$id==25,] ## data for subject 25
pd <- data.frame(lapply(X=di,FUN=app,t=di$day,to=te)) ## interpolate to te
pd$tf <- factor(te)
X <- predict(b,newdata=pd,type="lpmatrix")
eta <- drop(X%*%coef(b)); H <- cumsum(exp(eta))
J <- apply(exp(eta)*X,2,cumsum)
se <- diag(J%*%vcov(b)%*%t(J))^.5
plot(stepfun(te,c(1,exp(-H))),do.points=FALSE,ylim=c(0.7,1),
     ylab="S(t)",xlab="t (days)",main="",lwd=2)
lines(stepfun(te,c(1,exp(-H+se))),do.points=FALSE)
lines(stepfun(te,c(1,exp(-H-se))),do.points=FALSE)
rug(pbcseq$day[pbcseq$id==25]) ## measurement times

## plot survivor function and s.e. band for subject 25
rm(teDD,diDD,poisForm_survDD)
eg_id <- '1.T61'
teDD <- sort(unique(poisForm_surv$tDeath)) ## event times
diDD <- sap_surv_slim[sap_surv_slim$uniqID == eg_id, ] ## data for eg_id
poisForm_survDD <- data.frame(lapply(X=diDD,FUN=app,t=diDD$elapsedDays,to=teDD)) ## interpolate to te
poisForm_survDD$tf <- factor(teDD)
X <- predict(test, newdata=poisForm_survDD, type="lpmatrix")
eta <- drop(X%*%coef(test)) 
H <- cumsum(exp(eta))
J <- apply(exp(eta)*X, 2, cumsum)
se <- diag(J%*%vcov(test)%*%t(J))^.5
plot(stepfun(teDD,c(1,exp(-H))),do.points=FALSE,ylim=c(0,1),
     ylab="S(t)",xlab="t (days elapsed)",main="",lwd=2)
lines(stepfun(teDD,c(1,exp(-H+se))),do.points=FALSE)
lines(stepfun(teDD,c(1,exp(-H-se))),do.points=FALSE)
rug(sap_surv_slim$elapsedDays[sap_surv_slim$uniqID == eg_id ]) ## measurement times


## plot survivor function and s.e. band for treatment group
rm(teDD,diDD,poisForm_survDD)
choice <- 'Total'
te <- sort(unique(poisForm_surv$tDeath)) ## event times
diDD <- sap_surv_slim[sap_surv_slim$Treat == choice, ] ## data for eg_id
poisForm_survDD <- data.frame(lapply(X=diDD,FUN=app,t=diDD$elapsedDays,to=teDD)) ## interpolate to te
poisForm_survDD$tf <- factor(teDD)
X <- predict(test, newdata=poisForm_survDD, type="lpmatrix")
eta <- drop(X%*%coef(test)) 
H <- cumsum(exp(eta))
J <- apply(exp(eta)*X, 2, cumsum)
se <- diag(J%*%vcov(test)%*%t(J))^.5
plot(stepfun(teDD,c(1,exp(-H))),do.points=FALSE,ylim=c(0,1),
     ylab="S(t)",xlab="t (days elapsed)",main="",lwd=2)
lines(stepfun(teDD,c(1,exp(-H+se))),do.points=FALSE)
lines(stepfun(teDD,c(1,exp(-H-se))),do.points=FALSE)
rug(sap_surv_slim$elapsedDays[sap_surv_slim$uniqID == eg_id ]) ## measurement times
#par(mfrow=c(1,3))
