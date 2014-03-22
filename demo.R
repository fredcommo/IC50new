# Demo
require('devtools')
require('rGithubClient')

getFilesList <- function(git, tag = ''){
  flist <- git@tree$path
  return(flist[grep(tag, flist)])
}

git <- getRepo('fredcommo/IC50new')
Rlist <- getFilesList(git, '[^Demo].R$')
sourceRepoFile(git, Rlist)

# demo1: no replicate
set.seed(12345)
n <- 8
x <- sapply(seq(-3, 1, len=8), function(p) 10^(p))
bottom=0; top=1; xmid=(min(log10(x)) + max(log10(x)))/2; scal=-1; s=1
y <- .PL5(bottom, top, xmid, scal, s, log10(x)) + rnorm(n, 0, .05)

test <- Logistic(x, y)
test
plot(test)
getEstimates(test)
getPar(test)

# demo2: with replicates and outliers
set.seed(12345)
n = 8; replic = 3
x <- sapply(seq(-3, 1, len=n), function(p) 10^(p))
x <- rep(x, replic)
bottom=0; top=1; xmid=(min(log10(x)) + max(log10(x)))/2; scal=-1; s=1

y <- .PL5(bottom, top, xmid, scal, s, log10(x)) + rnorm(n*replic, 0, .1)
y[2]<- y[2]*.65; y[7]<- y[7] + .4

test <- Logistic(x, y)
test
plot(test)
getEstimates(test)
getPar(test)

# demo3: proliferation curve
set.seed(12346)
n = 8; replic = 3
x <- rep(seq(1, 48, len=n), replic)
bottom=1; top=100; xmid=(min(x) + max(x))/2; scal=.1; s=.5

y <- .PL5(bottom, top, xmid, scal, s, x) + rnorm(n*replic, 5, 6)

test <- Logistic(x, y, isProp=FALSE, useLog=FALSE)
test
plot(test, xlab = "Time (hrs)", ylab = "Proliferation", showInfl=TRUE)
predict(test, .25)
getEstimates(test)
getPar(test)

plot(getXcurve(test), getYcurve(test))
pars <- getPar(test)
abline(v=pars$params$xmid)