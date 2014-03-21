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
bottom=0; top=1; xmid=(min(x) + max(x))/2; scal=-1; s=1
x <- seq(log10(1e-3), log10(1.5), len=8)
y <- .PL5(bottom, top, xmid, scal, s, x) + rnorm(length(x), 0, .05)

test <- Logistic(x, y)
test
plot(test)
getEstimates(test)
getPar(test)

# demo2: with replicates and outliers
set.seed(12345)
bottom=-0.1; top=1.2; xmid=(min(x) + max(x))/2; scal=-1.75; s=0.6
x <- rep(seq(log10(1e-3), log10(1.5), len=8), 3)

y <- .PL5(bottom, top, xmid, scal, s, x) + rnorm(length(x), 0, .05)    # add noise
y[2]<- y[2]*.65; y[7]<- y[7] + .4   # add outliers

test <- Logistic(x, y)
test
plot(test)
getEstimates(test)
getPar(test)

# demo3: proliferation curve
set.seed(12346)
x <- rep(seq(1, 10, len=8), 3)
bottom=1; top=100; xmid=(min(x) + max(x))/2; scal=.5; s=0.7

y <- .PL5(bottom, top, xmid, scal, s, x) + rnorm(length(x), 5, 3)

test <- Logistic(x, y, isProp=FALSE, isLog=FALSE)
test
plot(test)
predict(test, .25)
getEstimates(test)
getPar(test)

