# Demo
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

x <- rep(seq(-1, 2, len=8), 3)
bottom=-0.1; top=1.2; xmid=(min(x) + max(x))/2; scal=-1.75; s=1.8

y <- .PL5(bottom, top, xmid, scal, s, x) + rnorm(length(x), 0, .075)
y[2]<- y[2]*.65; y[7]<- y[7] + .4

test <- model5P(x, y)
test
plot(test)
getEstimates(test)
