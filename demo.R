# Demo
require(devtools)
require(rGithubClient)
require(RCurl)

getFilesList <- function(git, tag=""){
  flist <- git@tree$path
  return(flist[grep(tag, flist)])
}

git <- getRepo('fredcommo/IC50new')
Rlist <- getFilesList(git, '[^Demo].R$')
sourceRepoFile(git, Rlist)

# demo1: data without replicates
url <- getURL("https://raw.github.com/fredcommo/IC50new/master/demo_files/demo1.csv")
dat <- read.delim(text=url)
demo1 <- nplm(dat$x, dat$y)
plot(demo1)
getAUC(demo1)
getEstimates(demo1)

# demo2: data with replicates
url <- getURL("https://raw.github.com/fredcommo/IC50new/master/demo_files/demo2.csv")
dat <- read.delim(text=url)
demo2 <- nplm(dat$x, dat$y)
plot(demo2)
getAUC(demo2)
getEstimates(demo2)

# demo3: proliferation test with replicates
url <- getURL("https://raw.github.com/fredcommo/IC50new/master/demo_files/demo3.csv")
dat <- read.delim(text=url)
demo3 <- nplm(dat$x, dat$y, useLog=FALSE)
plot(demo3, showInf=TRUE, showTarget=FALSE)
getEstimates(demo3)
predict(demo3, .8)
