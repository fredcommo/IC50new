[Example]: https://github.com/fredcommo/IC50new/blob/master/example.png
# IC50
##[Example]

### Compute weighted np-logistic regressions for fitting drug response curves, or proliferations. See demos.

The main function is nplm(x, y, ...), where:
x : is a vector of drug concentrations, time...
y : a vector of proportion (vs control)
T0: the experiment value at T0. Used if isProp=FALSE. See details.
Ctrl: the control value at T0. Used if isProp=FALSE. See details.
isProp : y is provided as proportion values, from 0 to 1. Default is TRUE, proportions will be computed otherwise. See details.
useLog : x values will log10 transfromed. Default is TRUE.
LPweight : a weights-coefficient, default = .25. See details
npars : number of parameters to use is the model. Possible values are "all" or any integer from 2 to 5. If "all" (default), parameters from 2 to 5 will be tested and the best model will be considered. See details.
method : the weighted method "res" (default), "sdw", "Y2", "pw", "gw"
B : number of simulations to estimate the 95% intervals. Default, B=1e4
...: other graphical parameters.

### Demo

```
require(devtools)
require(rGithubClient)
require(RCurl)
```
```
getFilesList <- function(git, tag=""){
  flist <- git@tree$path
  return(flist[grep(tag, flist)])
}

git <- getRepo('fredcommo/IC50new')
Rlist <- getFilesList(git, '[^Demo].R$')
sourceRepoFile(git, Rlist)
```

```
# demo1: data without replicates
url <- getURL("https://raw.github.com/fredcommo/IC50new/master/demo_files/demo1.csv")
dat <- read.delim(text=url)
demo1 <- nplm(dat$x, dat$y)
plot(demo1)
getAUC(demo1)
getEstimates(demo1)
```

```
# demo2: data with replicates
url <- getURL("https://raw.github.com/fredcommo/IC50new/master/demo_files/demo2.csv")
dat <- read.delim(text=url)
demo2 <- nplm(dat$x, dat$y)
plot(demo2)
getAUC(demo2)
getEstimates(demo2)
```

```
# demo3: proliferation test with replicates
url <- getURL("https://raw.github.com/fredcommo/IC50new/master/demo_files/demo3.csv")
dat <- read.delim(text=url)
demo3 <- nplm(dat$x, dat$y, useLog=FALSE)
plot(demo3, showInf=TRUE, showTarget=FALSE)
getEstimates(demo3)
predict(demo3, .8)
```
