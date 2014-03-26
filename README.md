[newloc]: https://github.com/fredcommo/nplogistic
> This repository will not be updated: see [newloc] for new developments.

[Example]: https://github.com/fredcommo/IC50new/blob/master/example.png
# IC50 
##[Example]

---
### Compute weighted np-logistic regressions for fitting drug response curves, or proliferations. See demos.
---
_The main function is nplm(x, y, ...), where:_  
>- x : is a vector of drug concentrations, time...  
>- y : is a vector of proportions (vs control)

---
_Other arguments (all have default values and can be omited):_
>- T0: the experiment value at T0. Used if isProp=FALSE. See details.  
>- Ctrl: the control value at T0. Used if isProp=FALSE. See details.  
>- isProp : y is provided as proportion values, from 0 to 1. Default is TRUE, proportions will be computed otherwise. See details.  
>- useLog : x values will log10 transfromed. Default is TRUE.  
>- LPweight : a weights-coefficient, default = .25. See details  
>- npars : number of parameters to use is the model. Possible values are "all" or any integer from 2 to 5. If "all" (default), parameters from 2 to 5 will be tested and the best model will be considered. See details.  
>- method : the weighted method "res" (default), "sdw", "Y2", "pw", "gw"  
>- B : number of simulations to estimate the 95% intervals for the predicted x values. Default, B=1e4  
>- ... : other graphical parameters.  

---
### Details
> - The logistic model is of the form:  
<img src="http://latex.codecogs.com/gif.latex?y=B&plus;&space;\frac{T&space;-&space;B}{[1&space;&plus;&space;10^{s.(c&space;-&space;x)}]^p}" title="y=B+ \frac{T - B}{[1 + 10^{s.(c - x)}]^p}" />  
> where _B_, _T_ are the bottom and top asymptotes, _b_ and _c_ are the slope an dthe x-coordinate at the inflexion point, and _p_ is an assymetric parameter. All these 5 parameters are sumultaneously optimzed using a _Newton_ algorithm (nlm R package).  
> When _npars_ is specified, some paramaters are forced to take a fixed value:  
> npars = 4, _p_ is forced to be 1.  
> npars = 3, _B_ is forced to be 0.  
> npars = 2, _T_ is forced to be 1.  

> - Proportions computation:  
> If _y_ is not provided as a vector of proportions (_isProp=FALSE_), proportions are calculated as follow:  
> <img src="http://latex.codecogs.com/gif.latex?resp_i&space;=&space;\frac{y_i&space;-&space;T_0}{Ctr&space;-&space;T_0}" title="resp_i = \frac{y_i - T_0}{Ctr - T_0}" />  
> where _T0_ and _Ctrl_ should be the observed values at _T=0_ and in the control condition, respectively. If one or both of these are _NA_, proportions will be roughly estimated as:  
<img src="http://latex.codecogs.com/gif.latex?resp_i&space;=&space;\frac{y_i}{max(y_i)}" title="resp_i = \frac{y_i}{max(y_i)}" />  
> - The weighted methods  
> Weighted square error are used as the objective function for optimizing the parameters.  
> The "_res_" method :  
<img src="http://latex.codecogs.com/gif.latex?err&space;=&space;\sum_i{\frac{(\hat{y_i}-y_i)^2}{(\hat{y_i}-y_i)^2^{w_i}}}" title="err = \sum_i{\frac{(\hat{y_i}-y_i)^2}{(\hat{y_i}-y_i)^2^{w_i}}}" />  
> The "_sdw_" method :  
<img src="http://latex.codecogs.com/gif.latex?err&space;=&space;\sum_{i=1}^{r}\frac{1}{\sigma_i}{\sum_{j=1}^{n}{(\hat{y}_i_j-y_i_j)^2}}" title="err = \sum_{i=1}^{r}\frac{1}{\sigma_i}{\sum_{j=1}^{n}{(\hat{y}_i_j-y_i_j)^2}}" />  


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
