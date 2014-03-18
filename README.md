[Example]: https://github.com/fredcommo/IC50new/blob/master/example.png
# IC50
##[Example]

### Compute I50 with weighted np-logistic regressions, using optic densities (ODs) or proportion of control, and drug concentrations. Area under the curve is also estimated using the trapezoide method and the Simpson method.

### Demo

```
require('devtools')
require('rGithubClient')
```
```
getFilesList <- function(git, tag = ''){
  flist <- git@tree$path
  return(flist[grep(tag, flist)])
}

git <- getRepo('fredcommo/IC50new')
Rlist <- getFilesList(git, '[^Demo].R$')
sourceRepoFile(git, Rlist)
```
```
x <- rep(seq(-1, 2, len=8), 3)
bottom=-0.1; top=1.2; xmid=(min(x) + max(x))/2; scal=-1.75; s=1.8

y <- .PL5(bottom, top, xmid, scal, s, x) + rnorm(length(x), 0, .075)
y[2]<- y[2]*.65; y[7]<- y[7] + .4

# Use the default weights method (method="sdw") if there are replicates. Use method="res", otherwise.
test <- model5P(x, y)
test
plot(test)
getEstimates(test)
```
