# How to insert tex code math equation into R help file with some hacks

## the Rd file doesn't support tex code 
https://stackoverflow.com/questions/14041601/documenting-equations-with-deqn-and-roxygen

## so here is the steps to hack:

#### Step 1:  generate the Rd file through roxygen2::roxygenise()
#### Step 2:  go to https://www.codecogs.com/eqnedit.php to type in the equation with tex code
#### Step 3:  copy the html (Edit) source code from the buttom of the page
#### Step 4:  replace tag "a" with tag "center" in the copied html code
#### Step 5:  paste the html code into the Rd file, for example, 
```
\details{
The robust EWMA mean algorithm has the form
\ifelse{html}{\out{

<center href="https://www.codecogs.com/eqnedit.php?latex=\hat{\mu}_t&space;=&space;\hat{\mu}_{t-1}&space;&plus;&space;(1-\lambda)\hat{\sigma}_{t-1}\psi_{\texttt{hub}}&space;\left(\frac{x_t-\hat{\mu}_{t-1}}{\hat{\sigma}_{t-1}}\right)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\hat{\mu}_t&space;=&space;\hat{\mu}_{t-1}&space;&plus;&space;(1-\lambda)\hat{\sigma}_{t-1}\psi_{\texttt{hub}}&space;\left(\frac{x_t-\hat{\mu}_{t-1}}{\hat{\sigma}_{t-1}}\right)" title="\hat{\mu}_t = \hat{\mu}_{t-1} + (1-\lambda)\hat{\sigma}_{t-1}\psi_{\texttt{hub}} \left(\frac{x_t-\hat{\mu}_{t-1}}{\hat{\sigma}_{t-1}}\right)" /></center>

}
}
```

## this hack depends on the codecogs.com's tex 2 html engine