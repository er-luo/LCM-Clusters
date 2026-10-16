title: "Simple simulation example"
output: pdf_document
~~~
```{r setup, include=FALSE, message=F, warning=F}
knitr::opts_chunk$set(echo = TRUE)
```

I thought I'd prepare a brief R markdown file about simulating
data and simulation studies. 

Let's start with a little about simulation studies.  They can be
useful for many things, but they also have some significant
limitations. First, consider some strengths. Let's say you've
written a program to estimate some quantity. Simulation studies
can help verify that you've programmed your algorithm or estimation
routine properly, or stated otherwise, they can help you find bugs
in your code. Additionally, simulation studies can be used to 
examine what happens when some approach's assumptions are violated.

However, this last example also points to simulation studies' biggest
limitation. When you simulate some data and show something or other,
you've only demonstrated it on data with that exact structure. It is
possible that alterations to your simulation could change your conclusions.
Thus, we need to be really careful not to overstate what our simulation
study does or does not show.

Let's create two correlated variables, $x$ and $y$, as a simple
starting example. The 'set.seed()' command provides the number that
is used to start the random number generation in R. 

Let's have a quick example of that. The 'runif()' command is one
of the most basic random number generation commands in R. It generates
a random value from a uniform distribution over the $(0,1)$ interval.
A uniform distribution is one in which every value has the same
probability of selection. So the first 'runif()' below returns three
values between zero and one. Note I didn't set the seed, so I just
got the values connected with whatever seed value my R session had
from a previous random number generation task. Or it grabbed a seed
from some other source if it couldn't find one from me. The R documentation
and code would explain exactly what is done.

Note when we run 'runif()' again, we get three different values. This
is working as it should, because again I didn't set the seed. Note that in the
following two 'runif()' calls, we get the same values because the same
startind seed was used each time.

```{r}
runif(3)
runif(3)
set.seed(41)
runif(3)
set.seed(41)
runif(3)
```

Playing with random numbers in R is fine without setting a seed, or
when you are working out the details and code of a
simulation. However, whenever you run something that you want to keep
or use as real work, set a seed at the start. This way, anyone who
uses the same seed and R version should be able to reproduce your
simulation exactly.

Here's a small simulation example. Generate two variables, one ($x$)
is drawn from a uniform distribution and the other is a function of
$x$ with an added error from a normal distribution, $N(0, 0.09)$.

```{r}
set.seed(41)
x <- runif(n = 500)
y <- 1 + .5*x + rnorm(n = 500, sd = .3)
cor(x,y)
plot(x,y)
```

Let's do an even simpler one. Generate two uncorrelated normally
distributed variables and calculate the correlation.

```{r}
set.seed(11)
x <- rnorm(n = 100)
y <- rnorm(n = 100)
cor(x,y)
```

Doing this once, gives us one estimated correlation. When we do a
simulation study, we typically know what the right answer is. In this
case, the right answer is zero, because we generated the two variables
without any association or dependence between them. (The example above
calculated $y$ based on $x$, so we know those are not statistically 
independent.)

It is unlikely any single simulated correlation will be exactly or even
very, very near zero. There's error in the estimate due to sampling.
(Remember the sampling distribution?) But if we do it 1000 times, 
if we average all those 1000 correlation estimates, we should get something
very close to the population value (zero, in this case), if we use
an unbiased estimator. (Don't worry about that, if you don't know what
that means, unless you want to know what it means.)

```{r}
set.seed(11)
tmpvec <- rep(NA, 1000)
for(i in 1:1000) {
    x <- rnorm(n = 100, mean = 0, sd = 1)
    y <- rnorm(n = 100, mean = 0, sd = 1)
    tmpvec[i] <- cor(x,y)
}
mean(tmpvec)
var(tmpvec)
sd(tmpvec)
rm(tmpvec, i, x, y)
```

Just for fun, what is the variance calculated at the end? Can you 
tell what it should be analytically? (This is just for your interest,
it isn't a quiz!)

The above is very simple. We could expand it to calculate confidence
intervals for each estimate and then calculate the percent of times
the confidence interval does not include zero. (Again, if you want
to think about it, what percentage should that be, if we use an
$\alpha = 0.05$?)
