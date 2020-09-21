## FUNCTIONS ##

# Weibull log-likelihood
log_lik_weibull <- function(data, param)
    {
        -sum(dweibull(data, shape = param[1], scale = param[2], log = TRUE))
    }



## EXERCISE ##

# data
y <- c(155.9, 200.2, 143.8, 150.1,152.1, 142.2, 147, 146, 146, 170.3, 148, 140, 118, 144, 97)
n <- length(y)


# define parameters grid
gamma <- seq(0.1, 15, length=100)
beta <- seq(100,200, length=100)
parvalues <- expand.grid(gamma,beta)

llikvalues <- apply(parvalues, 1, log_lik_weibull, data=y)
llikvalues <- matrix(-llikvalues, nrow=length(gamma), ncol=length(beta), byrow=F)

conf.levels <- c(0,0.5,0.75,0.9,0.95,0.99)


# estimate gamma and beta
gammahat<-uniroot(function(x) n/x+sum(log(y))-n*
                      sum(y^x*log(y))/sum(y^x),
                  c(1e-5,15))$root
betahat<- mean(y^gammahat)^(1/gammahat)
weib.y.mle<-c(gammahat,betahat)

# first element is the MLE for the shape gamma, second element the MLE for the scale beta
weib.y.mle



# observed information matrix
jhat<-matrix(NA,nrow=2,ncol=2)
jhat[1,1]<-n/gammahat^2+sum((y/betahat)^gammahat*
                                (log(y/betahat))^2)
jhat[1,2]<-jhat[2,1]<- n/betahat-sum(y^gammahat/betahat^(gammahat+1)*
                                         (gammahat*log(y/betahat)+1))
jhat[2,2]<- -n*gammahat/betahat^2+gammahat*(gammahat+1)/
    betahat^(gammahat+2)*sum(y^gammahat)
solve(jhat)


# quadratic approx
log_lik_qdiff <- function(data, param)
    {
        diffterm <- (param - weib.y.mle)

        # Note that in Taylor expansion around MLE-estimates:
        # - 0th order term is a fixed constant;
        # - 1st order term is 0.
        #
        # For that reason, 2nd+ order terms only account for the relative log-likelihood computation.

        -0.5*t(diffterm)%*%jhat%*%diffterm
    }


# log_lik
c_llikvalues <- apply(parvalues, 1, log_lik_qdiff, data=y)
c_llikvalues <- matrix(c_llikvalues, nrow=length(gamma), ncol=length(beta), byrow=F)


# contour plot
contour(gamma, beta, c_llikvalues,
        levels=-qchisq(conf.levels, 2)/2,
        xlab=expression(gamma),
        labels=as.character(conf.levels),
        ylab=expression(beta))
title('Weibull relative log likelihood (quadratic approx.)')
