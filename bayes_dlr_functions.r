# Fitting Empirical Bayes beta-binomial models
# Author: Josh Betz (https://github.com/jbetz-jhu)

# distribution function for prevalence ratios from a beta distribution
d.pr <-
  function(x, shape1, shape2, r) {
    dbeta(x = (x*r)/(1 + (x*r)),
          shape1 = shape1, shape2 = shape2)*(r/((r*x) + 1)^2)
  }




# distribution function for log prevalence ratios from a beta distribution
d.log.pr <-
  function(x, shape1, shape2, r) {
    dbeta(x = (r*exp(x))/(1 + (r*exp(x))),
          shape1 = shape1, shape2 = shape2)*(r*exp(x)/(1 + r*exp(x))^2)
  }




# ll.mbb - log Marginal Likelihood: Beta Binomial
ll.mbb <-
  function(
    y,
    t,
    mu,
    m
  ) {
    lgamma(t + 1) + lgamma(y + m*mu) + lgamma(t - y + (1 - mu)*m) +
      lgamma(mu*m + (1 - mu)*m) - lgamma(y + 1) - lgamma(t - y + 1) -
      lgamma(t + m) - lgamma(mu*m) - lgamma((1 - mu)*m)
  }




# mbb - Marginal Likelihood: Beta Binomial
mbb <-
  function(
    y,
    t,
    mu,
    m
  ) {
    exp(ll.mbb(y = y, t = t, mu = mu, m = m))
  }




# ll.mbb.sum - sum of log Marginal Likelihood: Beta Binomial
ll.mbb.sum <-
  function(y, t, mu, m){
    sum(ll.mbb(y = y, t = t, mu = mu, m = m))
  }




# ll.mbb.mix - log Marginal Likelihood: Beta Binomial Mixture
ll.mbb.mix <-
  function(y, t, mu.1, mu.2, m.1, m.2, epsilon) {
    log(((1 - epsilon)*mbb(y = y, t = t, mu = mu.1, m = m.1) +
           epsilon*mbb(y = y, t = t, mu = mu.2, m = m.2)))
  }




# ll.mbb.sum.mix - sum of log Marginal Likelihood: Beta Binomial Mixture
ll.mbb.sum.mix <-
  function(y, t, mu.1, mu.2, m.1, m.2, epsilon) {
    sum(ll.mbb.mix(y = y, t = t, mu.1 = mu.1, mu.2 = mu.2, 
                   m.1 = m.1, m.2 = m.2, epsilon = epsilon))
  }




# Method of Moments Estimator
bb.prevalence.ratio.mm <-
  function(data, y.0, n.0, y.1, n.1,
           id = NULL,
           posterior.quantiles = c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5,
                                   0.75, 0.9, 0.95, 0.975, 0.99),
           n.mc = 1e6) {
    
    results <-
      data.frame(
        id = 
          if(is.null(id)){
            1:nrow(data)
          } else{ 
            data[, id]
          },
        setNames(
          object = data[, c(y.0, n.0, y.1, n.1)],
          nm = c("y.0", "n.0", "y.1", "n.1")
        ),
        matrix(
          nrow = nrow(data),
          ncol = length(posterior.quantiles),
          dimnames = 
            list(NULL, 
                 paste0("gamma.mm_pq.",
                        posterior.quantiles)
            )
        ),
        gamma.mm_prior_mean = NA,
        gamma.mm_prior_variance = NA,
        gamma.mm_posterior_mean = NA,
        gamma.mm_posterior_variance = NA,
        check.names = FALSE
      )
    
    results$t.k <- with(results, y.1 + y.0)
    results$r.k <- with(results, n.1/ n.0)
    results$theta.k.hat <- with(results, y.1/t.k)
    
    if(sum(results$t.k < 1) > 0) 
      stop("Either y.0 or y.1 must be greater than 0.")
    
    mu.mm <- with(results, sum(y.1)/sum(t.k))
    s.squared <- with(results, mean(t.k*(theta.k.hat - mu.mm)^2))
    a <- s.squared/(mu.mm*(1-mu.mm))
    m.mm <- (mean(results$t.k) - a)/(a - 1)
    
    alpha.mm <- mu.mm*m.mm
    beta.mm <- (1-mu.mm)*m.mm
    
    if(mu.mm > 1 | mu.mm < 0 | !is.finite(m.mm) | m.mm < 0 |
       alpha.mm < 0 | beta.mm < 0){
      alpha.mm <- beta.mm <-
        prior.mean.mm <- prior.variance.mm <- 
        theta.prior.mean.mm <- theta.prior.variance.mm <- NaN
      warning(paste0("MM estimator does not exist or is degenerate"))
    } else {
      theta.prior.mean.mm <- alpha.mm/(alpha.mm + beta.mm)
      theta.prior.variance.mm <-
        (alpha.mm*beta.mm)/((alpha.mm + beta.mm + 1)*(alpha.mm + beta.mm)^2)
      
      if (beta.mm > 1) {
        results$gamma.mm_prior_mean <- (1/results$r.k)*(alpha.mm/(beta.mm - 1))
      } else {
        results$gamma.mm_prior_mean <- NaN
      }
      
      if (beta.mm > 1) {
        results$gamma.mm_prior_variance <- 
          (1/results$r.k^2)*(alpha.mm*(alpha.mm + beta.mm - 1))/((beta.mm - 2)*(beta.mm - 1)^2)
        
      } else {
        results$gamma.mm_prior_variance <- NaN
      }
      
      for (i in 1:nrow(results)){
        alpha.mm.posterior <- alpha.mm + results$y.1[i]
        beta.mm.posterior <- beta.mm + results$y.0[i]
        
        mm.posterior.theta <-
          rbeta(n = n.mc,
                shape1 = alpha.mm.posterior, shape2 = beta.mm.posterior)
        
        mm.posterior.gamma <-
          (1/results$r.k[i])*(mm.posterior.theta/(1 - mm.posterior.theta))
        
        
        results[i, paste0("gamma.mm_pq.", posterior.quantiles)] <-
          quantile(x = mm.posterior.gamma,
                   probs = posterior.quantiles)
        
        if (beta.mm.posterior > 1) {
          results$gamma.mm_posterior_mean[i] <- 
            (1/results$r.k[i])*(alpha.mm.posterior/(beta.mm.posterior - 1))
        } else {
          results$gamma.mm_posterior_mean[i] <-  NaN
        }
        
        if (beta.mm.posterior > 2) {
          results$gamma.mm_posterior_variance[i] <-
            (1/results$r.k[i]^2)*(alpha.mm.posterior*(alpha.mm.posterior + beta.mm.posterior - 1))/
            ((beta.mm.posterior - 2)*(beta.mm.posterior - 1)^2)
          
        } else {
          results$gamma.mm_posterior_variance[i] <-  NaN
        }
      }
    }
    
    return(
      list(
        results = results,
        mu.mm = mu.mm,
        s.squared = s.squared,
        a = a,
        m.mm = m.mm,
        alpha.mm = alpha.mm,
        beta.mm = beta.mm,
        theta.prior.mean.mm = theta.prior.mean.mm,
        theta.prior.variance.mm = theta.prior.variance.mm
      )
    )
  }




# Marginal Maximum Likelihood Estimator
bb.prevalence.ratio.mml <-
  function(
    data, y.0, n.0, y.1, n.1, id = NULL,
    posterior.quantiles = c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5,
                            0.75, 0.9, 0.95, 0.975, 0.99),
    n.mc = 1e6,
    verbose = FALSE
  ) {
    
    results <-
      data.frame(
        id = 
          if(is.null(id)){
            1:nrow(data)
          } else{ 
            data[, id]
          },
        setNames(object = data[, c(y.0, n.0, y.1, n.1)],
                 nm = c("y.0", "n.0", "y.1", "n.1")),
        matrix(nrow = nrow(data),
               ncol = length(posterior.quantiles),
               dimnames = 
                 list(NULL, 
                      paste0("gamma.mml_pq.",
                             posterior.quantiles))),
        gamma.mml_prior_mean = NA,
        gamma.mml_prior_variance = NA,
        gamma.mml_posterior_mean = NA,
        gamma.mml_posterior_variance = NA,
        check.names = FALSE
      )
    
    posterior.theta <- list()
    posterior.gamma <- list()
    
    results$t.k <- with(results, y.1 + y.0)
    results$r.k <- with(results, n.1/ n.0)
    
    if(sum(results$t.k < 1) > 0) 
      stop("Either y.0 or y.1 must be greater than 0.")
    
    mml.solution <- 
      optimx::optimx(
        par = c(mu = 0.5, m = 1),
        fn = function(x, y, t) {
          ll.mbb.sum(
            y = y,
            t = t,
            mu = x["mu"],
            m = x["m"]
          )
        },
        upper = c(1 - sqrt(.Machine$double.eps), Inf),
        lower = c(sqrt(.Machine$double.eps), sqrt(.Machine$double.eps)),
        y = results$y.1,
        t = results$t.k,
        method = c("L-BFGS-B", "nlminb"),
        control = 
          list(
            maximize = TRUE,
            all.methods = FALSE,
            trace = 0
          )
      )
    
    converged.solutions <-
      subset(mml.solution, convcode == 0)
    
    if(nrow(converged.solutions) > 0) {
      solution <-
        converged.solutions[which.max(converged.solutions$value),]
      
      mu.mml <- solution$mu
      m.mml <- solution$m
      
      alpha.mml <- mu.mml*m.mml
      beta.mml <- (1-mu.mml)*m.mml
      
      theta.prior.mean.mml <- alpha.mml/(alpha.mml + beta.mml)
      theta.prior.variance.mml <-
        (alpha.mml*beta.mml)/((alpha.mml + beta.mml + 1)*(alpha.mml + beta.mml)^2)
      
      if (beta.mml > 1) {
        results$gamma.mml_prior_mean <- (1/results$r.k)*(alpha.mml/(beta.mml - 1))
      } else {
        results$gamma.mml_prior_mean <- NaN
      }
      
      if (beta.mml > 1) {
        results$gamma.mml_prior_variance <- 
          (1/results$r.k^2)*(alpha.mml*(alpha.mml + beta.mml - 1))/((beta.mml - 2)*(beta.mml - 1)^2)
        
      } else {
        results$gamma.mml_prior_variance <- NaN
      }
      
      for (i in 1:nrow(results)){
        alpha.mml.posterior <- alpha.mml + results$y.1[i]
        beta.mml.posterior <- beta.mml + results$y.0[i]
        
        mml.posterior.theta <-
          rbeta(n = n.mc,
                shape1 = alpha.mml.posterior, shape2 = beta.mml.posterior)
        
        mml.posterior.gamma <-
          (1/results$r.k[i])*(mml.posterior.theta/(1 - mml.posterior.theta))
        
        posterior.theta[[i]] <-
          setNames(
            object = data.frame(mml.posterior.theta),
            nm = as.character(data[i, id])
          )
        
        posterior.gamma[[i]] <-
          setNames(
            object = data.frame(mml.posterior.gamma),
            nm = as.character(data[i, id])
          )
        
        results[i, paste0("gamma.mml_pq.", posterior.quantiles)] <-
          quantile(x = mml.posterior.gamma,
                   probs = posterior.quantiles)
        
        if (beta.mml.posterior > 1) {
          results$gamma.mml_posterior_mean[i] <- 
            (1/results$r.k[i])*(alpha.mml.posterior/(beta.mml.posterior - 1))
        } else {
          results$gamma.mml_posterior_mean[i] <-  NaN
        }
        
        if (beta.mml.posterior > 2) {
          results$gamma.mml_posterior_variance[i] <-
            (1/results$r.k[i]^2)*(alpha.mml.posterior*(alpha.mml.posterior + beta.mml.posterior - 1))/
            ((beta.mml.posterior - 2)*(beta.mml.posterior - 1)^2)
        } else {
          results$gamma.mml_posterior_variance[i] <-  NaN
        }
      }
      
      posterior.theta <- do.call(what = cbind, args = posterior.theta)
      posterior.gamma <- do.call(what = cbind, args = posterior.gamma)
      
      if(verbose){
        return(
          list(
            results = results,
            mu.mml = mu.mml,
            m.mml = m.mml,
            alpha.mml = alpha.mml,
            beta.mml = beta.mml,
            theta.prior.mean.mml = theta.prior.mean.mml,
            theta.prior.variance.mml = theta.prior.variance.mml,
            mml.solution = mml.solution,
            posterior.theta = posterior.theta,
            posterior.gamma = posterior.gamma
          )
        )
      } else {
        return(
          list(
            results = results,
            mu.mml = mu.mml,
            m.mml = m.mml,
            alpha.mml = alpha.mml,
            beta.mml = beta.mml,
            theta.prior.mean.mml = theta.prior.mean.mml,
            theta.prior.variance.mml = theta.prior.variance.mml
          )
        )
      }
    } else {
      return(mml.solution)
      warning("No methods converged at a solution. ",
              "Check the optimx object for more information.")
    }
  }




# For a given starting value of the mixture parameters, find the MML estimate.
bb.2c.mixture.optimize <-
  function(
    data, y.0, n.0, y.1, n.1, id = NULL,
    mu.1, mu.2, m.1, m.2, epsilon,
    max.iterations = 1e2,
    tolerance = 1e-8,
    verbose = FALSE
  ) {
    
    results <-
      data.frame(
        id = 
          if(is.null(id)){
            1:nrow(data)
          } else{ 
            data[, id]
          },
        setNames(
          object = data[, c(y.0, n.0, y.1, n.1)],
          nm = c("y.0", "n.0", "y.1", "n.1")
        )
      )
    
    results$t.k <- with(results, y.1 + y.0)
    results$r.k <- with(results, n.1/n.0)
    
    if(sum(results$t.k < 1) > 0) 
      stop("Either y.0 or y.1 must be greater than 0.")
    
    converged <-  FALSE
    j = 1
    
    ll <- rep(NA_real_, length = (max.iterations + 1))
    par.trace <- list()
    
    ll[[j]] <- 
      ll.mbb.sum.mix(
        y = results$y.1,
        t = results$t.k,
        mu.1 = mu.1,
        mu.2 = mu.2,
        m.1 = m.1,
        m.2 = m.2,
        epsilon = epsilon
      )
    
    params <-
      list(
        mu.1 = mu.1,
        mu.2 = mu.2,
        m.1 = m.1,
        m.2 = m.2,
        epsilon = epsilon,
        ll.1 = NA,
        ll.2 = NA
      )
    
    while(converged == FALSE & j <= max.iterations) {
      par.trace[[j]] <- params
      
      m.step <-
        optimx::optimx(
          par = 
            c(inv.logit.mu.1 = qlogis(params[["mu.1"]]),
              inv.logit.mu.2 = qlogis(params[["mu.2"]]),
              log.m.1 = log(params[["m.1"]]),
              log.m.2 = log(params[["m.2"]]),
              inv.logit.epsilon = qlogis(params[["epsilon"]])
            ),
          fn = function(mix.params, y, t){
            ll.mbb.sum.mix(
              y = y,
              t = t,
              mu.1 = plogis(mix.params["inv.logit.mu.1"]),
              mu.2 = plogis(mix.params["inv.logit.mu.2"]),
              m.1 = exp(mix.params["log.m.1"]),
              m.2 = exp(mix.params["log.m.2"]),
              epsilon = plogis(mix.params["inv.logit.epsilon"])
            )
          },
          method = c("L-BFGS-B", "nlminb", "bobyqa"),
          y = results$y.1,
          t = results$t.k,
          upper = c( 15,  15,  15,  15, 0),
          lower = c(-15, -15, -15, -15, -15),
          control =
            list(
              maximize = TRUE,
              trace = 0
            )
        )
      
      m.step.converged <- subset(m.step, convcode == 0)
      if(nrow(m.step.converged) < 1) {
        stop("No methods converged at a solution. ",
             "Check the optimx object for more information.")
      }
      
      m.step.converged <- 
        m.step.converged[which.max(m.step.converged$value),]
      
      params["mu.1"] <- plogis(m.step.converged$inv.logit.mu.1)
      params["mu.2"] <- plogis(m.step.converged$inv.logit.mu.2)
      params["m.1"] <- exp(m.step.converged$log.m.1)
      params["m.2"] <- exp(m.step.converged$log.m.2)
      params["epsilon"] <- plogis(m.step.converged$inv.logit.epsilon)
      
      ll[[j + 1]] <-
        ll.mbb.sum.mix(
          y = results$y.1,
          t = results$t.k,
          mu.1 = params[["mu.1"]],
          mu.2 = params[["mu.2"]],
          m.1 = params[["m.1"]],
          m.2 = params[["m.2"]],
          epsilon = params[["epsilon"]]
        )
      
      j <- j + 1
      
      if(abs(ll[[j]] - ll[[j-1]]) < tolerance) {
        converged <- TRUE
      }
    } # Iterate until convergence/max iterations
    
    if(!converged) warning("Iteration limit reached without convergence.")
    
    params <- 
      with(params,
           c(mu.1 = mu.1,
             mu.2 = mu.2,
             m.1 = m.1,
             m.2 = m.2,
             epsilon = epsilon
           )
      )
    
    if(verbose) {
      return(
        list(
          ll = ll[1:j],
          par.trace = par.trace,
          params = params
        )
      )
    } else {
      return(
        list(
          params = params,
          ll = ll[j])
      )
    }
  }




# Perform a grid search over starting values, specified by 
# `mu.1`, `mu.2`, `m.1`, `m.2`, and `epsilon`.
bb.2c.mixture.grid.search <-
  function(
    data, y.0, n.0, y.1, n.1,
    id = NULL,
    mu.1 = c(0.1, 0.25, 0.5, 0.75, 0.9),
    mu.2 = c(0.1, 0.25, 0.5, 0.75, 0.9),
    epsilon = c(0.15, 0.35),
    m.1 = c(1),
    m.2 = c(1),
    n.mc = 1e6,
    posterior.quantiles = c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5,
                            0.75, 0.9, 0.95, 0.975, 0.99),
    max.iterations = 1e3,
    tolerance = 1e-8,
    verbose = FALSE
  ) {
    
    search.grid <-
      expand.grid(
        mu.1 = mu.1,
        mu.2 = mu.2,
        epsilon = epsilon,
        m.1 = m.1,
        m.2 = m.2
      )
    
    results <-
      data.frame(id = 
                   if(is.null(id)){
                     1:nrow(data)
                   } else{ 
                     data[, id]
                   },
                 setNames(object = data[, c(y.0, n.0, y.1, n.1)],
                          nm = c("y.0", "n.0", "y.1", "n.1")),
                 matrix(nrow = nrow(data),
                        ncol = length(posterior.quantiles),
                        dimnames = 
                          list(NULL, 
                               paste0("gamma.2c_pq.",
                                      posterior.quantiles))),
                 gamma.2c_prior_mean = NA,
                 gamma.2c_prior_variance = NA,
                 gamma.2c_posterior_mean = NA,
                 gamma.2c_posterior_variance = NA)
    
    results$t.k <- with(results, y.1 + y.0)
    results$r.k <- with(results, n.1/n.0)
    
    optima <- NA*search.grid
    optima$ll <- NA
    
    all.optima <- list()
    
    for(i in 1:nrow(search.grid)){
      params <- search.grid[i,]
      
      optimum <-
        bb.2c.mixture.optimize(
          data = data,
          y.0 = y.0,
          n.0 = n.0,
          y.1 = y.1,
          n.1 = n.1,
          mu.1 = params$mu.1,
          mu.2 = params$mu.2,
          m.1 = params$m.1,
          m.2 = params$m.2,
          epsilon = params$epsilon,
          max.iterations = max.iterations,
          tolerance = tolerance,
          verbose = verbose
        )
      
      if(verbose) all.optima[[i]] <- optimum
      
      optima[i, names(optimum$params)] <- optimum$params
      optima$ll[i] <- tail(optimum$ll, 1)
      cat("Iteration", i, "of", nrow(search.grid), "complete.\n")
    }
    
    optimal.parameters <-
      optima[which.max(optima$ll),
             c("mu.1", "mu.2", "m.1", "m.2", "epsilon")]
    
    alpha.1 <- with(optimal.parameters, mu.1*m.1)
    beta.1 <- with(optimal.parameters, (1 - mu.1)*m.1)
    alpha.2 <- with(optimal.parameters, mu.2*m.2)
    beta.2 <- with(optimal.parameters, (1 - mu.2)*m.2)
    epsilon <- with(optimal.parameters, epsilon)
    mu.mix <- with(optimal.parameters, (1 - epsilon)*mu.1 + epsilon*mu.2)
    
    results$ll.1 <-
      ll.mbb(y = results$y.1,
             t = results$t.k,
             mu = optimal.parameters$mu.1,
             m = optimal.parameters$m.1)
    
    results$ll.2 <-
      ll.mbb(y = results$y.1,
             t = results$t.k,
             mu = optimal.parameters$mu.2,
             m = optimal.parameters$m.2)
    
    results$log.mlr.2.1 <- with(results, ll.2 - ll.1)
    results$mlr.2.1 <- exp(results$log.mlr.2.1)
    
    results$posterior.odds.2.1 <-
      with(results, mlr.2.1*(epsilon/(1 + epsilon)))
    
    results$posterior.probability.2 <-
      with(results, posterior.odds.2.1/(1 + posterior.odds.2.1))
    
    infinite.mlr <-
      with(results, which(is.infinite(mlr.2.1) & mlr.2.1 > 0))
    results$posterior.probability.2[infinite.mlr] <- 1
    
    if(beta.1 > 1 & beta.2 > 1) {
      mml.prior.theta <-
        c(rbeta(n = round(n.mc*(1 - epsilon)), shape1 = alpha.1, shape2 = beta.1),
          rbeta(n = n.mc - round(n.mc*(1 - epsilon)), shape1 = alpha.2, shape2 = beta.2))
      
      mml.prior.gamma.unscaled <-
        (mml.prior.theta/(1 - mml.prior.theta))
      
      results$gamma.2c_prior_mean <-
        (1/results$r.k[i])*mean(mml.prior.gamma.unscaled)
    } else {
      results$gamma.2c_prior_mean <- NaN
    }
    
    if(beta.1 > 2 & beta.2 > 2) {
      results$gamma.2c_prior_variance =
        (1/results$r.k[i]^2)*var(mml.prior.gamma.unscaled)
    } else {
      results$gamma.2c_prior_variance <- NaN
    }
    
    posterior.theta <- list()
    posterior.gamma <- list()
    
    for (i in 1:nrow(results)){
      mml.posterior.theta <-
        c(rbeta(n = round(n.mc*(1 - results$posterior.probability.2[i])),
                shape1 = alpha.1 + results$y.1[i], 
                shape2 = beta.1 + results$y.0[i]),
          rbeta(n = n.mc - round(n.mc*(1 - results$posterior.probability.2[i])),
                shape1 = alpha.2 + results$y.1[i], 
                shape2 = beta.2 + results$y.0[i]))
      
      posterior.theta[[i]] <-
        setNames(
          object = data.frame(mml.posterior.theta),
          nm = as.character(data[i, id])
        )
      
      mml.posterior.gamma <-
        (1/results$r.k[i])*(mml.posterior.theta/(1 - mml.posterior.theta))
      
      posterior.gamma[[i]] <-
        setNames(
          object = data.frame(mml.posterior.gamma),
          nm = as.character(data[i, id])
        )
      
      results[i, paste0("gamma.2c_pq.", posterior.quantiles)] <-
        quantile(x = mml.posterior.gamma,
                 probs = posterior.quantiles)
      
      results[i, c("gamma.2c_posterior_mean", 
                   "gamma.2c_posterior_variance")] <-
        c(mean(mml.posterior.gamma), var(mml.posterior.gamma))
    }
    
    posterior.theta <- do.call(what = cbind, args = posterior.theta)
    posterior.gamma <- do.call(what = cbind, args = posterior.gamma)
    
    if(verbose){
      return(
        list(
          results = results,
          parameters =
            with(optimal.parameters,
                 c(mu.1 = mu.1, m.1 = m.1, mu.2 = mu.2, m.2 = m.2,
                   epsilon = epsilon,
                   alpha.1 = alpha.1, beta.1 = beta.1,
                   alpha.2 = alpha.2, beta.2 = beta.2
                 )
            ),
          all.results = optima,
          all.optima = all.optima,
          search.grid = search.grid,
          posterior.theta = posterior.theta,
          posterior.gamma = posterior.gamma
        )
      )
    } else{
      return(
        list(
          results = results,
          parameters =
            with(optimal.parameters,
                 c(mu.1 = mu.1, m.1 = m.1, mu.2 = mu.2, m.2 = m.2,
                   epsilon = epsilon,
                   posterior.mean = mu.1*(1 - epsilon) + mu.2*epsilon,
                   alpha.1 = alpha.1, beta.1 = beta.1,
                   alpha.2 = alpha.2, beta.2 = beta.2
                 )
            )
        )
      )
    }
  }