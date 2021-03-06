#' Bayesian hierarchical expressed agenda model 
#'
#' \code{ExpAgendaVonmon} implements bayesian hierarchical topic model for texts from Grimmer (2010).
#'
#' @param obj an EADTMatrix class object created by \code{\link{PreProcess}} containing the \code{term.doc} and \code{authors} matrices. Note \code{term.doc} and \code{authors} should not be set independently if \code{obj} is specified.
#' @param term.doc matrix. Suppose that there are \eqn{D} total documents and \eqn{w} words. \code{term.doc} is a term-document matrix with dimensions: \eqn{D * w}. \code{term.doc} should be sorted by author. Do not set if \code{obj} is specified.
#' @param authors matrix. If there are \eqn{n} total actors whose attention to issues you would like to measure. \code{author} is an \eqn{n * 2} matrix. The first column specifies the first document in the term-document matrix that was authored by a given author. The second column specifies the last document authored. Do not set if \code{obj} is specified.
#' @param n.cats numeric. Sets the number of components (topics) in the mixture model. The default is 10.
#' @param kappa numeric. Distribution's dispersion [CHECK].
#' @param verbose logical. Whether or not to print out each iteration.
#'
#' @source Grimmer, J. (2010). A Bayesian Hierarchical Topic Model for Political Texts: Measuring Expressed Agendas in Senate Press Releases. Political Analysis, 18, 1-35. \url{http://pan.oxfordjournals.org/content/18/1/1.short}.
#' 
#' @return
#' A \code{ExpAgendaOut} object. The object contains five elements: \code{thetas}, \code{mus}, \code{rs}, \code{alpha}, and \code{authorID}. \code{thetas} are [FILL IN]. \code{mus} are location on the unit hyperspace where the vMF distribution reaches its mode for each stem [CHECK]. \code{rs} are the probability of document \eqn{j} from author \eqn{i} being from topic \eqn{k}. \code{alphas} are the prior distributions of the topics. \code{authorID} is used for \code{\link{DocTopics}} to return the documents their their original order.
#' 
#'
#' 
#' @import MCMCpack
#' 
#' @export

ExpAgendaVonmon <- function(obj = NULL, term.doc = NULL, authors = NULL, n.cats = 10, kappa = 400, verbose = TRUE){
  # Determine if obj or term.doc/authors is specified
  if (!is.null(obj) & !is.null(term.doc) & !is.null(authors)){
    stop("Only obj or term.doc & authors can be set at once.")
  }
  if (!is.null(obj) & !is.null(term.doc)){
    stop("term.doc cannot be set if obj is also specified.")
  }
  if (!is.null(obj) & !is.null(authors)){
    stop("authors cannot be set if obj is also specified.")
  }
  
  # Extract matrices from obj
  if (!is.null(obj) & class(obj) != "ExpAgendaDTMatrix"){
    stop("obj must be of class ExpAgendaDTMatrix and created by PreProsses.")
  }
  else if (!is.null(obj) & class(obj) == "ExpAgendaDTMatrix"){
    authors <- obj[[1]]
    term.doc <- obj[[2]]
  }
  
  # Stop if an author only has one observation
  if (any(authors[, 1] - authors[, 2] == 0)){
    OneDoc <- subset(authors, authors[, 1] == authors[, 2])
    stop("The following authors have only one text. Please remove the author before proceeding.\n\n", paste(row.names(OneDoc), collapse = "\n"))
  }
  
  ## the likelihood for the dirichlet component of the model
  dir.lik <- function(par, pis, prior){
    alphas = par[1:ncol(pis)]
    alphas <- exp(alphas) 
    ester <- log(ddirichlet(pis,alpha=alphas))
    ## ester[which(ester==-Inf)]<- log(1e-323)
    priors <- dgamma(alphas, rate = prior, shape = 1, log = T)
    out <- sum(ester) + sum(priors)
    return(out)
  }
  
  N <- nrow(authors)
  
  ## component labels
  taus <- matrix(NA, nrow=nrow(term.doc), ncol=n.cats)
  
  mus <- matrix(NA, nrow=ncol(term.doc), ncol=n.cats)
  for(j in 1:ncol(mus)){
    mus[,j]<- rgamma(ncol(term.doc), shape=10)
  }

  for(j in 1:ncol(mus)){
    mus[,j]<- mus[,j]/sqrt(mus[,j]%*%mus[,j])
  }
  
  ## actually alpha in the paper
  ## will fix in output
  thetas <- matrix(NA, nrow=1, ncol=n.cats)
  for(j in 1:nrow(thetas)){
    thetas[j,] <- 20 + rnorm(n.cats, 0,0.025)
  }
  
  pis <- matrix(NA, nrow = N, ncol = n.cats)
  for(j in 1:N){
    pis[j,] <- thetas + (authors[j, 2] - authors[j, 1])/n.cats
  }
   
  kappa <- kappa
  k <- 0
  prior <- 1
  prior <- c(rep(prior, n.cats))
  prior <- prior*-1
  Bayes <- 1
  v <- 0
  z <- 0
  
  while(z == 0){
    thetas.old <- thetas
    pi.gam <- digamma(pis) - digamma(apply(pis, 1, sum))
    exp.pi <- exp(pi.gam)
    
    ## step one computes the variational
    ## posterior, which is going to be very close
    ## the original posterior from the ECM algorithm
    ## once again, differences in the 'pi.gam' stage
    
    part1 <- kappa*term.doc%*%mus
    
    for(j in 1:N){
      for(k in authors[j,1]:authors[j,2]){
        temp<- part1[k,] - max(part1[k,])
        num<- exp.pi[j,]*exp(temp)
        taus[k,]<- num/sum(num)
        }
      }

    mus <- matrix(1/sqrt(ncol(term.doc)), nrow=nrow(mus), ncol=n.cats)
    m <- 0
     
    for(j in 1:N){
      for(k in authors[j,1]:authors[j,2]){
        fills <- term.doc[k,]%o%taus[k,]
        mus <- mus + fills
      }
    }
    
    for(j in 1:ncol(mus)){
      mus[,j] <- mus[,j]/sqrt(mus[,j]%*%mus[,j])
    }
     
    ## the M part for top level Dirichlet
    ## is the maximization of alpha parameters
    
    ## this is the Newton-Raphson algorithm from 
    ## Blei, Ng, and Jordan (2003)
    alpha <- thetas[1,]
    ## and we know that the log
    N1 <- N
    te <- log(exp.pi)
    suff.stats <- apply(te, 2, sum)/N1
    k <- 0
    a <- 0
    while(k == 0){
      sum.alpha<- digamma(sum(alpha))
      di.alph<- digamma(alpha)
      grads<- Bayes*prior + N1*(sum.alpha - di.alph + suff.stats)
      qs<- - N1*trigamma(alpha)
      c <- N1*trigamma(sum(alpha))
      b1<- sum(grads/qs)
      b2<- 1/c + sum(1/qs)
      b<- b1/b2
      g<- (grads-b)/qs
      alpha2<- alpha - g
      ester<- which(alpha2<0)
      if(length(ester)>0){
        alpha2[ester]<- pi + rpois(length(ester), rep(3, length(ester)))
      }
      alpha<- alpha2
      # print(alpha)
      g<- max(g)
      if(abs(g)<1e-8){
        k<-1}
      a<- a + 1
      if(a>15000){
        print('Switching to Optim')
        temp.pi<- exp.pi/apply(exp.pi,1, sum)
        temp<- optim(log(thetas[1,]), dir.lik, control=list(trace=100, fnscale=-1), method='BFGS',
                     pis=temp.pi, prior=abs(prior))
        alpha <- exp(temp$par) 
        k <- 1
      }
    }
    
    thetas[1,]<- alpha
    
    sen.list<- list()
    for(j in 1:N){
      ints <- authors[j,1]:authors[j,2]
      sen.list[[j]] <- taus[ints,]
    }
    
    for(i in 1:N){
      pre.mle <- apply(sen.list[[4]], 2, sum)
      theta.ch <- thetas
      #for(j in 1:n.coals){
      #theta.ch[j,]<- theta.ch[j,] + pre.mle
      #theta.ch[j,]<- theta.ch[j,]*coals[i,j]
      #}
      theta.ch <- theta.ch + pre.mle
      pis[i,]<- theta.ch
    }
    
    
    afr <- abs(thetas.old-thetas)
    if(max(afr)<1e-5){
      z <- 1}
    cat('\n')
    #plot(mus[,1]~mus[,2])
    #print(thetas[1,])
    #print(max(thetas))
    if(verbose == TRUE){
      cat(max(afr), '\n')
      cat('next iteration', '\n')}
  }
  
  authorID <- obj$authorID
  out <- list(pis, mus, taus, thetas, authorID)
  names(out) <- c('thetas', 'mus', 'rs', 'alpha', 'authorID')
  class(out) <- "ExpAgendaOut"
  return(out)
}