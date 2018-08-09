## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(DesignLibrary)

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github("DeclareDesign/DesignLibrary", keep_source = TRUE)

## ------------------------------------------------------------------------
my_designer <- function(N = 100,
                        prob = .5){
  if(0 > prob | 1 < prob) stop("prob must be in [0,1]")
  if(1 > N) stop("design must have at least two units")
  {{{ 
    population <- declare_population(N = N, noise = rnorm(N))
    potential_outcomes <- declare_potential_outcomes(Y ~ Z + noise)
    assignment <- declare_assignment(prob = prob)
    estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
    estimator <- declare_estimator(Y ~ Z, estimand = estimand)
    reveal <- declare_reveal(Y,Z)
    my_design <- population +
      potential_outcomes +
      estimand +
      assignment +
      reveal +
      estimator
  }}}
  attr(my_design, "code") <- DesignLibrary:::construct_design_code(designer = my_designer,
                                                   args =  match.call.defaults())
  my_design
}

## ----include=F-----------------------------------------------------------
# For testing:
my_designer()

## ---- eval=FALSE---------------------------------------------------------
#  {{{
#      population <- declare_population(N = N, noise = rnorm(N))
#      potential_outcomes <- declare_potential_outcomes(Y ~ Z + noise)
#      assignment <- declare_assignment(prob = prob)
#      estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#      estimator <- declare_estimator(Y ~ Z, estimand = estimand)
#      reveal <- declare_reveal(Y,Z)
#      my_design <- population +
#        potential_outcomes +
#        estimand +
#        assignment +
#        reveal +
#        estimator
#  }}}

## ----eval = F------------------------------------------------------------
#  N <- 100
#  prob <- 0.5

## ---- eval=FALSE---------------------------------------------------------
#  attr(my_design, "code") <- construct_design_code(designer = my_designer,
#                                                   args =  match.call.defaults())

## ----eval = FALSE--------------------------------------------------------
#  my_designer <- function(N = 100,
#                          prob = .5){
#    design_code <- paste0(
#      "population <- declare_population(N = ",N,", noise = rnorm(N))
#      potential_outcomes <- declare_potential_outcomes(Y ~ Z + noise)
#      assignment <- declare_assignment(prob = ",prob,")
#      estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
#      estimator <- declare_estimator(Y ~ Z, estimand = estimand)
#      reveal <- declare_reveal(Y,Z)
#      my_design <- population +
#        potential_outcomes +
#        estimand +
#        assignment +
#        reveal +
#        estimator")
#    my_design <- eval(parse(text = design_code))
#    attr(my_design, "code") <- design_code
#    my_design
#  }

## ---- eval=FALSE---------------------------------------------------------
#  attr(my_designer,"shiny_arguments") <-
#    list(
#      N = c(50, 100, 10000),
#      prob = c(.25, .50, .75)
#      )

## ---- eval=FALSE---------------------------------------------------------
#  attr(my_designer, "tips") <-
#    list(
#      N = "Sample size",
#      prob = "Probability of assignment to treatment"
#    )

## ---- eval=FALSE---------------------------------------------------------
#  attr(my_designer, "description") <- "
#  <p> A design of sample size <code>N</code> and probability of assignment <code>prob</code>.
#  "

## ---- eval=FALSE---------------------------------------------------------
#  #' Create a design
#  #'
#  #' This designer builds a design with \code{N} units.
#  #'
#  #' Key limitations: ate cannot be specified
#  #'
#  #' Note: Units are assigned to treatment with probability \code{prob} using complete random assignment
#  #'
#  

## ---- eval=FALSE---------------------------------------------------------
#  #' @param N A integer. Sample size
#  #' @param prob A number within the interval [0,1]. Probability of assigment to treatment.
#  #' @return A design.
#  #' @examples
#  #' To make a design using default arguments:
#  #' my_design <- my_designer()
#  #'

## ---- eval=FALSE---------------------------------------------------------
#  #' @concept two arm design
#  #' @seealso \code{\link{my_design}} \code{\link{simple_two_arm_designer}}

## ---- eval=FALSE---------------------------------------------------------
#  #'
#  #' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#  #' @export

