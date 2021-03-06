#' Create a two-by-two factorial design
#'
#' Builds a two-by-two factorial design in which assignments to each factor are independent of each other.
#' 
#' @details
#' Three types of estimand are declared. First, weighted averages of the average treatment effects of each treatment, given the two conditions of the other treatments. Second and third, the difference in treatment effects of each treatment, given the conditions of the other treatment.
#' 
#' Units are assigned to treatment using complete random assignment. Potential outcomes follow a normal distribution.
#' 
#' Treatment A is assigned first and then Treatment B within blocks defined by treatment A. Thus, if there are 6 units 
#' 3 are guaranteed to receive treatment A but the number receiving treatment B is stochastic.
#' 
#' See \code{\link{multi_arm_designer}} for a factorial design with non independent assignments.
#' 
#' @param N An integer. Size of sample.
#' @param prob_A A number in [0,1]. Probability of assignment to treatment A.
#' @param prob_B A number in [0,1]. Probability of assignment to treatment B.
#' @param weight_A A number. Weight placed on A=1 condition in definition of "average effect of B" estimand.
#' @param weight_B A number. Weight placed on B=1 condition in definition of "average effect of A" estimand.
#' @param outcome_means A vector of length 4. Average outcome in each A,B condition, in order AB = 00, 01, 10, 11. Values overridden by mean_A0B0, mean_A0B1, mean_A1B0, mean_A1B1, if provided.
#' @param mean_A0B0 A number. Mean outcome in A=0, B=0 condition.
#' @param mean_A0B1 A number. Mean outcome in A=0, B=1 condition.
#' @param mean_A1B0 A number. Mean outcome in A=1, B=0 condition.
#' @param mean_A1B1 A number. Mean outcome in A=1, B=1 condition.
#' @param sd_i A nonnegative scalar. Standard deviation of individual-level shock (common across arms).
#' @param outcome_sds A nonnegative vector of length 4. Standard deviation of (additional) unit level shock in each condition, in order AB = 00, 01, 10, 11.
#' @param args_to_fix A character vector. Names of arguments to be args_to_fix in design.
#' @aliases simple_factorial_designer
#' @return A two-by-two factorial design.
#' @author \href{https://declaredesign.org/}{DeclareDesign Team}
#' @concept experiment factorial
#' @importFrom DeclareDesign declare_assignment declare_inquiry declare_estimator declare_population declare_potential_outcomes declare_reveal diagnose_design redesign
#' @importFrom fabricatr fabricate 
#' @importFrom randomizr conduct_ra 
#' @importFrom estimatr lm_robust
#' @importFrom rlang list2 expr eval_bare
#' @export two_by_two_designer simple_factorial_designer
#'
#' @examples
#' design <- two_by_two_designer(outcome_means = c(0,0,0,1))
#' 
#' # A design biased for the specified estimands:
#' design <- two_by_two_designer(outcome_means = c(0,0,0,1), prob_A = .8, prob_B = .2)
#' \dontrun{
#' diagnose_design(design)
#' }
#' 
#' # A design with estimands that "match" the assignment:
#' design <- two_by_two_designer(outcome_means = c(0,0,0,1), 
#'                                     prob_A = .8, prob_B = .2, 
#'                                     weight_A = .8, weight_B = .2)
#' \dontrun{
#' diagnose_design(design)
#' }
#' 
#' # Compare power with and without interactions, given same average effects in each arm
#' designs <- redesign(two_by_two_designer(), 
#'                     outcome_means = list(c(0,0,0,1), c(0,.5,.5,1)))
#' \dontrun{
#' diagnose_design(designs)
#' }
#' 
two_by_two_designer <- function(N = 100,
                                prob_A = .5,
                                prob_B = .5,
                                weight_A = .5, 
                                weight_B = .5, 
                                outcome_means = rep(0,4),
                                mean_A0B0 = outcome_means[1],
                                mean_A0B1 = outcome_means[2],
                                mean_A1B0 = outcome_means[3],
                                mean_A1B1 = outcome_means[4],
                                sd_i = 1,
                                outcome_sds = rep(0,4),
                                args_to_fix = NULL
){
  if((weight_A < 0) || (weight_B < 0) || (weight_A > 1) || (weight_B > 1)) stop("weight_A and weight_B must be in [0,1]")
  if(max(c(sd_i, outcome_sds) < 0) )      stop("sd_i and outcome_sds must be nonnegative")
  if(max(c(prob_A, prob_B) < 0)) stop("prob_ arguments must be nonnegative")
  if(max(c(prob_A, prob_B) > 1))  stop("prob_ arguments must not exceed 1")
  {{{
    
    # M: Model
    population <- declare_population(N, u = rnorm(N, sd=sd_i))
    
    potential_outcomes <- declare_potential_outcomes(
      Y_A_0_B_0 = mean_A0B0 + u + rnorm(N, sd = outcome_sds[1]),  
      Y_A_0_B_1 = mean_A0B1 + u + rnorm(N, sd = outcome_sds[2]),  
      Y_A_1_B_0 = mean_A1B0 + u + rnorm(N, sd = outcome_sds[3]),
      Y_A_1_B_1 = mean_A1B1 + u + rnorm(N, sd = outcome_sds[4]))
    
    
    # I: Inquiry
    estimand_1 <- declare_inquiry(
      ate_A = weight_B*mean(Y_A_1_B_1 - Y_A_0_B_1) + (1-weight_B)*mean(Y_A_1_B_0 - Y_A_0_B_0))
    
    estimand_2 <- declare_inquiry(
      ate_B = weight_A*mean(Y_A_1_B_1 - Y_A_1_B_0) + (1-weight_A)*mean(Y_A_0_B_1 - Y_A_0_B_0))
    
    estimand_3 <- declare_inquiry(
      interaction = mean((Y_A_1_B_1 - Y_A_1_B_0) - (Y_A_0_B_1 - Y_A_0_B_0)))
    
    # D: Data Strategy
    
    # Factorial assignments
    assign_A <- declare_assignment(prob = prob_A, assignment_variable = A)
    
    assign_B <- declare_assignment(prob = prob_B, assignment_variable = B, blocks = A)
    
    reveal_Y <- declare_reveal(Y_variables = Y, assignment_variables = c(A,B))
    
    # A: Answer Strategy
    estimator_1 <- declare_estimator(Y ~ A + B,
                                     model = lm_robust,
                                     term = c("A", "B"),
                                     inquiry = c("ate_A", "ate_B"), 
                                     label = "No_Interaction")
    
    estimator_2 <- declare_estimator(Y ~ A + B + A:B,
                                     model = lm_robust,
                                     term = "A:B", 
                                     inquiry = "interaction", 
                                     label = "Interaction")
    
    # Design
    two_by_two_design <- population + potential_outcomes + 
      estimand_1 + estimand_2 + estimand_3 +
      assign_A + assign_B + reveal_Y + 
      estimator_1 + estimator_2
    
  }}}
  
  attr(two_by_two_design, "code") <- 
    construct_design_code(designer = two_by_two_designer, 
                          args = match.call.defaults(),
                          args_to_fix = args_to_fix,
                          exclude_args = "outcome_means",
                          arguments_as_values = TRUE)
  
  two_by_two_design
}

attr(two_by_two_designer, "definitions") <- data.frame(
  names  = c("N",  "prob_A",  "prob_B",  "weight_A",  "weight_B",  
             "outcome_means",  "mean_A0B0",  "mean_A0B1",  "mean_A1B0",  
             "mean_A1B1",  "sd_i",  "outcome_sds", "args_to_fix"),
  tips  = c("Sample size",
            "Probability of assignment to treatment A",
            "Probability of assignment to treatment B",
            "Weight on B=1 condition for effect of A estimand", 
            "Weight on A=1 condition for effect of B estimand", 
            "Average outcome in each A,B condition, in order: A0B0, A0B1, A1B0, A1B1.",
            "Mean outcome for A=0, B=0",
            "Mean outcome for A=0, B=1",
            "Mean outcome for A=1, B=0",
            "Mean outcome for A=1, B=1",
            "Standard deviation of individual-level shock",
            "Standard deviation of unit level shock in each condition",
            "Names of arguments to be args_to_fix"),
  class = c("integer", rep("numeric", 11),"character"),
  vector = c(rep(FALSE, 11), TRUE, TRUE),
  min = c(1, 0, 0, 1/10000, 1/10000, rep(-Inf, 5), 0, 0, NA), 
  max = c(Inf, 1, 1, rep(Inf, 9), NA),
  inspector_min = c(100, 0, 0, 1/10, 1/10, rep(-1, 5), 0, 0, NA),
  inspector_step = c(50, .2, .2, 1/2, 1/2, rep(.2, 7), NA),
  stringsAsFactors = FALSE
)

attr(two_by_two_designer, "shiny_arguments") <- list(
  N = c(16, 32, 64), weight_A = c(0, .5), 
  mean_A0B1 = 0:1, 
  mean_A1B0 = 0:1, 
  mean_A1B1 = -1:3) 

attr(two_by_two_designer, "description") <- "
<p> A 2x2 factorial design of sample size <code>N</code> with independent treatment assignment.
"


simple_factorial_designer <- function(...){
  .Deprecated("two_by_two_designer")
  dots <- list2(...)
  eval_bare(expr(two_by_two_designer(!!!dots)))
}
