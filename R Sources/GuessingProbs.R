library(dplyr)
library(tidyr)

#'probability (by standard binomial expansions) of (exactly!) k correct answers out of N trials
#'
#' @param k amount of trials answered correctly
#' @param N amount of total trials
#' @param p the probability of a correct response
#' @param q the probability of an incorrect response
k_out_of_N = function(k,N,p = 0.5,q = 0.5){
  if(p+q > 1 | p+q < 0){stop("The sum of p and q must be 1, p and q must lie in range [0,1], respectively!")}
  if(N < 1){stop("At least one trial required in each test!")}
  if(k < 0){stop("Number of correct responses must not be smaller than 0!")}
  if(k > N){stop("Number of correct responses must not be greater than number of total trials!")}
  if(p == q){
    (factorial(N) / (factorial(k) * factorial(N-k))) * 0.5^N
  }else{
    (factorial(N) / (factorial(k) * factorial(N-k))) * p^k * q^(N-k)
  }
}

#'probability (by standard binomial expansions) of k or FEWER correct answers out of N trials, i.e. 
#' the sum of all the probabilities from 0 to k
#'
#' @param k amount of trials answered correctly at most
#' @param N amount of total trials
#' @param p the probability of a correct response
#' @param q the probability of an incorrect response
k_out_of_N_cumulative = function(k,N,p = 0.5,q = 0.5){
  if(k < 0) return(0);
  probs_sum = k_out_of_N(k,N,p,q) + k_out_of_N_cumulative(k-1,N,p,q)
  return(probs_sum);
}

#'returns table with probabilities of k or MORE correct answers out of N trials
#'for a range of k's and N's
#'with k as columns 
# and N as rows 
#'
#' @param k amount of trials at least answered correctly
#' @param N amount of total trials 
#' @param p the probability of a correct response
k_out_of_N_matrix_cumulative =  function(Ns = c(1:6), ks = c(1:6), p = 0.5){
  if(p < 0 | p > 1){stop("p needs to lie within the range [0,1]")}
  expand.grid(k = ks, N = Ns) %>%
    filter(k <= N) %>% 
    rowwise() %>%
    mutate(probs_lek = 1 - k_out_of_N_cumulative(k-1,N, p, 1-p))  %>%
    spread(key = k, value = probs_lek)
}


#'determines the amount of trials that participants are required to respond to correctly,
#' chances to pass N trials by chance (i.e. guessing) must not succeed 5%.
#' returns the minimal requirement of all valid options, or the best possible requirement.
#'
#' @param N amount of control/attention trials per questionnaire
#' @param p the probability of a correct response
find_k_min = function(N, p = 0.5){
  if(N < 1){stop("At least one trial required in each test!")}
  probs = expand.grid(k = c(1:N), N = N) %>% 
    rowwise() %>% 
    mutate(gek_probs = 1 - k_out_of_N_cumulative(k-1,N, p, 1-p))
  if(any(probs$gek_probs <= 0.05)){
    k_min = probs %>%
      # filter options where chances to pass by guessing are small enough
      filter(gek_probs <= 0.05) %>% 
      # chose minimal requirement of all valid options
      select(k) %>% min()
  }else{
    warning(paste0("The minimal chances to pass this test by chance still lie at ",
                   min(probs$gek_probs),"!"))
    k_min = probs %>% as.data.frame() %>%
      slice_min(gek_probs) %>% pull(k)
  }
  if(k_min == N){
    warning(paste0("This test requires perfection! Participants are not allowed to err even once! Is this adequate regarding the length of your questionnaire?"))
  }
  return(k_min)
}

#' Computes the joint probabilities of passing LS control trials on a positional account
#' for different requirements, i.e. k out of N correct responses
#' 
#' The assessment of participants’ performance is carried out separately for grammatical (CONDTION_NO == 1) and
#' ungrammatical (CONDTION_NO == 2) stimuli, focusing on the pertinent side of the scale in each case. 
#' The chance to pass control trials is then the joint probability of passing the two groups individually. 
#' 
#' @param N_gram amount of grammatical control trials per questionnaire 
#' @param p_gram the probability of a correct response for grammatical controls
#' @param N_ungram amount of ungrammatical control trials per questionnaire 
#' @param p_ungram the probability of a correct response for ungrammatical controls
probs_joint_positional = function(N_gram, p_gram = 3/5, 
                            N_ungram, p_ungram = 2/5){
  probs_gram = expand.grid(gram_k = c(0:N_gram), gram_N = N_gram) %>% 
    rowwise() %>% 
    mutate(gram_probs  = 1 - 
             k_out_of_N_cumulative(gram_k-1,gram_N, p_gram, 1-p_gram))
  probs_ungram = expand.grid(ungram_k = c(0:N_ungram), ungram_N = N_ungram) %>% 
    rowwise() %>% 
    mutate(ungram_probs = 1 - k_out_of_N_cumulative(ungram_k-1,ungram_N, p_ungram, 1-p_ungram))
  probs_joint = merge(probs_gram, probs_ungram, all = TRUE) %>%
    mutate(joint_probs = gram_probs * ungram_probs)
  return(probs_joint)
}

