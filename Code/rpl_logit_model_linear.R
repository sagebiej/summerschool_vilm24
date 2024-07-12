library("apollo")

database <- data

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "RPL ",
  modelDescr ="Random Parameters Logit Logit Model Linear",
  indivID = "ID",
  mixing = TRUE,
  outputDirectory = "Estimation_results"
)



# ################################################################# #
#### DEFINE apollo_beta()                                        ####
# ################################################################# #

### set starting values all to 0 
apollo_beta=c(mu_asc    = 0,
              mu_hnv  = 0,
              
              mu_hnv_vis  = 0,
              mu_pa  = 0,
              
              mu_pa_acc = 0,
              mu_cost = 0,
              sd_asc    = 0,
              sd_hnv  = 0,
              
              sd_hnv_vis  = 0,
              sd_pa  = 0,
              
              sd_pa_acc = 0,
              sd_cost = 0)



# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ##
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 2000, #50 für Code testen, min. 500 für verlässliche Ergebnisse
  interUnifDraws = c(),
  # nur normal distributed: (für beitrag  lognormal transformieren)
  interNormDraws = c("draws_asc",
                     "draws_hnv",
                     "draws_hnv_vis",
                     "draws_pa",
                     "draws_pa_acc",
                     "draws_cost"),
  # keine Intra-Individuen Heterogenität: (das wären abweichende Präferenzen für selbe Individuen zwischen verschiedenen Choices)
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["asc"]] = mu_asc + sd_asc* draws_asc
  randcoeff[["hnv"]] = mu_hnv + sd_hnv * draws_hnv
  randcoeff[["hnv_vis"]] = mu_hnv_vis + sd_hnv_vis * draws_hnv_vis
  randcoeff[["pa"]] = mu_pa + sd_pa * draws_pa
  randcoeff[["pa_acc"]] = mu_pa_acc + sd_pa_acc * draws_pa_acc
  randcoeff[["cost"]] = - exp(mu_cost + sd_cost * draws_cost)
  
  return(randcoeff)
}


# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #



### keine Parameter fix halten
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate" ){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below) #added interaction of price and personal income 
  V = list()
  V[['alt1']] = hnv * alt1_x1 +  hnv_vis * alt1_x2 + 
                pa * alt1_x3  +  pa_acc  * alt1_x4 + 
                cost * alt1_x5
  
  V[['alt2']] = asc 
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = CHOICE,
    V             = V  # tell function to use list vector defined above
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}
# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

rpl_logit_linear = apollo_estimate(apollo_beta, apollo_fixed,
                                    apollo_probabilities, apollo_inputs, estimate_settings=list(estimationRoutine="bfgs",
                                                                                                hessianRoutine="maxLik"))

apollo_saveOutput(rpl_logit_linear)


