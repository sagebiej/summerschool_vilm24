library("apollo")

database <- data

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "Clogit_linear_sq",
  modelDescr ="Conditional Logit Model Linear with actual levels",
  indivID = "ID",
  mixing = FALSE,
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
              mu_cost = 0)






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
  V[['alt1']] = mu_hnv * alt1_HNV + mu_hnv_vis * alt1_x2 + 
                mu_pa * alt1_protected  + mu_pa_acc * alt1_x4 + 
                mu_cost * alt1_x5
  
  V[['alt2']] = mu_asc + mu_hnv * alt2_HNV +
                mu_pa * alt2_protected 
  
  
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
  #P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

c_logit_linear_sq = apollo_estimate(apollo_beta, apollo_fixed,
                                    apollo_probabilities, apollo_inputs, estimate_settings=list(estimationRoutine="bfgs",
                                                                                                hessianRoutine="maxLik"))

apollo_saveOutput(c_logit_linear_sq)


