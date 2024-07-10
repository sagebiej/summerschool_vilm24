library("apollo")

database <- data

### initialise apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "Clogit",
  modelDescr ="Conditional Logit Model",
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
              mu_hnv2 = 0,
              mu_hnv_vis  = 0,
              mu_hnv2_vis = 0,
              mu_pa  = 0,
              mu_pa2 = 0,
              mu_pa_half = 0,
              mu_pa2_half = 0,
              mu_pa_full = 0,
              mu_pa2_full = 0,
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
  V[['alt1']] = mu_hnv * alt1_HNV + mu_hnv2 * alt1_HNVsq + mu_hnv_vis * alt1_HNV * alt1_x2  + mu_hnv2_vis * alt1_HNVsq * alt1_x2 +
                mu_pa * alt1_protected + mu_pa2 * alt1_protectedsq + mu_pa_half * alt1_protected * Dummy_Half + 
                mu_pa2_half * alt1_protectedsq * Dummy_Half + mu_pa_full * alt1_protected * Dummy_Full + mu_pa2_full * alt1_protectedsq * Dummy_Full +
                mu_cost * alt1_x5
  
  V[['alt2']] = mu_asc + mu_hnv * alt2_HNV + mu_hnv2 * alt2_HNVsq 
                mu_pa * alt2_protected + mu_pa2 * alt2_protectedsq 
  
  
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

c_logit = apollo_estimate(apollo_beta, apollo_fixed,
                                    apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik"))

apollo_saveOutput(c_logit)


