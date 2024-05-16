# farmer <- function(x, varnames)
#   {
# A) farmer benefits without intervention ####
  pre_interv_benefit <- (vv(maize_yield_t_ha, gen_CV, n_years) *
                          ha_per_hh * no_of_seasons * 1000 * # conversion to kg/ha
                          vv(price_maize_per_kg, gen_CV, n_years) + 
                          vv( value_of_farm_assets, gen_CV, n_years))/ #natural capital in the farm e,g., trees
                          currency_change
  
  
  #B) Costs of farmer with Intervention ####
  # Farmer not joining the program
  farmer_nonpopinvol_event <- chance_event(intervention_nonpopInvolv,
                                           1, 0, n=1)
  
  # for (decision_consolidate in c(FALSE, TRUE)) {
  #   
  #   if(decision_consolidate){
  #     consolidate <- TRUE
  #     consolidate_plan_cost <- TRUE
  #   } else {
  #     consolidate <- FALSE
  #     consolidate_plan_cost <- TRUE
  #   }
  #   if (farmer_nonpopinvol_event) {
  #     consolidate <- FALSE
  #     consolidate_plan_cost <- TRUE
  #   } 
  #   
  #   # Calculate farmer COSTS 
  #   if(consolidate) {

      # cost of land consolidation
      # value of farm assets includes all assets e.g., trees, boreholes etc that the
      # farmer can longer access.
      # hh costs prior to the first payment that could have been realized from farm

     
  farmer_one_time_cost <- cost_of_disruption + # Damages on physical assets
                          hhupkeep_prior_to_first_payment # HH income needed to sustain the hh prior 1st payment
  
  farmer_recurring_cost <- vv(value_of_farm_assets, gen_CV, n_years) + #natural capital in the farm lost
                          (vv(saved_food_cost_pm, gen_CV, n_years) * 12)  #Food available from farm
  
  farmer_costs <-  (farmer_one_time_cost + farmer_recurring_cost)/currency_change
    # }else{
    #   farmer_costs <- 0 
    # }
    # if(consolidate_plan_cost){
    #   #planning cost involves legal fee and the cost of knowledge acquisition
      farmer_plan_cost <- planning_cost/currency_change
    # } else {
    #   farmer_plan_cost <- 0
    # }
      
# C) Calculate farmer BENEFIT  ####
    # if(consolidate) {
      
      #farmer's compensation 
      #hospital bills saved (proxy for long term health)
      health_event <- chance_event(health_risk, 1,0, n_years)
      #effect of the health risk on hh income
      medical_bills_saved <- health_event * 
        vv(income_on_hospital/100, gen_CV,n_years) * # hospital cost per year
        hh_income_pa
      
      #social cohesion benefit as a factor of free time by the cost of labour
      vice_event <- chance_event(vice_risk, 1,0, n_years) # vices in the community e.g. crime, drug use
      domesticconflict_event <- chance_event(domesticconflict_risk, 1, 0, n_years) 
      
      hours_on_vice <- vice_event * vv(social_time, gen_CV, n_years)
      hours_on_dconflict <- domesticconflict_event*vv(social_time, gen_CV, n_years)
      
      #impact of vice and conflict risks on social cohesion 
      effect_social_risks <- sapply(c(hours_on_vice + hours_on_dconflict),
                                    function(x)min(x, 1))
      actual_social_time <- social_time * (1-effect_social_risks)
      social_cohesion <- actual_social_time * vv(labour_cost, gen_CV, n_years)
      
      #Better childhood benefit = long term benefit is education
      #quantified by child's contribution to family farm labour weekly
      #and the value of labour
      
      better_childhood <- child_farm_time * children_per_hh * 
                        vv(labour_cost, gen_CV, n_years) 
      
      hh_land_size_acre <- ha_per_hh * ha_acre_conversion
      
      farmer_benefit <- ((vv(compensation_income_pm_acre, gen_CV, n_years) * 
                           hh_land_size_acre * 12) +
                           (vv(off_farm_employment, gen_CV, n_years) *12) + #alternative businesses 
                           (vv(production_costs_saved_acre, gen_CV, n_years) * # production costs (inputs,planting, weeding, harvesting)
                              hh_land_size_acre) + 
                           sale_of_hh_items_not_needed + #farm tools  and other hh items no longer required
                           medical_bills_saved + 
                           social_cohesion + 
                           better_childhood)/currency_change
      
    # }else {
    #   farmer_benefit <- 0
    # }
      
  #D) Calculate net benefit ####
    
    # if(decision_consolidate){
      farmer_consolidation_benefit <- farmer_benefit - 
                                  (farmer_costs + farmer_plan_cost)
      result_intervention <- farmer_consolidation_benefit 
    # }
    # if (!decision_consolidate){
      result_n_intervention <- pre_interv_benefit - farmer_plan_cost
  #   }
  # } # end of the intervention loop
  
  
  # NPV ###
  NPV_intervention <- 
    discount(result_intervention, discount_rate, calculate_NPV = T)
  
  NPV_n_intervention <- 
    discount(result_n_intervention, discount_rate, calculate_NPV = T)
  
#    return(list(Interv_NPV = NPV_intervention,
#                 No_Interv_NPV = NPV_n_intervention,
#                NPV_decision_do = NPV_intervention - NPV_n_intervention,
#                Cashflow_decision_do = result_intervention - result_n_intervention))
# }
# mcSimulation_results <- decisionSupport::mcSimulation(
#   estimate = estimate_read_csv("Input_tables/farmer_input_table.csv"),
#   model_function = farmer,
#   numberOfModelRuns = 1e2, #1000
#   functionSyntax = "plainNames")

# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
#                                     vars = c("Interv_NPV", "No_Interv_NPV"),
#                                     method = 'smooth_simple_overlay', 
#                                     base_size = 8)
# 
# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
#                                     vars = "NPV_decision_do",
#                                     method = 'boxplot_density')
# 
# decisionSupport::plot_cashflow(mcSimulation_object = mcSimulation_results, 
#                                cashflow_var_name = "Cashflow_decision_do")
# 
# pls_result <- plsr.mcSimulation(object = mcSimulation_results,
#                                 resultName = names(mcSimulation_results$y)[3], 
#                                 ncomp = 1)

# input_table <- read.csv("Input_tables/farmer_input_table.csv")
# 
# decisionSupport::plot_pls(pls_result, 
#                           input_table = input_table, 
#                           threshold = 0)

# mcSimulation_table <- data.frame(mcSimulation_results$x, 
#                                  mcSimulation_results$y[1:3])
# 
# evpi <- decisionSupport::multi_EVPI(mc = mcSimulation_table, 
#                                     first_out_var = "Interv_NPV")

# decisionSupport::plot_evpi(evpi, decision_vars = "Implementer_NPV_decision_do")


# compound_figure(mcSimulation_object = mcSimulation_results, 
#                 input_table = read.csv("Input_tables/farmer_input_table.csv"), 
#                 plsrResults = pls_result, 
#                 EVPIresults = evpi,
#                 decision_var_name = "NPV_decision_do", 
#                 cashflow_var_name = "Cashflow_decision_do",
#                 base_size = 7)
