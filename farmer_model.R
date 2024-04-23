install.packages("decisionSupport")

library(decisionSupport)

# function start
farmer_decision <- function(x, varnames)
  {
  #pre-farmer benefits
  pre_interv_benefit <- vv(maize_yield_t_ha, gen_CV, n_years) * 1000* ha_per_hh * 
    vv(price_maize_per_kg, gen_CV, n_years) + value_of_farm_assets

  #chance event
farmer_nonpopinvol_event <- chance_event(intervention_nonpopInvolv,
                                         1, 0, n=1)
 
for (decision_consolidate in c(FALSE, TRUE)) {
  
  if(decision_consolidate){
    consolidate <- TRUE
    consolidate_plan_cost <- TRUE
  } else {
    consolidate <- FALSE
    consolidate_plan_cost <- TRUE
  }
  if (farmer_nonpopinvol_event) {
    consolidate <- FALSE
    consolidate_plan_cost <- TRUE
  } 
  
  # Calculate farmer costs ####
  if(consolidate) {
    #cost of land consolidation
    #value of farm assets includes all assets e.g., trees, boreholes etc that the
    #farmer can longer access. 
    farmer_costs <- (c((value_of_farm_assets + cost_of_disruption),
                       rep(0, n_years-1))) + 
                      (vv(saved_food_cost_pm, gen_CV, n_years) * 12)
  } else{
    farmer_costs <- 0 
  }
  if(consolidate_plan_cost){
    #planning cost involves legal fee and the cost of knowledge acquisition
    farmer_plan_cost <- planning_cost
  } else {
    farmer_plan_cost <- 0
  }
  # Calculate farmer benefit  ####
  if(consolidate) {
    #farmer gets a commensurate benefit to the size of land and 
    #productivity 
   
  farmer_benefit <- (vv(passive_land_income_pm, gen_CV, n_years) * 12) +
      (vv(off_farm_employment, gen_CV, n_years) *12) +
      (vv(production_costs_saved_acre, gen_CV, n_years) * 
         (ha_per_hh/ha_acre_conversion))
  }else {
    farmer_benefit <- 0
  }
  # Calculate net benefit ####
  
  if(decision_consolidate){
    consolidation_benefit <- farmer_benefit - farmer_costs - 
      farmer_plan_cost
    net_benefit <- consolidation_benefit - pre_interv_benefit
    result_intervention <- net_benefit
  }
  if (!decision_consolidate){
    total_cost <- farmer_costs + farmer_plan_cost
    result_n_intervention <- total_cost
  }
} # end of the intervention loop

  # NPV ###
  NPV_intervention <- 
    discount(result_intervention, discount_rate, calculate_NPV = T)
  
  NPV_n_intervention <- 
    discount(result_n_intervention, discount_rate, calculate_NPV = T)
  
  return(list(Interv_NPV = NPV_intervention,
              No_Interv_NPV = NPV_n_intervention,
              NPV_decision_do = NPV_intervention - NPV_n_intervention, 
              Cashflow_decision_do = result_intervention - result_n_intervention))
}

#simulate results
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = estimate_read_csv("Input_tables/farmer_input_table.csv"),
  model_function = farmer_decision,
  numberOfModelRuns = 1e2, #100
  functionSyntax = "plainNames")

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Interv_NPV", "No_Interv_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 8)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_do",
                                    method = 'boxplot_density')

decisionSupport::plot_cashflow(mcSimulation_object = mcSimulation_results, 
                               cashflow_var_name = "Cashflow_decision_do")

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], 
                                ncomp = 1)

input_table <- read.csv("Input_tables/farmer_input_table.csv")

decisionSupport::plot_pls(pls_result, 
                          input_table = input_table, 
                          threshold = 0)

mcSimulation_table <- data.frame(mcSimulation_results$x, 
                                 mcSimulation_results$y[1:3])

evpi <- decisionSupport::multi_EVPI(mc = mcSimulation_table, 
                                    first_out_var = "Interv_NPV")

decisionSupport::plot_evpi(evpi, decision_vars = "NPV_decision_do")


compound_figure(mcSimulation_object = mcSimulation_results, 
                 input_table = read.csv("Input_tables/farmer_input_table.csv"), 
                 plsrResults = pls_result, 
                 EVPIresults = evpi,
                 decision_var_name = "NPV_decision_do", 
                 cashflow_var_name = "Cashflow_decision_do",
                 base_size = 7)
