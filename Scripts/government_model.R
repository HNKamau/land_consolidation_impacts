# GOVERNMENT COSTS AND BENEFITS
# Risks associated with government

lc <- function(x, varnames)
  {

political_interf_event <- chance_event(political_interference, 
                                      1,0,n = 1)
inadequate_funds_event <- chance_event(inadequate_funds, 1, 0, n=1)
farmer_nonpopinvol_event <- chance_event(intervention_nonpopInvolv,
                                         1, 0, n=1)

# Intervention loop
for(decision_consolidate in c(FALSE, TRUE)){
  planning <- TRUE
  if(decision_consolidate && !farmer_nonpopinvol_event){
    establishment <- TRUE # compensation, farm preparation, ongoing operations
     } else {
    establishment <- FALSE
     }
  
# Government COSTS
if(planning){
  
  # Risks increasing the costs
  more_money_due_to_politicalinterf <- 
    political_interf_event * money_needed_politicalinterf/100 
  
actual_public_awareness <- public_awareness * (1+more_money_due_to_politicalinterf)
  
plan_cost <- 
            actual_public_awareness +  # communication, community meetings etc
            training_cost + # Training community elders, TOTs and others
            feasibility_study_cost + # Pre-feasibility study for suitability
            policy_formulation_cost # Formulation of policy, act, and or bill
}else{
    plan_cost <- 0
  }

#Establishment cost
if(establishment) {
total_ha <- ha_per_hh * total_hhs #target total land size

land_prep_cost <- land_prep_per_acre / ha_acre_conversion * total_ha # land survey and leveling
machinery_cost <- farm_infrastructure + farm_machinery_infrastructure # buildings,tractors, harrows, seeders, cars etc
irrigation_cost <- (irrigation_infrastructure_per_ha * total_ha) #irrigation equipment
initial_cost <- land_prep_cost + machinery_cost + irrigation_cost

  ## Operations cost 
  # maintenance cost of machinery and irrigation infrastructure
  # initialize maintenance cost by zeros through out the years
  maintenance_cost_annual <- rep(0, n_years) 
  
  #setting the maintenance cost of second year
  maintenance_cost_annual [2] <-
    farm_machinery_maintence_percent/100 * farm_machinery_infrastructure +
    irrigation_maitenance_percent/100 *  irrigation_infrastructure_per_ha * 
    total_ha
  
  # Looping through years 3 to n years to calculate the maintenance
  for(i in 3:n_years){
    maintenance_cost_annual[i] <- 
      maintenance_cost_annual[i-1] +
      vv(maintenance_cost_annual[2], gen_CV, n_years)
  }
#  print(maintenance_cost_annual)
  annual_compensation <- vv(compensation_income_pm_acre, gen_CV, n_years) * 
                        total_hhs * 12 # annual payments to farmers
  annual_production_cost <- vv(production_cost_per_acre, gen_CV, n_years) / 
                        ha_acre_conversion * total_ha * no_of_seasons
  all_labour_cost <- vv(all_labour, gen_CV, n_years) # skilled and unskilled labour
  
  operations_costs <- annual_compensation + annual_production_cost + 
                      maintenance_cost_annual + all_labour_cost

LC_costs <- (plan_cost + initial_cost + operations_costs)/ currency_change

}else {
  LC_costs <- 0
}
  
  # Benefits ##
  if(establishment){
  # Ex-ante risks that will reduce the benefits
  natural_hazard_event <- chance_event(natural_hazard, 1,0, n_years)
  pest_disease_event <- chance_event(pest_disease_risk, 1,0, n_years)
  
  crop_loss_drought <- natural_hazard_event * vv(yield_loss_drought/100, gen_CV, n_years)
  crop_loss_disease <- pest_disease_event * vv(yield_loss_disease/100, gen_CV, n_years)
  
  effect_of_crop_risks <- sapply(c(crop_loss_drought + crop_loss_disease),
                                 function(x) min(x,1))
  
  crop_yield_produced <- vv(maize_yield_t_ha, gen_CV, n_years) * total_ha *
                          no_of_seasons 
  actual_yield_t_ha <- crop_yield_produced * (1-effect_of_crop_risks)
  
 rainfed_revenue <- 
    (actual_yield_t_ha * 1000 * # change to kg/ha
    vv(price_maize_per_kg, gen_CV, n_years))/currency_change
  
  #benefit of scale operation, irrigation
  return_due_machinery <- actual_yield_t_ha *
                            vv(machinery_technical_efficiency/100, gen_CV, n_years) *
                            vv(price_maize_per_kg, gen_CV, n_years)

  return_due_irrigation <- actual_yield_t_ha *  
                            vv(yield_irrigation_factor/100, gen_CV, n_years) *
                            vv(price_maize_per_kg, gen_CV, n_years)
  
  crop_revenue <- rainfed_revenue+ return_due_machinery + return_due_irrigation
  
  } else {
  crop_revenue <- 0
}
  # Calculate net benefit ####
  
  if(decision_consolidate){
    GOVT_consolidation_benefit <- crop_revenue - LC_costs
    GOVT_result_intervention = GOVT_consolidation_benefit
  }
  if (!decision_consolidate){
    GOVT_total_cost <- LC_costs
    GOVT_result_n_intervention = GOVT_total_cost
  }
}
  # NPV ###
  GOVT_NPV_intervention <- 
    discount(GOVT_result_intervention, discount_rate, calculate_NPV = T)
  
  GOVT_NPV_n_intervention <- 
    discount(GOVT_result_n_intervention, discount_rate, calculate_NPV = T)

  return(list(GOVT_Interv_NPV = GOVT_NPV_intervention,
              GOVT_No_Interv_NPV = GOVT_NPV_n_intervention,
              GOVT_NPV_decision_do = GOVT_NPV_intervention - GOVT_NPV_n_intervention,
             GOVT_Cashflow_decision_do = GOVT_result_intervention -
                                        GOVT_result_n_intervention))

  } # end of intervention loop

mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = estimate_read_csv("Input_tables/farmer_input_table.csv"),
  model_function = lc,
  numberOfModelRuns = 1e2, #100
  functionSyntax = "plainNames")

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], 
                                ncomp = 1)

mcSimulation_table <- data.frame(mcSimulation_results$x, 
                                 mcSimulation_results$y[1:3])

evpi <- decisionSupport::multi_EVPI(mc = mcSimulation_table, 
                                    first_out_var = "GOVT_Interv_NPV")

compound_figure(mcSimulation_object = mcSimulation_results, 
                input_table = read.csv("Input_tables/farmer_input_table.csv"), 
                plsrResults = pls_result, 
                EVPIresults = evpi,
                decision_var_name = "GOVT_NPV_decision_do", 
                cashflow_var_name = "GOVT_Cashflow_decision_do",
                base_size = 10)
