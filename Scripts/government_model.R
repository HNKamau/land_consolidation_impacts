# GOVERNMENT COSTS AND BENEFITS

lc <- function(x, varnames)
  {

political_interf_event <- chance_event(political_interference,1,0,n = 1)
# inadequate_funds_event <- chance_event(inadequate_funds, 1, 0, n=1)
farmer_nonpopinvol_event <- chance_event(intervention_nonpopInvolv, 1, 0, n=1)

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
  
actual_public_awareness <- public_awareness * (1+ more_money_due_to_politicalinterf)
  
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
total_ha <- ha_per_hh * total_hhs  #target total land size
# only 95% of total ha is for crop production
crop_land <- round(total_ha * 0.95)

land_prep_cost <- land_prep_per_acre / ha_acre_conversion * crop_land # land survey and leveling
machinery_cost <- farm_infrastructure + farm_machinery_infrastructure # buildings,tractors, harrows, seeders, cars etc
irrigation_cost <- (irrigation_infrastructure_per_ha * crop_land) #irrigation equipment
initial_cost <- land_prep_cost + machinery_cost + irrigation_cost

  ## Operations cost 
  # maintenance cost of machinery and irrigation infrastructure
  # initialize maintenance cost by zeros through out the years
  maintenance_cost_annual <- rep(0, n_years) 
  
  #setting the maintenance cost of second year
  maintenance_cost_annual [2] <-
    farm_machinery_maintence_percent/100 * farm_machinery_infrastructure +
    irrigation_maitenance_percent/100 *  irrigation_infrastructure_per_ha * 
    crop_land
  
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
  all_labour_cost <- vv(all_labour, gen_CV, n_years) # (un)skilled labour
  
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
  
  crop_loss_drought <- natural_hazard_event * vv(yield_loss_drought/100, 
                                                 gen_CV, n_years)
  crop_loss_disease <- pest_disease_event * vv(yield_loss_disease/100, 
                                               gen_CV, n_years)
  
  effect_of_crop_risks <- sapply(c(crop_loss_drought + crop_loss_disease),
                                 function(x) min(x,1))
  
  crop_yield_produced <- vv(maize_yield_t_ha, gen_CV, n_years) * crop_land *
                        no_of_seasons 
  actual_yield_t_ha <- crop_yield_produced * (1-effect_of_crop_risks)
  
 rainfed_revenue <- 
    (actual_yield_t_ha * 1000 * # change to kg/ha
    vv(price_maize_per_kg, gen_CV, n_years))/currency_change
  
  #benefit of scale operation, irrigation
  return_due_machinery <- actual_yield_t_ha *
                            vv(machinery_technical_efficiency/100, 
                               gen_CV, n_years) *
                            vv(price_maize_per_kg, gen_CV, n_years)

  return_due_irrigation <- actual_yield_t_ha *  
                            vv(yield_irrigation_factor/100, gen_CV, n_years) *
                            vv(price_maize_per_kg, gen_CV, n_years)
  
  crop_revenue <- rainfed_revenue + return_due_machinery + return_due_irrigation
  
  
# Land use planning and segregation of all the areas near water ways
# for example rivers: which is approximately 5 % of the total land
# planting of trees  and grass in these areas 
  biodiversity_land <- round(5/100 * total_ha) 
  
  # cost of establishing and maintaining mangoes
  mango_establishment_cost <- mango_establishment_cost_kes_acre * 
                            ha_acre_conversion * biodiversity_land
  mango_maintenance_cost <- vv(mango_maintenance_cost_kes_acre, gen_CV, n_years) *
                            ha_acre_conversion * biodiversity_land
  
  mango_total_cost <- mango_establishment_cost + mango_maintenance_cost
  # recommended spacing is 10m by 10m 
# 1 ha is to 10000 m^2
# n_mango_trees <- biodiversity_land/0.01

  mango_yield <- gompertz_yield(max_harvest = maximum_harv,
                                time_to_first_yield_estimate = 5,
                                time_to_second_yield_estimate = 7,
                                first_yield_estimate_percent = first_yield_estimate/100 ,
                                second_yield_estimate_percent = second_yield_estimate/100,
                                n_years = n_years,
                                var_CV = gen_CV, 
                                no_yield_before_first_estimate = TRUE)
  # Applying risks to mango yield and benefits
  mango_disease_event <- chance_event(pest_disease_risk,
                                      value_if = yield_loss_disease/100,
                                      value_if_not = 0,
                                      n = n_years, 
                                      CV_if = gen_CV,
                                      CV_if_not = 0,
                                      one_draw = FALSE)
  
  mango_drought_event <- chance_event(natural_hazard, 
                                      value_if = yield_loss_drought/100,
                                      value_if_not = 0,
                                      n = n_years,
                                      CV_if = gen_CV, 
                                      CV_if_not = 0, 
                                      one_draw = FALSE)
  effect_of_risks_mango <- sapply(mango_disease_event+mango_drought_event, 
                                  function(x) min(1,x))
  actual_mango_yield <- mango_yield * (1- effect_of_risks_mango)
  mango_returns <- actual_mango_yield * 
                  (biodiversity_land * trees_per_ha) * #total mango trees
                  price_mango_fruit
  
  # The uncultivated space between mangoes is estimated to be 75% of the areas
  # https://www.researchgate.net/publication/287277939_Farmer_to_farmer_spread_of_fodder_crops-an_analysis_on_mango_orchards_in_south_India
  # To calculate the hay intercrop 
  grass_areas <- biodiversity_land * proportion_of_uncultivated_land
  # costs: Establishment and maintenance
  # https://cgspace.cgiar.org/server/api/core/bitstreams/e71b06ce-8e33-4286-88f5-71214e5b5a98/content
  grass_establishment_cost <- (grass_production_cost_ha * grass_areas) 
  grass_maintenance_cost <- vv(grass_maintenance_kes_ha_year, gen_CV, n_years) * 
                            grass_areas
  grass_total_cost <- grass_establishment_cost + grass_maintenance_cost
  
  grass_biomass_yield_ton <- grass_areas * vv(grass_yield_t_ha_year,gen_CV, n_years)
  # Using an example of 6 common resilient grasses in Kenya: 
  #introduce risks like drought
  grass_biomass_lost_drought <- chance_event(natural_hazard,
                                             value_if = yield_loss_drought/100,
                                             value_if_not = 0,
                                             n = n_years, 
                                             CV_if = gen_CV,
                                             CV_if_not = 0,
                                             one_draw = FALSE)
  adjusted_grass_biomass_yield_t <- (1 - grass_biomass_lost_drought) *
                    grass_biomass_yield_ton
# Bales produces
  n_bales <- (adjusted_grass_biomass_yield_t * 1000)/hay_bale_kg # change yield to kg
  hay_returns <- n_bales * vv(price_hay_bale, gen_CV, n_years)
  
 # benefit the amount of carbon sequestered over time on agricultural land
 # Apply risks to carbon sequestration
 #   1) wrong crop choice which can lead to low agrobiodiversity
 #   2) carbon market failure
 #   3) drought risk - less biomass to sequester carbon 
 #   4) Impact of large scale farming on degradation ( here we calculate the 
 #      risk on carbon loss in soil per hectare). The key drivers of 
 #     degradation would be nutrient use and traction from machinery
 

# calculate permanent biomass (grass and mango trees)

mango_biomass_stock_ton <- vv(biomass_mango_t_ha, gen_CV, n_years) *
                        biodiversity_land


# Risks related to biomass loss e.g., drought (natural hazard), diseases
# adjust the biodiversity for that.. 
biomass_loss_drought <- vv(mango_biomass_stock_ton, gen_CV, n_years) * 
                        mango_drought_event # drought
biomass_loss_diseases <- vv(mango_biomass_stock_ton, gen_CV, n_years) *
                          mango_disease_event  
adjusted_mango_biomass_stock_ton <- mango_biomass_stock_ton - 
                        (biomass_loss_drought +  biomass_loss_diseases)

# Biomass from grass. 
# with 75 % defoliation rate - at least 25% standing aboveground matter 
abg_grass <- adjusted_grass_biomass_yield_t * 0.25
# below ground matter of grasses
bgb_grass_t <- vv(bgb_grass_t_ha, gen_CV, n_years) * grass_areas
# Drought can decrease the root biomass of grassess 
# https://doi.org/10.1016/j.scitotenv.2023.166209
# applying risk to root biomass
reduced_grassroot_biomass <- chance_event(natural_hazard,root_biomass_reduced/100,
                                           0, n_years, gen_CV)

adjusted_bgb_grass_t <- (1-reduced_grassroot_biomass) * bgb_grass_t

total_carbon_stock <- (adjusted_bgb_grass_t + adjusted_mango_biomass_stock_ton) * 
                      biomass_carbon_conversion_factor
# adjust the carbon price based on the risk of carbon markets
carbon_market_event <- chance_event(carbon_market_risk,
                                    carbon_price_reduction/100, 0, n_years)

carbon_price_kes <- vv(carbon_price, gen_CV, n_years) * currency_change
carbon_price_kes <- carbon_price_kes * (1- carbon_market_event)

carbon_benefit <- total_carbon_stock * carbon_price_kes

LC_benefit <- (crop_revenue + mango_returns + 
               hay_returns + carbon_benefit)/
              currency_change

  } else {
  LC_benefit <- 0
}
  # Calculate net benefit ####
  
  
  if(decision_consolidate){
    Implementer_consolidation_benefit <- LC_benefit - (LC_costs + 
                                ((mango_total_cost + grass_total_cost)/
                                   currency_change))
                                                     
    Implementer_result_intervention = Implementer_consolidation_benefit
  }
  if (!decision_consolidate){
    Implementer_total_cost <- LC_costs + ((mango_total_cost + grass_total_cost)/
                                            currency_change)
    Implementer_result_n_intervention = Implementer_total_cost
  }
}
  # NPV ###
crop_NPV <- discount(crop_revenue, discount_rate, calculate_NPV = TRUE)

mango_NPV <- discount(mango_returns, discount_rate, calculate_NPV = TRUE)

hay_NPV <-discount(hay_returns, discount_rate, calculate_NPV = TRUE)

carbon_benefit_NPV <- discount(carbon_benefit, discount_rate, calculate_NPV = T)

Implementer_NPV_intervention <- discount(Implementer_result_intervention,
                                         discount_rate, calculate_NPV = T)

Implementer_NPV_n_intervention <- discount(Implementer_result_n_intervention, 
                                           discount_rate, calculate_NPV = T)

  return(list(Implementer_crop_NPV  = crop_NPV,
              Implementer_mango_NPV = mango_NPV,
              Implementer_hay_NPV = hay_NPV,
              Implementer_carbon_benefit_NPV = carbon_benefit_NPV,
              Implementer_Interv_NPV = Implementer_NPV_intervention,
              Implementer_NPV_decision_do = Implementer_NPV_intervention - 
                                            Implementer_NPV_n_intervention,
             Implementer_Cashflow_decision_do = Implementer_result_intervention -
                                        Implementer_result_n_intervention))

  } # end of intervention loop

mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = estimate_read_csv("Input_tables/farmer_input_table.csv"),
  model_function = lc,
  numberOfModelRuns = 1e2, #100
  functionSyntax = "plainNames")

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[6],
                                ncomp = 1)

mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y)

evpi <- decisionSupport::multi_EVPI(mc = mcSimulation_table,
                                    first_out_var = "Implementer_crop_NPV")

compound_figure(mcSimulation_object = mcSimulation_results,
                input_table = read.csv("Input_tables/farmer_input_table.csv"),
                plsrResults = pls_result,
                EVPIresults = evpi,
                decision_var_name = "Implementer_NPV_decision_do",
                cashflow_var_name = "Implementer_Cashflow_decision_do",
                base_size = 10)
