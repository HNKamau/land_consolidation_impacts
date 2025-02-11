# function to make variables and test the model
# used only in model construction

make_variables <- function(est,n=1)
{x <- decisionSupport::random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

# CW NOTE I had to do some work to get the separators to work here
# CW NOTE had to change ; to , and update the file somehow 
make_variables(decisionSupport::estimate_read_csv(paste("Input_tables/land_consolidation_input.csv", sep=",")))


set.seed(254)

land_consolidation_function <- function(x, varnames)
{
  # A) Benefits from Do nothing option ####
  # We use maize as the reference crop 
  # Maize production risks facing farmers and their impact on crop production benefit
  
  prop_maize_yield_lost_hazard <- chance_event(natural_hazard, 
                                               yield_loss_drought/100, 0, gen_CV, n=n_years,
                                               one_draw = FALSE)
  
  prop_maize_yield_lost_disease <- chance_event(pest_disease_risk,
                                                yield_loss_disease/100, 0, gen_CV,n=n_years, 
                                                one_draw = FALSE)
  # CW NOTE seems like a lot of losses here for input constraints.. is this againat some ideal super high yield?
  
  # HK I looked at all input related constraints as listed in this paper https://doi.org/10.1007/s12571-010-0053-8
  # HK Inclusive in the input constraint risk include issues of seed quality, fertilizer, herbicides and pesticides
  # HK on second though... the pest-disease could be considered within the input-constraint risks. 
  
  prop_maize_yield_lost_input_constraint <- 
    chance_event(farmer_inadequate_funds_risk, yield_loss_due_to_input_constraint,
                 0, gen_CV, n=n_years, one_draw = FALSE)
  
 
  prop_maize_yield_lost_management <- 
    chance_event(min(late_planting_risk, health_risk),
                 yield_loss_due_to_management, 0, gen_CV, n=n_years,
                 one_draw = FALSE)
  
  #effects of all the above risks                                                                        
  expected_impact_of_risks <- sapply(c(prop_maize_yield_lost_hazard + 
                           prop_maize_yield_lost_disease + prop_maize_yield_lost_input_constraint
                         + prop_maize_yield_lost_management),function(x) min(x,1))
  # CW NOTE farm risks is not really 'risks' but rather expected impacts of all risks.. ✅
  # CW NOTE seems rather high - 100% losses in  few cases
  # HK NOTE.. maize yield can range as low as 1.4  to 8 Mg/ha  Ngeno 2024 https://doi.org/10.1016/j.heliyon.2024.e24657
  farmer_yield_t_ha <- vv(maize_yield_t_ha, gen_CV, n_years) * no_of_seasons
  # CW NOTE expected yields 12-14 should be expectes of a high functioning maize crop.. 
  # CW NOTE here we have already a low expecation of yield so i wonder if some of the risks above are already included
  adjusted_farmer_yield_t_ha <- farmer_yield_t_ha * (1 - expected_impact_of_risks)
  # CW NOTE generally very low and sometimes none at all - can you chekc this against the 'real worls' observations
  # CW NOTE are these reasonable annual harvest numbers? 4,4,7,2,9,5,4,0,0, etc. 
  
  crop_benefit <- 
    adjusted_farmer_yield_t_ha * 
    ha_per_hh * 1000 * # conversion to kg/ha
    vv(maize_price_kes_kg, gen_CV, n_years) 
  # CW NOTE should shcck ethe numbers here and see if this is all reasonable
  # CW NOTE do we have the right shillings per kg? we might expect higher incomes
  # HK we do have right pries, in fact the price can go lower than that. 
  
  # natural capital  
  # The community values land security not only as having title deeds but also the 
  # ability to invest in perennial plants such as trees. 
  # Trees provide numerous benefits including provision of fuel wood, fruits, soil protection, among others. 
  
  adjusted_value_of_assets <-vv(value_of_farm_assets, gen_CV, n_years,relative_trend = inflation_rate)
  # CW NOTE a but fuzzy for me here.. as a farmer do I get an annual return from my assests? 
  # CW NOTE is this in the form of land gaining value?  
  # HK Response: Value of farm assets is accounted on the basis of land security. Land security is equated to perennial plants such as tree which gain value over time. 
  
  # Food cost saved from food accessible from the farm 
  annual_saved_food_cost <- vv(saved_food_cost_pm, gen_CV, n_years,relative_trend = inflation_rate) * 12 
  # CW NOTE names could be more clear for my taste - does pm mean something about the condolidation?  
  # HK pm refers to per month. The description is provided in the input table, but I can write it in full. 
  # CW NOTE if we add a relative trend to some variabvles we should be sure to be consistent
  # HK I would expect so. The amount of money spent on food increases with increasing inflation which is reflected in the numbers. 
  # CW NOTE this should also be excluded from the NPV calcualtion (not part of discounting anymore)
  # HK noted. 
  
  status_quo_benefit <- crop_benefit + adjusted_value_of_assets + annual_saved_food_cost
  # CW NOTE here I suppose if there are no yields the reare also no saved food costs 
  # HK - this is the first option where they do nothing... meaning they save food costs from getting some food items from the land. 
  # CW NOTE no yields in (several cases when I run this model)
  # I do not understand your comment here. 
  
  # B) do nothing costs ####
  
  # Medical Bills from farm related stresses
  # Active lifestyles such as exercises, working on farm. 
  # Scholars have found an association between physical activities and health costs reduction. 
  # Aoyagi & Shephard 2012 https://doi.org/10.2165/11590530-000000000-00000 (3.7%)
  # Sato et al. 2020 https://doi.org/10.1016/j.amepre.2019.12.009 0.4% in 2 years and 1% in 3 years
  
  medical_bills <- prop_hhincome_spent_on_hospital/100 * vv(hh_income_pa, gen_CV,n_years)
  
  do_nothing_medical_bills <- medical_bills * (1-pc_healthcosts_reduced_physical_activities/100) # percent saved due to active lifestyle
  # CW NOTE can there also be medical savings when people have an active lifestyle working in the field ✅
  # CW NOTE just a thought that farms can also lead to health (I think this is not unfounded)
  
  #This is something I can check 
  
  # production costs for the farm
  farming_costs <- vv(production_costs_acre, gen_CV, n_years,relative_trend = inflation_rate)*
    ha_per_hh * ha_acre_conversion * no_of_seasons
  # CW NOTE why 'saved' production costs? ✅
  
  # HK I  need to change the name... Essentially its the production cost per acre... and can also be reversed to reflect the save prdn costs. 
  
  # planning cost involves legal fee and the cost of knowledge acquisition
  planning_cost
  # CW NOTE this is a single total and is very high 
  # HK Why do you think it is high? the average hourly rate for a lawyer in Kenya is between 5000 to 20000
  # HK The stakeholders gave me this range. It includes legal feed, knowledge acquisition, stationery, and printing of documents
  # CW NOTE four times the farming_costs for all years of the scenario...✅  I have addressed this. 
  
  status_quo_cost <- do_nothing_medical_bills + farming_costs 
  status_quo_cost[1] <- status_quo_cost[1] + planning_cost
  # CW NOTE #########
  # CW NOTE related to the note above 
  # CW NOTE the farmer plan costs (40,000 shillings) is added to each year 
  # HK why is this recurring? It is a one off fee - it should not recur. I have to adjust that. ✅
  # CW NOTE + farmer_plan_costs is happening each year as a constant
  
  # C) Farmer  costs  with land consolidation ####
  
  farmer_one_time_cost <- cost_of_disruption_kes + # Damages on physical assets
                          hhupkeep_prior_to_first_payment + # HH income needed to sustain the hh prior 1st payment
                          planning_cost # planning cost involves legal fee and the cost of knowledge acquisition
  # CW NOTE maybe 'planning costs' go here? ✅
  
  
  farmer_recurring_cost <- adjusted_value_of_assets + # loss
                          annual_saved_food_cost # Food unavailable from farm directly
  
  land_consolidation_cost <-  farmer_recurring_cost 
  land_consolidation_cost[1] <- land_consolidation_cost[1] + farmer_one_time_cost 
  # CW NOTE added again here to each year of the list
  # HK (adjusted ) 
  # CW NOTE planning costs should probably only happen in year 1 ✅
  
  # D) Farmer Benefits with land consolidation ####
  # A farmer who leases their land, gets a compensation fee commensurate to their land size. 
  income_from_land_leasing <- vv(compensation_income_pm_acre, gen_CV, n_years)* 2 * 
                              ha_per_hh * ha_acre_conversion
  # CW NOTE I have not heard of lesor before ✅  changed that  to income_from_land_leasing
  # CW NOTE this is important and could be explained more ✅   
  
  #off farm employment and wages
  off_farm_income <- vv(off_farm_income_kes_pm, gen_CV, n_years) *12   # pm is per month
  # CW NOTE this is abit critical since we can imagine off farm jobs can be hard to get for farmers
  # This uncertainty is reflected in the ranges used. They are broad and span from -ve to +ve income ✅  h
  # CW NOTE depends on the education and other factors
  # CW NOTE my experience is that farmers can go to be low wage workers
  # CW NOTE not a lot of options for them
  # CW NOTE I would expect this to be lower
  
  #income saved: production costs for farming
  production_costs_saved <- farming_costs
  
  # hospital bills saved (proxy for long term health) from farm related stresses
  # we also assume that active lifestyle might reduce over time, therefore... the 
  # benefit is reduced by cost of inactivity
  do_nothing_medical_bills
  # CW NOTE all medical bills are imminiated? 
  # CW NOTE seems a bit overly hopeful - should we explain this a bit more? 
  # rather than being hopeful, I will introduce the element of inactivity ✅
  
  # social cohesion benefit
  # social cohesion is expressed as a factor of free time as a result of the the intervention. With the intervention, free time is associated with social cohesion through community participation and involvement. We quantify it by the cost of labor per hour. First, risks that threaten social cohesion are anti-social behaviors e.g crime, drug use, theft and domestic conflict.They take up time that would otherwise be used for community cooperation or activities
  
  # CW NOTE  I like that you make notes in your scripts - would be good to see more of them ✅
  # CW NOTE use indentations when writing in line notes
  social_time_risk <- max(vice_risk,domesticconflict_risk)
  
  adjusted_social_time <- vv(social_time_hr_day, gen_CV, n_years) * (1-social_time_risk)
  social_cohesion <- adjusted_social_time * vv(labour_cost_kes_hr, gen_CV, n_years, 
                                               relative_trend = inflation_rate)
  # CW NOTE Is it possible that the consolidation can be bad for cohesion? 
  # CW NOTE getting a job off farm adn all that can cause less contact
  # CW NOTE farmers who lose land and connection to land and culture
  # CW NOTE maybe this is not as black and white as this
  
  # Do you have suggestions of how this can be approached? 
  
  
  # Childhood benefit
  # Small scale farming employ family labor including children to help with farm labor which reduces the overall production costs. Scholars argue that when children are involved in such activities, they lose the opportunity to just being children and enjoy childhood. Land consolidation, allows for childhood benefit. To determine this benefit, we determined the number of hours a child(ren) contribute(s) to family farm labor weekly. The value of childhood benefit was equated to the value of hiring labor (wage/hour)
  
  childhood_benefit <- child_farm_time_hr_week * children_per_hh * n_weeks_child_farm_time *
    vv(labour_cost_kes_hr, gen_CV, n_years, relative_trend = inflation_rate ) 
  # CW NOTE generally keep direction out of varible names and values ✅
  # CW NOTE 'better' should just be beenfit or something.. this can go up or down ✅
  # CW NOTE  could also be a negative effect right? 
  # CW NOTE labor costs are income? or is this something else? childcare?
  # CW NOTE is is income for kids? They get 30ks per hour? 
  # CW NOTE why is their income added to the benefots of land consolidation?
  # HK I have added line notes this. ✅ 
  
  land_consolidation_benefit <-income_from_land_leasing + off_farm_income + production_costs_saved + 
      do_nothing_medical_bills +  social_cohesion + childhood_benefit
  
  land_consolidation_benefit[1] <-  land_consolidation_benefit[1] + sale_of_hh_items_not_needed
  # CW NOTE a bit confused here how all this is a benefit only for consolidaation
  # CW NOTE seems like some of this also exists in the baseline
  # CW NOTE agree? 
  # HK which ones particularly? ❓
  # CW NOTE perhaps we should mention that in the model and adjust values accordingly
  
  # E) Calculate net benefit ####
  status_quo_netbenefit <- status_quo_benefit - status_quo_cost
  status_quo_netbenefit_usd <- status_quo_netbenefit/currency_change
  
  land_consolidation_netbenefit <- land_consolidation_benefit - land_consolidation_cost
  land_consolidation_netbenefit_usd <- land_consolidation_netbenefit/currency_change

  
  # NPV ####
  NPV_land_consolidation_netbenefit <- discount(land_consolidation_netbenefit, discount_rate, calculate_NPV = TRUE)
  NPV_land_consolidation_netbenefit_usd <- discount(land_consolidation_netbenefit_usd, discount_rate, calculate_NPV = T)
  
  
  NPV_status_quo_netbenefit <- discount(status_quo_netbenefit, discount_rate, calculate_NPV = TRUE)
  NPV_status_quo_netbenefit_usd <- discount(status_quo_netbenefit_usd, discount_rate, calculate_NPV = T)
  
  
  return(list(benefit_lc_kes = NPV_land_consolidation_netbenefit,
              benefit_status_quo_kes = NPV_status_quo_netbenefit,
              net_benefit_lc_kes = NPV_land_consolidation_netbenefit - NPV_status_quo_netbenefit, 
              benefit_lc_usd = NPV_land_consolidation_netbenefit_usd,
              benefit_status_quo_usd = NPV_status_quo_netbenefit_usd,
              net_benefit_lc_usd = NPV_land_consolidation_netbenefit_usd - NPV_status_quo_netbenefit_usd,
              Cashflow_decision_do = land_consolidation_netbenefit - status_quo_netbenefit))
}
