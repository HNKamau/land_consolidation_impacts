# 
# library(decisionSupport)
# library(tidyverse)
# library(ggplot2)
# 
# set.seed(254)
# 
# source(file = "make_variables.R")
# make_variables(estimate_read_csv("farmerLC_size0.csv",
#                                  sep = ";"))
# 
# farmer <- function(x, varnames)
# {

  # A) farmer benefits without intervention ####
  # We use maize equivalent of farm yield and the value of natural capital (trees) 
  # to calculate pre intervention benefits that a farmer has and will have during 
  # the project period. 
  
  # ex-ante risks to farmers crops
  natural_hazard
  pest_disease_risk
  farmer_inadequate_funds_risk
  late_planting_risk
  health_risk
  
  prop_maize_yield_lost_hazard <- chance_event(natural_hazard, 
                                               yield_loss_drought/100, 0, gen_CV, n=n_years, one_draw = FALSE)
  prop_maize_yield_lost_disease <- chance_event(pest_disease_risk,
                                                yield_loss_disease/100, 0, gen_CV,n=n_years, one_draw = FALSE)
  prop_maize_yield_lost_input_constraint <- 
    chance_event(farmer_inadequate_funds_risk, yield_loss_due_to_input_constraint,
                 0, gen_CV, n=n_years, one_draw = FALSE)
  prop_maize_yield_lost_management <- 
    chance_event(min(late_planting_risk, health_risk),
                 yield_loss_due_to_management, 0, gen_CV, n=n_years, one_draw = FALSE)
  
  #effects of all the above risks                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
  farm_risks <- sapply(c(prop_maize_yield_lost_hazard + 
                           prop_maize_yield_lost_disease + prop_maize_yield_lost_input_constraint
                         + prop_maize_yield_lost_management),function(x) min(x,1))
  
  farmer_yield_t_ha <- vv(maize_yield_t_ha, gen_CV, n_years) * no_of_seasons
  
  adjusted_farmer_yield_t_ha <- farmer_yield_t_ha * (1 - farm_risks)
  
  
  crop_benefit <- 
    adjusted_farmer_yield_t_ha *  ha_per_hh * 1000 * # conversion to kg/ha
    vv(low_maize_price_kes_kg, gen_CV, n_years) 
  
  # natural capital  
  adjusted_value_of_assets <-vv(value_of_farm_assets, gen_CV, n_years,
                                relative_trend = inflation_rate)
  
  # Food cost saved from food accessible from the farm 
  annual_saved_food_cost <- vv(saved_food_cost_pm, gen_CV, n_years, 
                               relative_trend = inflation_rate) * 12 
  
  farmer_pre_interv_benefit <- crop_benefit + adjusted_value_of_assets +
    annual_saved_food_cost
  
  # B) Cost of the farmer before the intervention ####
  # medical Bills from farm related stresses
  medical_bills <- prop_hhincome_spent_on_hospital/100 * 
    vv(hh_income_pa, gen_CV,n_years)
  
  # production costs for the farm
  farming_costs <- vv(production_costs_saved_acre, gen_CV, n_years, 
                      relative_trend = inflation_rate)*
    ha_per_hh * ha_acre_conversion * no_of_seasons
  
  # planning cost involves legal fee and the cost of knowledge acquisition
  farmer_plan_cost  <- planning_cost
  
  farmer_pre_interv_cost <- medical_bills + farming_costs + farmer_plan_cost
  
  # C) Farmer  costs  with Intervention ####
  farmer_one_time_cost <- cost_of_disruption_kes + # Damages on physical assets
    hhupkeep_prior_to_first_payment 
  # HH income needed to sustain the hh prior 1st payment
  
  # planning cost involves legal fee and the cost of knowledge acquisition
  farmer_plan_cost  <- planning_cost
  
  farmer_recurring_cost <- adjusted_value_of_assets + # loss
    annual_saved_food_cost # Food unavailable from farm directly
  
  farmer_cost_interv <-  farmer_one_time_cost +  farmer_plan_cost +
    farmer_recurring_cost
  
  # D) Farmer Benefits with Intervention ####
  # farmer's annual compensation aka lessor fee
  lessor_fee02 <-vv(compensation_income2_pm_acre, gen_CV, n_years)* 2 * 
    ha_per_hh * ha_acre_conversion
  
  #off farm employments and wages
  off_farm_income <-vv(off_farm_income_kes_pm, gen_CV, n_years) *12
  
  #income saved: production costs for farming
  production_costs_saved <- farming_costs
  # hospital bills saved (proxy for long term health) from farm related stresses
  medical_bills_saved <- medical_bills  
  
  # social cohesion benefit
  # social cohesion is expressed as a factor of free time as a result of the 
  # the intervention. With the intervention, free time is associated with 
  # social cohesion through community participation and involvement. 
  # We quantify it by the cost of labour per hour
  
  # First, risks that threaten social cohesion are vices e.g crime, drug use,
  # theft and domestic conflict - They take up time that would otherwise be used 
  # for community cooperation or activities
  
  social_time_risk <- max(vice_risk,
                          domesticconflict_risk)
  
  adjusted_social_time <- vv(social_time_hr_day, gen_CV, n_years) *
    (1-social_time_risk)
  social_cohesion <- adjusted_social_time * vv(labour_cost_kes_hr, gen_CV, 
                                               n_years, relative_trend = inflation_rate)
  
  # Better childhood benefit = long term benefit is education
  # quantified by child's contribution to family farm labour weekly
  # and the value of hired labour
  
  better_childhood <- child_farm_time_hr_week * children_per_hh * 
    n_weeks_child_farm_time *
    vv(labour_cost_kes_hr, gen_CV, n_years, 
       relative_trend = inflation_rate ) 
  
  farmer_benefit_interv02 <-
    lessor_fee02 + off_farm_income + production_costs_saved + 
    sale_of_hh_items_not_needed +  medical_bills_saved + social_cohesion + 
    better_childhood
  
  # E) Calculate net benefit ####
  farmer_pre_interv_netbenefit_kes <- farmer_pre_interv_benefit -
    farmer_pre_interv_cost
  
  result_pre_intervention <- farmer_pre_interv_netbenefit_kes/currency_change
  
  farmer_interv_netbenefit_kes02 <- farmer_benefit_interv02 - farmer_cost_interv
  
  result_intervention <- farmer_interv_netbenefit_kes02/currency_change
  
  # Calculate categorized costs and benefits ####
  # Categories include 1) Food costs not saved 
  food_cost_saved_npv <- discount(annual_saved_food_cost, discount_rate,
                                  calculate_NPV = TRUE)
  # 2) Natural assets lost 
  natural_assets_npv <- discount(adjusted_value_of_assets, discount_rate,
                                 calculate_NPV = T)
  # 3) Planning cost 
  # planning_npv <- farmer_plan_cost
  # 4) Household income prior 1st payout  + 5) Disruption cost
  # one_time_npv <- farmer_one_time_cost
  # 6) Compensation for land
  lease_npv02 <- discount(lessor_fee02, discount_rate, calculate_NPV = T)
  # 7) Production costs saved
  prdn_costs_npv <- discount(farming_costs, discount_rate, calculate_NPV = T)
  # 8) Alternative employment
  alt_income_npv <- discount(off_farm_income, discount_rate, calculate_NPV = T)
  # 9) Better childhood
  better_childhood_npv <- discount(better_childhood, discount_rate,
                                   calculate_NPV = T)
  # 10) Social cohesion
  social_npv <- discount(social_cohesion, discount_rate, calculate_NPV = T)
  # 11) Medical bills saved
  medical_npv <- discount(medical_bills_saved, discount_rate, calculate_NPV = T)
  # 12) Annual crop yield
  crop_npv <- discount(crop_benefit, discount_rate, calculate_NPV = T)
  
  
  # NPV ####
  NPV_interv_kes02 <- discount(farmer_interv_netbenefit_kes02, discount_rate,
                             calculate_NPV = TRUE)
  NPV_intervention02 <- 
    discount(result_intervention, discount_rate, calculate_NPV = T)
  
  NPV_n_interv_kes02 <- discount(farmer_pre_interv_netbenefit_kes, discount_rate,
                               calculate_NPV = TRUE)
  
  NPV_n_intervention02 <- 
    discount(result_pre_intervention, discount_rate, calculate_NPV = T)
  
  # Benefit cost ratio ####
  # without intervention
  npv_farmer_pre_interv_benefit <- discount(farmer_pre_interv_benefit, discount_rate,
                                            calculate_NPV = T)
  
  npv_farmer_pre_interv_cost <- discount(farmer_pre_interv_cost, discount_rate,
                                         calculate_NPV = T)
  bcr_n_interv <-npv_farmer_pre_interv_benefit/npv_farmer_pre_interv_cost
  # with intervention
  npv_farmer_benefit_interv <- discount(farmer_benefit_interv02, discount_rate, 
                                        calculate_NPV = T)
  npv_farmer_cost_interv <- discount(farmer_cost_interv, discount_rate, 
                                     calculate_NPV = T)
  bcr_interv <- npv_farmer_benefit_interv/npv_farmer_cost_interv
  
#   return(list(Interv_NPV02 = NPV_intervention02,
#               Interv_NPV_kes02 = NPV_interv_kes02,
#               No_Interv_NPV02 = NPV_n_intervention02,
#               No_Interv_NPV_kes02 = NPV_n_interv_kes02,
#               NPV_decision_kes02 = NPV_interv_kes02 - NPV_n_interv_kes02,
#               NPV_decision_do = NPV_intervention02 - NPV_n_intervention02,
#               BCR_no_interv = bcr_n_interv,
#               BCR_interv = bcr_interv,
#               Food_money_saved = food_cost_saved_npv,
#               Natural_assets = natural_assets_npv,
#               Planning = farmer_plan_cost,
#               Disruption_cost = farmer_one_time_cost,
#               Lease = lease_npv02,
#               Production_costs = prdn_costs_npv,
#               Alternative_income = alt_income_npv,
#               Childhood = better_childhood_npv,
#               Social = social_npv,
#               Medical = medical_npv,
#               Annual_yield = crop_npv,
#               Cashflow_decision_s0l2 = farmer_interv_netbenefit_kes02 -
#                 farmer_pre_interv_netbenefit_kes))
# }
# 
# # Model runs ####
# mcSimulation_results <-  decisionSupport::mcSimulation(
#   estimate = estimate_read_csv("farmerLC_size0.csv",sep =";"),
#   model_function = farmer,
#   numberOfModelRuns = 1e2, #10000
#   functionSyntax = "plainNames")
# 
# # Outcome distribution ####
# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
#                                     vars = c("Interv_NPV02", "No_Interv_NPV02"),
#                                     method = 'smooth_simple_overlay',
#                                     base_size = 8)
# 
# decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
#                                     vars = "NPV_decision_do",
#                                     method = 'boxplot_density')

# # decisionSupport::plot_cashflow(mcSimulation_object = mcSimulation_results, 
# #                                cashflow_var_name = "Cashflow_decision_s0l2")
# 
# # Variable Importance ####
# pls_result <- plsr.mcSimulation(object = mcSimulation_results,
#                                 resultName = names(mcSimulation_results$y)[7],
#                                 ncomp = 1)
# #write.csv(pls_result, 'Outcome/pls.csv')
# decisionSupport::plot_pls(pls_result,
#   input_table = read.csv("farmerLC_size0.csv", sep=";"),
#   threshold = 1)
# 
# ggsave("Graph/Farmer/vip.png", width = 7, height = 5, units = 'in', dpi = 300)
# 
# # Information value ####
# mcSimulation_table <- data.frame(mcSimulation_results$x,
#                                  mcSimulation_results$y[1:6])
# evpi <- decisionSupport::multi_EVPI(mc = mcSimulation_table,
#                                     first_out_var = "Interv_NPV")
# write.csv(evpi,'Outcome/evpi.csv')
# decisionSupport::plot_evpi(evpi, 
#                            decision_vars = "NPV_decision_kes",
#                            input_table =read.csv("farmerLC_size0.csv", sep=";"),
#                            bar_color = "#1a80bb") 
#   
# 
# # Compound figure ####
# compound_figure(mcSimulation_object = mcSimulation_results,
#                 input_table = read.csv("farmerLC_size0.csv", sep =";"),
#                 plsrResults = pls_result,
#                 EVPIresults = evpi,
#                 decision_var_name = "NPV_decision_kes",
#                 cashflow_var_name = "Cashflow_decision_s0l2",
#                 base_size = 7)
# 
# 
# 
# # post_analysis ####
# outcome <- as.data.frame(mcSimulation_results$y)
# theXs <- as.data.frame(mcSimulation_results$x)
# str(outcome)
# 
# outcome <- outcome %>% mutate(NPV_per_ha = Interv_NPV_kes/ha_per_hh, 
#                               No_NPV_per_ha = No_Interv_NPV_kes/ha_per_hh,
#                               NPV_per_ha_decision = NPV_decision_kes/ha_per_hh )
# write.csv(outcome, 'Outcome/farmer_mcresults.csv')
# write.csv(theXs, 'Outcome/farmer_mcresults_x.csv')
# 
# 
# # plotting ####
# 
# outcome <- read.csv('Outcome/farmer_mcresults.csv')
# 
# ##ranges
# ranges <- outcome %>% 
#   select(NPV_per_ha, No_NPV_per_ha, NPV_per_ha_decision) %>% 
#   pivot_longer(cols = everything(), 
#                names_to = "variables", 
#                values_to = "project_outcome") %>%
#   group_by(variables) %>%
#   summarise(
#     min_value = min(project_outcome),
#     max_value = max(project_outcome)
#   )
# 
# print(ranges)
# 
# ## Percentages ####
# 
# percentages <- outcome %>% 
#   select(NPV_per_ha, No_NPV_per_ha, NPV_per_ha_decision) %>% 
#   pivot_longer(cols = everything(), 
#                names_to = "variables", 
#                values_to = "project_outcome") %>%
#   group_by(variables) %>%
#   summarise(
#     negative = sum(project_outcome < 0, na.rm = TRUE) / n() * 100,
#     positive = sum(project_outcome >= 0, na.rm = TRUE) / n() * 100
#   )
# 
# print(percentages)
# 
# 
# ##bounds KES per hectare
# percentiles <- outcome %>% 
#   select(NPV_per_ha, No_NPV_per_ha, NPV_per_ha_decision) %>% 
#  pivot_longer(cols = everything(), 
#               names_to = "variables", 
#               values_to = "project_outcome") %>%
#  group_by(variables) %>%
#  summarise(
#      lower_bound = quantile(project_outcome, 0.05, na.rm = TRUE),
#      upper_bound = quantile(project_outcome, 0.95, na.rm = TRUE))
# print(percentiles)
# 
# ##bounds USD ()
# percentiles <- outcome %>% 
#   select(Interv_NPV, No_Interv_NPV, NPV_decision_do) %>% 
#   pivot_longer(cols = everything(), 
#                names_to = "variables", 
#                values_to = "project_outcome") %>%
#   group_by(variables) %>%
#   summarise(
#     lower_bound = quantile(project_outcome, 0.05, na.rm = TRUE),
#     upper_bound = quantile(project_outcome, 0.95, na.rm = TRUE))
# print(percentiles)
# 
# 
# # BCR bounds ###
# bcr_percentiles <- outcome %>% 
#   select(BCR_interv, BCR_no_interv) %>% 
#   pivot_longer(cols = everything(), 
#                names_to = "variables", 
#                values_to = "bcr") %>%
#   group_by(variables) %>%
#   summarise(
#     lower_bound = quantile(bcr, 0.05, na.rm = TRUE),
#     upper_bound = quantile(bcr, 0.95, na.rm = TRUE))
# print(bcr_percentiles)
# 
# ### Normalised by hectare (ha_per_hh) ########
# farmer2.1 <- outcome %>% 
#   select( NPV_per_ha, No_NPV_per_ha, NPV_per_ha_decision) %>% 
#   pivot_longer(., cols =c( "NPV_per_ha", "No_NPV_per_ha",
#                           "NPV_per_ha_decision"),
#                names_to = "variables", values_to = "project_outcome") %>% 
#   
#   group_by(variables) %>%
#   mutate(lower_bound = quantile(project_outcome, 0.05, na.rm = T), # 95%
#          upper_bound = quantile(project_outcome, 0.95, na.rm = T)) %>%
#   filter(project_outcome >= lower_bound & project_outcome <= upper_bound) %>%
#   ungroup() %>%
#   mutate(variables = factor(variables, 
#                             levels = c("NPV_per_ha", "No_NPV_per_ha", 
#                                        "NPV_per_ha_decision"))) %>%  #fix the order
#   ggplot(aes(y=project_outcome/1e6, x = variables, fill = variables))+
#   geom_boxplot(width = .25, alpha = 0.7) +
#   
#   labs(y =" NPV per ha in million  Kes", x= "")+
#   scale_x_discrete(labels = c( "NPV_per_ha" = "A","No_NPV_per_ha" = "B",
#                               "NPV_per_ha_decision" = "C")) +
#   scale_fill_manual(name = "",
#                     labels = c(" (A) NPV per ha with intervention",
#                                " (B) NPV per ha without intervention",
#                                " (C) A - B"),
#                     values = c("turquoise", "deeppink", "purple"))+
#   theme_bw()
# farmer2.1 + theme(legend.position = c(0.3, 0.9),
#                   legend.justification = c("left", "top"))
# 
# ggsave('Graph/Farmer/NPV_per_ha_boxplot.png', width = 7, height = 5, 
#        units = 'in', dpi = 300)
# 
# ## geom density for the normalised results.. 
# farmer3.1 <- outcome %>% 
#   select (NPV_per_ha, No_NPV_per_ha, NPV_per_ha_decision) %>% 
#   pivot_longer(cols = c("NPV_per_ha", "No_NPV_per_ha", "NPV_per_ha_decision"),
#                names_to = "variables", values_to = "project_outcome") %>% 
#   group_by(variables) %>%
#   mutate(lower_bound = quantile(project_outcome, 0.005, na.rm = TRUE), # 99% 
#          upper_bound = quantile(project_outcome, 0.995, na.rm = TRUE)) %>%
#   filter(project_outcome >= lower_bound & project_outcome <= upper_bound) %>%
#   ungroup() %>%
#   mutate(variables = factor(variables, 
#                             levels = c("NPV_per_ha", "No_NPV_per_ha", 
#                                        "NPV_per_ha_decision"))) %>%  #fix the order
#   ggplot(aes(x = project_outcome / 1e6, colour = variables, fill = variables)) +
#   geom_density(alpha = 0.25) +
#   labs(y = "Probability density", x = "NPV per ha in million Kes") +
#   scale_fill_manual(name = "",
#                     labels = c(" (A) NPV per ha with intervention",
#                                " (B) NPV per ha without intervention",
#                                " (C) A - B"),
#                     values = c("turquoise", "orange", "blue")) +
#   scale_colour_manual(name = "",
#                       labels = c(" (A) NPV per ha with intervention",
#                                  " (B) NPV per ha without intervention",
#                                  " (C) A - B"),
#                       values = c("turquoise", "orange", "blue")) +
#   theme_light() +
#   theme(legend.position = c(0.9, 0.9),
#         legend.justification = c("right", "top"))
# 
# # Print the plot
# print(farmer3.1)
# ggsave('Graph/Farmer/NPV_per_ha.png', width = 7, height = 5, units = 'in',
#        dpi = 300)
# 
# 
# # Smooth overlay #### (Normalised with and without intervention NPV per ha)
# farmer1.2 <- outcome %>%
#   select(NPV_per_ha, No_NPV_per_ha) %>%
#   pivot_longer(., cols = c("NPV_per_ha", "No_NPV_per_ha"),
#                names_to = "variables", values_to = "project_outcome") %>%
#   group_by(variables) %>%
#   mutate(lower_bound = quantile(project_outcome, 0.025, na.rm = T), # 95%
#          upper_bound = quantile(project_outcome, 0.975, na.rm = T)) %>%
#   filter(project_outcome >= lower_bound & project_outcome <= upper_bound) %>%
#   ungroup() %>%
#   mutate(variables = factor(variables, 
#                             levels=c('NPV_per_ha', 'No_NPV_per_ha'))) %>% 
#   ggplot(aes(x = project_outcome/1e6, colour = variables, fill = variables))+
#   geom_density(alpha = .25) +
#   coord_cartesian(xlim = c(-50, 550)) +
#   labs(x = "NPV per hectare in Million KES", y = "Probability density")+
#   scale_colour_manual(name = "",
#                     labels = c(" Benefit with intervention",
#                                " Benefit without intervention"),
#                     values = c("turquoise", "deeppink"))+
#   scale_fill_manual(name = "",
#                       labels = c(" Benefit with intervention",
#                                  " Benefit without intervention"),
#                       values = c("turquoise", "deeppink"))+
#   theme_bw()
# farmer1.2<- farmer1.2 + theme(legend.position = c(0.8, 0.9),
#                   legend.justification = c("right", "top"),
#                   axis.text = element_text(size = 14, face = 'bold'),
#                   axis.title = element_text(size = 16, face = 'bold'),
#                   legend.text = element_text(size = 15))
# print(farmer1.2)
# 
# ggsave('Graph/Farmer/interventions.png', width = 7, height = 5, units = 'in',
#        dpi = 300)
# 
# # NPV per ha for with and without intervention ####
# farmer2.1 <- outcome %>% 
#   select(NPV_per_ha, No_NPV_per_ha) %>% 
#   pivot_longer(., cols =c("NPV_per_ha", "No_NPV_per_ha"),
#                names_to = "variables", values_to = "project_outcome") %>% 
#   group_by(variables) %>%
#   mutate(lower_bound = quantile(project_outcome, 0.05, na.rm = T), # 90%
#          upper_bound = quantile(project_outcome, 0.95, na.rm = T)) %>%
#   filter(project_outcome >= lower_bound & project_outcome <= upper_bound) %>%
#   ungroup() %>%
#   mutate(variables = factor(variables, 
#                             levels=c('NPV_per_ha', 'No_NPV_per_ha'))) %>%
#   ggplot(., aes(y=project_outcome/1e6,x= variables, fill = variables)) +
#   geom_boxplot(width = 0.2) +
#   labs(y =" NPV per hectare in Million KES", x = "")+
#   scale_x_discrete(labels = c( "NPV_per_ha" = "A","No_NPV_per_ha" = "B")) +
#   scale_fill_manual(name="",
#                     labels = c("  (A) Benefit with intervention", 
#                                "  (B) Benefit without intervention"),
#                     values = c("turquoise", "deeppink"))+
#   theme_bw()
# farmer2.1 <- farmer2.1 + theme(legend.position = c(0.95, 0.8),
#                   legend.justification = c("right", "top"),
#                   axis.text = element_text(size = 14, face = "bold"),
#                   axis.title = element_text(size = 16, face = "bold"),
#                   legend.text = element_text(size = 15))
# print(farmer2.1)
# 
# ggsave('Graph/Farmer/interventions_boxplot.png', width = 7, height = 5, 
#        units = 'in', dpi = 300)
# 
# # NPV difference of with and without intervention ####
# outcome %>% 
#   select(NPV_decision_kes) %>% 
#   pivot_longer(., cols ="NPV_decision_kes",
#                names_to = "variables", values_to = "project_outcome") %>% 
#   ggplot(aes(x=project_outcome/1e6)) +
#   geom_histogram(aes(fill = project_outcome>0),
#                 breaks=seq(-250,800, by=2))+
#   #geom_density(alpha=.5) +
#   guides(fill = FALSE)+
#   labs(x =" Project outcome in Million Kes", y= "Frequency")+
#   annotate(geom = "text",label ="25.95 %", x = -150, y = 60,color = "black") +
#   annotate(geom = "text", label ="74.05 %", x = 280, y = 60,color = "black") +
#   theme_light()
# 
# # NPV decision do. - normalised per ha ######
# percentages <- outcome %>% 
#   select(NPV_per_ha_decision) %>% 
#   summarise(
#     negative = sum(NPV_per_ha_decision < 0, na.rm = TRUE) / n() * 100,
#     positive = sum(NPV_per_ha_decision >= 0, na.rm = TRUE) / n() * 100
#   )
# 
# negative_percent <- round(percentages$negative, 2)
# positive_percent <- round(percentages$positive, 2)
# 
# custom_colors <- c("negative" = "#1a80bb", "positive" = "#ea801c") #orange
# 
# outcome %>% 
#   mutate(outcome_category = ifelse(NPV_per_ha_decision >= 0, "positive", 
#                                    "negative")) %>% 
#   select(NPV_per_ha_decision, outcome_category) %>% 
#   pivot_longer(., cols ="NPV_per_ha_decision",
#                names_to = "variables", values_to = "project_outcome") %>% 
#   ggplot(aes(x = project_outcome / 1e6, fill = outcome_category)) +
#   geom_histogram(breaks = seq(-250, 800, by = 2)) +
#   scale_fill_manual(values = custom_colors) +
#   guides(fill = FALSE)+
#   labs(x =" NPV per hectare in Million KES", y= "Frequency")+
#   annotate(geom = "text",label = paste0(negative_percent, " %"), x = -150,
#            y = 60,color = "black", size = 6) +
#   annotate(geom = "text", label = paste0(positive_percent, " %"), x = 280,
#            y = 60,color = "black", size = 6) +
#   theme_bw() +
#   theme(
#     axis.text = element_text(size = 14, face = "bold"),
#     axis.title = element_text(size = 16, face = 'bold')
#   )
# 
# ggsave('Graph/Farmer/decision_do.png', width = 7, height = 5, units = 'in',
#        dpi = 300)
# 
# ## BCR ####
# farmer3.1 <- outcome %>% 
#   select(BCR_interv, BCR_no_interv) %>% 
#   pivot_longer(., cols =c("BCR_interv", "BCR_no_interv"),
#                names_to = "variables", values_to = "project_outcome") %>% 
#   group_by(variables) %>%
#   mutate(lower_bound = quantile(project_outcome, 0.05, na.rm = T), # 90%
#          upper_bound = quantile(project_outcome, 0.95, na.rm = T)) %>%
#   filter(project_outcome >= lower_bound & project_outcome <= upper_bound) %>%
#   ungroup() %>%
#   mutate(variables = factor(variables, 
#                             levels=c('BCR_interv', 'BCR_no_interv'))) %>%
#   ggplot(., aes(y=project_outcome,x= variables, fill = variables)) +
#   geom_boxplot(width = 0.2) +
#   labs(y ="Benefit cost ratio", x = "")+
#   scale_x_discrete(labels = c( "BCR_interv" = "A","BCR_no_interv" = "B")) +
#   scale_fill_manual(name="",
#                     labels = c("  (A) With intervention", 
#                                "  (B) Without intervention"),
#                     values = c("turquoise", "deeppink"))+
#   theme_bw()
# farmer3.1 <- farmer3.1 + theme(legend.position = c(0.95, 0.9),
#                                legend.justification = c("right", "top"),
#                                axis.text = element_text(size = 14, face = "bold"),
#                                axis.title = element_text(size = 16, face = "bold"),
#                                legend.text = element_text(size = 15))
# print(farmer3.1)
# 
# ggsave('Graph/Farmer/bcr.png', width = 7, height = 5, units = 'in', dpi = 300)
# 
# ## Cashflow ####
# 
# outcome %>% 
#   select(Cashflow_decision_do1:Cashflow_decision_do25) %>% 
#   gather(key = 'Year', value = 'Cashflow', 
#          starts_with('Cashflow_decision_s0l2')) %>% 
#   mutate(Year = as.numeric(gsub('Cashflow_decision_s0l2', '', Year))) %>% 
#   group_by(Year) %>% 
#   summarise(
#     median = median(Cashflow/1e6),
#     q5 = quantile(Cashflow/1e6, 0.05),
#     q25 = quantile(Cashflow/1e6, 0.25),
#     q75 = quantile(Cashflow/1e6, 0.75),
#     q95 = quantile(Cashflow/1e6, 0.95)
#   ) %>% 
#   ggplot(aes(x=Year, y = median)) +
#   geom_ribbon(aes(ymin = q5, ymax = q95), fill = '#D3B2D8', alpha =.5) +
#   geom_ribbon(aes(ymin = q25, ymax = q75), fill = '#D3B2D8', alpha = 1)+
#   geom_line(color = 'darkred', size = 1) +
#   labs(x="Project time period (Year)", y= 'Annual Cashflow in million KES')+ 
#   theme_bw()
# 
# ggsave('Graph/Farmer/cashflow.png', width = 7, height = 5, units = 'in', 
#        dpi = 300)
# 
# ##PLS ####
# vipplot<-decisionSupport::plot_pls(pls_result,
#     input_table = read.csv("Input_tables/farmer_only_input_table.csv", sep=";"),
#                           threshold = 1,
#     pos_color = "#1a80bb", #blue
#     neg_color = "#ea801c") #orange
# 
# vipplot<- vipplot + 
#               theme(
#               axis.text = element_text(size = 10),
#               axis.title = element_text(size = 12, face="bold"),
#               legend.text = element_text(size = 10),
#               axis.ticks = element_blank()) 
# 
# vipplot <- vipplot + 
#   scale_y_discrete(labels = function(y) str_wrap(y, width = 10))
# 
# print(vipplot)  
# 
# 
# ggsave('Graph/Farmer/vip.png', width = 7, height = 5, units = 'in', dpi = 300)
# 
# ##EVPI ####
# 
# evpidata <- read.csv('Outcome/evpi.csv')
# 
# evpiplot <- evpidata %>%
#   select(NPV_decision_kes.EVPI_do, NPV_decision_kes.variable)  %>% 
#   filter(NPV_decision_kes.EVPI_do > 0)  %>%  
#   ggplot(aes( y = NPV_decision_kes.variable, x = NPV_decision_kes.EVPI_do))+
#   geom_col( fill = '#1a80bb', position = position_dodge()) +
#   labs(x = 'Expected Value of Perfect Information (KES)', y = "") + 
#   scale_y_discrete(
#     labels = c("ha_per_hh" = str_wrap( "Household land size (Ha)", width = 10),
#                "compensation_income_pm_acre" = str_wrap("Lease income (KES/acre)",
#                width =10))) +
#   theme_bw()+
#   scale_x_continuous(expand = c(0, 0), labels = comma) + #library(scales)
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(size = 12, face="bold"),
#         axis.ticks = element_blank())
# 
# 
# print(evpiplot)
# ggsave('Graph/Farmer/evpi.png', width = 7, height = 5, units  = 'in', dpi = 300)
# 
# library(ggpubr)
# ggarrange(vipplot, evpiplot, ncol = 2, nrow = 1,
#           widths =1,heights = 1,
#           labels = c("a)", "b)"))
# 
# ggsave('Graph/Farmer/vip_evpi_plot.png', width = 12, height = 5 , 
#        units = 'in', dpi = 300)
# 
# 
# # combining distribution of NPVs
# ggarrange(farmer1.2, farmer3.1, ncol = 2, nrow = 1,
#           widths =  1, 
#           labels = c("a)", "b)"))
# ggsave("Graph/Farmer/npv_bcr.png",width = 12, height = 5, units = 'in',
#        dpi = 300)
# 
# 
# 
# # Executing suggestions from the meeting ####
# 
# # 1. calculate individual costs and benefits and show in the text. 
# 
# table1.1 <- outcome %>% 
#   select(Food_money_saved:Annual_yield) %>% 
#   pivot_longer(cols = everything(), 
#                names_to = "variables", 
#                values_to = "cost_benefits") %>%
#   group_by(variables) %>%
#   summarise(
#     # min_value = min(cost_benefits),
#     # max_value = max(cost_benefits),
#     lower_bound = quantile(cost_benefits, 0.05, na.rm = TRUE),
#     median_value = median(cost_benefits),
#     upper_bound = quantile(cost_benefits, 0.95, na.rm = TRUE))
# 
# stat.table <- as.data.frame(table1.1)
# names(stat.table) <- c("Cost/Benefit", "Lower bound", "Median", "Upper Bound")
# 
# nicer_table <- rempsyc::nice_table(stat.table)
# ??nice_table
# print(nicer_table, preview = "docx")
# 
# flextable::save_as_docx(nicer_table, path = "Graph/Farmer/Table_1.docx")
