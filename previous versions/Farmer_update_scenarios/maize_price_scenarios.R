library(decisionSupport)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggpubr)

set.seed(254)

source(file = "make_variables.R")
make_variables(estimate_read_csv("FarmerLC_inputdata.csv",
                                 sep = ";"))

# The model ####

maize_price_function <- function(x, varnames){
  
  source(file = "Farmer_low_MP.R")
    source(file = "Farmer_medium_MP.R")
  source(file = "Farmer_high_MP.R")
  return(list(Interv_NPV_kes = NPV_interv_kes,     
              No_Interv_NPV_kes = NPV_n_interv_kes,
              NPV_decision_kes = NPV_interv_kes - NPV_n_interv_kes,
              Interv_NPV_kes1 = NPV_interv_kes1,     
              No_Interv_NPV_kes1 = NPV_n_interv_kes1,
              NPV_decision_kes1 = NPV_interv_kes1 - NPV_n_interv_kes1,
              Interv_NPV_kes2 = NPV_interv_kes2,     
              No_Interv_NPV_kes2 = NPV_n_interv_kes2,
              NPV_decision_kes2 = NPV_interv_kes2 - NPV_n_interv_kes2,
              
              BCR_no_interv = bcr_n_interv,
              BCR_interv = bcr_interv,
              BCR_no_interv1 = bcr_n_interv1,
              BCR_interv1 = bcr_interv1,
              BCR_no_interv2 = bcr_n_interv2,
              BCR_interv2 = bcr_interv2,
              
              Food_money_saved = food_cost_saved_npv,
              Natural_assets = natural_assets_npv,
              Planning = farmer_plan_cost, 
              Disruption_cost = farmer_one_time_cost,
              Lease = lease_npv, 
              Production_costs = prdn_costs_npv, 
              Alternative_income = alt_income_npv, 
              Childhood = better_childhood_npv,
              Social = social_npv,
              Medical = medical_npv,
              Annual_yield = crop_npv,
              
              Food_money_saved1 = food_cost_saved_npv1,
              Natural_assets1 = natural_assets_npv1,
              Planning1 = farmer_plan_cost1, 
              Disruption_cost1 = farmer_one_time_cost1,
              Lease1 = lease_npv1, 
              Production_costs1 = prdn_costs_npv1, 
              Alternative_income1 = alt_income_npv1, 
              Childhood1 = better_childhood_npv1,
              Social1 = social_npv1,
              Medical1 = medical_npv1,
              Annual_yield1 = crop_npv1,
              
              Food_money_saved2 = food_cost_saved_npv2,
              Natural_assets2 = natural_assets_npv2,
              Planning2 = farmer_plan_cost2, 
              Disruption_cost2 = farmer_one_time_cost2,
              Lease2 = lease_npv2, 
              Production_costs2 = prdn_costs_npv2, 
              Alternative_income2 = alt_income_npv2, 
              Childhood2 = better_childhood_npv2,
              Social2 = social_npv2,
              Medical2 = medical_npv2,
              Annual_yield2 = crop_npv2,
              
              Cashflow_decision_do = farmer_interv_netbenefit_kes -
                farmer_pre_interv_netbenefit_kes,
              Cashflow_decision_hh1 = farmer_interv_netbenefit_kes1 -
                farmer_pre_interv_netbenefit_kes1,
              Cashflow_decision_ha2 = farmer_interv_netbenefit_kes2 -
                farmer_pre_interv_netbenefit_kes2))
}


# Model runs ####
mcSimulation_results <-  decisionSupport::mcSimulation(
  estimate = estimate_read_csv("FarmerLC_inputdata.csv",sep =";"),
  model_function = maize_price_function,
  numberOfModelRuns = 1e4, #10000
  functionSyntax = "plainNames")

# Outcome distribution ####
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                    vars = c("Interv_NPV_kes", "No_Interv_NPV_kes"),
                                    method = 'smooth_simple_overlay',
                                    base_size = 8)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                    vars = c("Interv_NPV_kes1", "No_Interv_NPV_kes1"),
                                    method = 'smooth_simple_overlay',
                                    base_size = 8)
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                    vars = c("Interv_NPV_kes2", "No_Interv_NPV_kes2"),
                                    method = 'smooth_simple_overlay',
                                    base_size = 8)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                    vars = c("NPV_decision_kes",
                                      "NPV_decision_kes1","NPV_decision_kes2"),
                                    method = 'boxplot')

# decisionSupport::plot_cashflow(mcSimulation_object = mcSimulation_results,
#                                cashflow_var_name = "Cashflow_decision_do")

# Variable Importance ####
pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[9],
                                ncomp = 1)
#write.csv(pls_result, 'Outcome/pls.csv')
decisionSupport::plot_pls(pls_result,
  input_table = read.csv("farmerLC_size0.csv", sep=";"),
  threshold = 1)

# ggsave("Graph/Farmer/vip.png", width = 7, height = 5, units = 'in', dpi = 300)

# Information value ####
mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[1:9])
evpi <- decisionSupport::multi_EVPI(mc = mcSimulation_table,
                                    first_out_var = "Interv_NPV_kes")
write.csv(evpi,'Outcome/evpi.csv')
decisionSupport::plot_evpi(evpi,
                           decision_vars = "NPV_decision_kes",
                           input_table =read.csv("FarmerLC_inputdata.csv", sep=";"),
                           bar_color = "#1a80bb")


# Compound figure ####
compound_figure(mcSimulation_object = mcSimulation_results,
                input_table = read.csv("FarmerLC_inputdata.csv", sep =";"),
                plsrResults = pls_result,
                EVPIresults = evpi,
                decision_var_name = "NPV_decision_kes2",
                cashflow_var_name = "Cashflow_decision_ha2",
                base_size = 7)



# post_analysis ####
outcome <- as.data.frame(mcSimulation_results$y)
theXs <- as.data.frame(mcSimulation_results$x)
str(outcome)

outcome <- outcome %>% mutate(NPV_per_ha = Interv_NPV_kes/ha_per_hh,
                              No_NPV_per_ha = No_Interv_NPV_kes/ha_per_hh,
                              NPV_per_ha_decision = NPV_decision_kes/ha_per_hh )
write.csv(outcome, 'Outcome/farmer_mcresults.csv')
write.csv(theXs, 'Outcome/farmer_mcresults_x.csv')


# plotting ####

outcome <- read.csv('Outcome/farmer_mcresults.csv')

## ranges ####
ranges <- outcome %>%
  select(NPV_per_ha, No_NPV_per_ha, NPV_per_ha_decision) %>%
  pivot_longer(cols = everything(),
               names_to = "variables",
               values_to = "project_outcome") %>%
  group_by(variables) %>%
  summarise(
    min_value = min(project_outcome),
    max_value = max(project_outcome)
  )

print(ranges)

## Percentages ####

percentages <- outcome %>%
  select(NPV_per_ha, No_NPV_per_ha, NPV_per_ha_decision) %>%
  pivot_longer(cols = everything(),
               names_to = "variables",
               values_to = "project_outcome") %>%
  group_by(variables) %>%
  summarise(
    negative = sum(project_outcome < 0, na.rm = TRUE) / n() * 100,
    positive = sum(project_outcome >= 0, na.rm = TRUE) / n() * 100
  )

print(percentages)


##bounds KES per hectare
percentiles <- outcome %>%
  select(NPV_per_ha, No_NPV_per_ha, NPV_per_ha_decision) %>%
 pivot_longer(cols = everything(),
              names_to = "variables",
              values_to = "project_outcome") %>%
 group_by(variables) %>%
 summarise(
     lower_bound = quantile(project_outcome, 0.05, na.rm = TRUE),
     upper_bound = quantile(project_outcome, 0.95, na.rm = TRUE))
print(percentiles)

##bounds USD ()
percentiles <- outcome %>%
  select(Interv_NPV, No_Interv_NPV, NPV_decision_do) %>%
  pivot_longer(cols = everything(),
               names_to = "variables",
               values_to = "project_outcome") %>%
  group_by(variables) %>%
  summarise(
    lower_bound = quantile(project_outcome, 0.05, na.rm = TRUE),
    upper_bound = quantile(project_outcome, 0.95, na.rm = TRUE))
print(percentiles)


# BCR bounds ###
bcr_percentiles <- outcome %>%
  select(BCR_interv, BCR_no_interv) %>%
  pivot_longer(cols = everything(),
               names_to = "variables",
               values_to = "bcr") %>%
  group_by(variables) %>%
  summarise(
    lower_bound = quantile(bcr, 0.05, na.rm = TRUE),
    upper_bound = quantile(bcr, 0.95, na.rm = TRUE))
print(bcr_percentiles)

### Normalised by hectare (ha_per_hh) ########
farmer2.1 <- outcome %>%
  select( NPV_per_ha, No_NPV_per_ha, NPV_per_ha_decision) %>%
  pivot_longer(., cols =c( "NPV_per_ha", "No_NPV_per_ha",
                          "NPV_per_ha_decision"),
               names_to = "variables", values_to = "project_outcome") %>%

  group_by(variables) %>%
  mutate(lower_bound = quantile(project_outcome, 0.05, na.rm = T), # 95%
         upper_bound = quantile(project_outcome, 0.95, na.rm = T)) %>%
  filter(project_outcome >= lower_bound & project_outcome <= upper_bound) %>%
  ungroup() %>%
  mutate(variables = factor(variables,
                            levels = c("NPV_per_ha", "No_NPV_per_ha",
                                       "NPV_per_ha_decision"))) %>%  #fix the order
  ggplot(aes(y=project_outcome/1e6, x = variables, fill = variables))+
  geom_boxplot(width = .25, alpha = 0.7) +

  labs(y =" NPV per ha in million  Kes", x= "")+
  scale_x_discrete(labels = c( "NPV_per_ha" = "A","No_NPV_per_ha" = "B",
                              "NPV_per_ha_decision" = "C")) +
  scale_fill_manual(name = "",
                    labels = c(" (A) NPV per ha with intervention",
                               " (B) NPV per ha without intervention",
                               " (C) A - B"),
                    values = c("turquoise", "deeppink", "purple"))+
  theme_bw()
farmer2.1 + theme(legend.position = c(0.3, 0.9),
                  legend.justification = c("left", "top"))

ggsave('Graph/Farmer/NPV_per_ha_boxplot.png', width = 7, height = 5,
       units = 'in', dpi = 300)

## geom density for the normalised results..
farmer3.1 <- outcome %>%
  select (NPV_per_ha, No_NPV_per_ha, NPV_per_ha_decision) %>%
  pivot_longer(cols = c("NPV_per_ha", "No_NPV_per_ha", "NPV_per_ha_decision"),
               names_to = "variables", values_to = "project_outcome") %>%
  group_by(variables) %>%
  mutate(lower_bound = quantile(project_outcome, 0.005, na.rm = TRUE), # 99%
         upper_bound = quantile(project_outcome, 0.995, na.rm = TRUE)) %>%
  filter(project_outcome >= lower_bound & project_outcome <= upper_bound) %>%
  ungroup() %>%
  mutate(variables = factor(variables,
                            levels = c("NPV_per_ha", "No_NPV_per_ha",
                                       "NPV_per_ha_decision"))) %>%  #fix the order
  ggplot(aes(x = project_outcome / 1e6, colour = variables, fill = variables)) +
  geom_density(alpha = 0.25) +
  labs(y = "Probability density", x = "NPV per ha in million Kes") +
  scale_fill_manual(name = "",
                    labels = c(" (A) NPV per ha with intervention",
                               " (B) NPV per ha without intervention",
                               " (C) A - B"),
                    values = c("turquoise", "orange", "blue")) +
  scale_colour_manual(name = "",
                      labels = c(" (A) NPV per ha with intervention",
                                 " (B) NPV per ha without intervention",
                                 " (C) A - B"),
                      values = c("turquoise", "orange", "blue")) +
  theme_light() +
  theme(legend.position = c(0.9, 0.9),
        legend.justification = c("right", "top"))

# Print the plot
print(farmer3.1)
ggsave('Graph/Farmer/NPV_per_ha.png', width = 7, height = 5, units = 'in',
       dpi = 300)


# Smooth overlay #### (Normalised with and without intervention NPV per ha)
farmer1.2 <- outcome %>%
  select(NPV_per_ha, No_NPV_per_ha) %>%
  pivot_longer(., cols = c("NPV_per_ha", "No_NPV_per_ha"),
               names_to = "variables", values_to = "project_outcome") %>%
  group_by(variables) %>%
  mutate(lower_bound = quantile(project_outcome, 0.025, na.rm = T), # 95%
         upper_bound = quantile(project_outcome, 0.975, na.rm = T)) %>%
  filter(project_outcome >= lower_bound & project_outcome <= upper_bound) %>%
  ungroup() %>%
  mutate(variables = factor(variables,
                            levels=c('NPV_per_ha', 'No_NPV_per_ha'))) %>%
  ggplot(aes(x = project_outcome/1e6, colour = variables, fill = variables))+
  geom_density(alpha = .25) +
  coord_cartesian(xlim = c(-50, 550)) +
  labs(x = "NPV per hectare in Million KES", y = "Probability density")+
  scale_colour_manual(name = "",
                    labels = c(" Benefit with intervention",
                               " Benefit without intervention"),
                    values = c("turquoise", "deeppink"))+
  scale_fill_manual(name = "",
                      labels = c(" Benefit with intervention",
                                 " Benefit without intervention"),
                      values = c("turquoise", "deeppink"))+
  theme_bw()
farmer1.2<- farmer1.2 + theme(legend.position = c(0.8, 0.9),
                  legend.justification = c("right", "top"),
                  axis.text = element_text(size = 14, face = 'bold'),
                  axis.title = element_text(size = 16, face = 'bold'),
                  legend.text = element_text(size = 15))
print(farmer1.2)

ggsave('Graph/Farmer/interventions.png', width = 7, height = 5, units = 'in',
       dpi = 300)

# NPV per ha for with and without intervention ####
farmer2.1 <- outcome %>%
  select(NPV_per_ha, No_NPV_per_ha) %>%
  pivot_longer(., cols =c("NPV_per_ha", "No_NPV_per_ha"),
               names_to = "variables", values_to = "project_outcome") %>%
  group_by(variables) %>%
  mutate(lower_bound = quantile(project_outcome, 0.05, na.rm = T), # 90%
         upper_bound = quantile(project_outcome, 0.95, na.rm = T)) %>%
  filter(project_outcome >= lower_bound & project_outcome <= upper_bound) %>%
  ungroup() %>%
  mutate(variables = factor(variables,
                            levels=c('NPV_per_ha', 'No_NPV_per_ha'))) %>%
  ggplot(., aes(y=project_outcome/1e6,x= variables, fill = variables)) +
  geom_boxplot(width = 0.2) +
  labs(y =" NPV per hectare in Million KES", x = "")+
  scale_x_discrete(labels = c( "NPV_per_ha" = "A","No_NPV_per_ha" = "B")) +
  scale_fill_manual(name="",
                    labels = c("  (A) Benefit with intervention",
                               "  (B) Benefit without intervention"),
                    values = c("turquoise", "deeppink"))+
  theme_bw()
farmer2.1 <- farmer2.1 + theme(legend.position = c(0.95, 0.8),
                  legend.justification = c("right", "top"),
                  axis.text = element_text(size = 14, face = "bold"),
                  axis.title = element_text(size = 16, face = "bold"),
                  legend.text = element_text(size = 15))
print(farmer2.1)

ggsave('Graph/Farmer/interventions_boxplot.png', width = 7, height = 5,
       units = 'in', dpi = 300)

# NPV difference of with and without intervention ####
outcome %>%
  select(NPV_decision_kes) %>%
  pivot_longer(., cols ="NPV_decision_kes",
               names_to = "variables", values_to = "project_outcome") %>%
  ggplot(aes(x=project_outcome/1e6)) +
  geom_histogram(aes(fill = project_outcome>0),
                breaks=seq(-250,800, by=2))+
  #geom_density(alpha=.5) +
  guides(fill = FALSE)+
  labs(x =" Project outcome in Million Kes", y= "Frequency")+
  annotate(geom = "text",label ="25.95 %", x = -150, y = 60,color = "black") +
  annotate(geom = "text", label ="74.05 %", x = 280, y = 60,color = "black") +
  theme_light()

# NPV decision do. - normalised per ha ######
percentages <- outcome %>%
  select(NPV_per_ha_decision) %>%
  summarise(
    negative = sum(NPV_per_ha_decision < 0, na.rm = TRUE) / n() * 100,
    positive = sum(NPV_per_ha_decision >= 0, na.rm = TRUE) / n() * 100
  )

negative_percent <- round(percentages$negative, 2)
positive_percent <- round(percentages$positive, 2)

custom_colors <- c("negative" = "#1a80bb", "positive" = "#ea801c") #orange

outcome %>%
  mutate(outcome_category = ifelse(NPV_per_ha_decision >= 0, "positive",
                                   "negative")) %>%
  select(NPV_per_ha_decision, outcome_category) %>%
  pivot_longer(., cols ="NPV_per_ha_decision",
               names_to = "variables", values_to = "project_outcome") %>%
  ggplot(aes(x = project_outcome / 1e6, fill = outcome_category)) +
  geom_histogram(breaks = seq(-250, 800, by = 2)) +
  scale_fill_manual(values = custom_colors) +
  guides(fill = FALSE)+
  labs(x =" NPV per hectare in Million KES", y= "Frequency")+
  annotate(geom = "text",label = paste0(negative_percent, " %"), x = -150,
           y = 60,color = "black", size = 6) +
  annotate(geom = "text", label = paste0(positive_percent, " %"), x = 280,
           y = 60,color = "black", size = 6) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 16, face = 'bold')
  )

ggsave('Graph/Farmer/decision_do.png', width = 7, height = 5, units = 'in',
       dpi = 300)

## BCR ####
farmer3.1 <- outcome %>%
  select(BCR_interv, BCR_no_interv) %>%
  pivot_longer(., cols =c("BCR_interv", "BCR_no_interv"),
               names_to = "variables", values_to = "project_outcome") %>%
  group_by(variables) %>%
  mutate(lower_bound = quantile(project_outcome, 0.05, na.rm = T), # 90%
         upper_bound = quantile(project_outcome, 0.95, na.rm = T)) %>%
  filter(project_outcome >= lower_bound & project_outcome <= upper_bound) %>%
  ungroup() %>%
  mutate(variables = factor(variables,
                            levels=c('BCR_interv', 'BCR_no_interv'))) %>%
  ggplot(., aes(y=project_outcome,x= variables, fill = variables)) +
  geom_boxplot(width = 0.2) +
  labs(y ="Benefit cost ratio", x = "")+
  scale_x_discrete(labels = c( "BCR_interv" = "A","BCR_no_interv" = "B")) +
  scale_fill_manual(name="",
                    labels = c("  (A) With intervention",
                               "  (B) Without intervention"),
                    values = c("turquoise", "deeppink"))+
  theme_bw()
farmer3.1 <- farmer3.1 + theme(legend.position = c(0.95, 0.9),
                               legend.justification = c("right", "top"),
                               axis.text = element_text(size = 14, face = "bold"),
                               axis.title = element_text(size = 16, face = "bold"),
                               legend.text = element_text(size = 15))
print(farmer3.1)

ggsave('Graph/Farmer/bcr.png', width = 7, height = 5, units = 'in', dpi = 300)

## Cashflow ####

outcome %>%
  select(Cashflow_decision_do1:Cashflow_decision_do25) %>%
  gather(key = 'Year', value = 'Cashflow',
         starts_with('Cashflow_decision_do')) %>%
  mutate(Year = as.numeric(gsub('Cashflow_decision_do', '', Year))) %>%
  group_by(Year) %>%
  summarise(
    median = median(Cashflow/1e6),
    q5 = quantile(Cashflow/1e6, 0.05),
    q25 = quantile(Cashflow/1e6, 0.25),
    q75 = quantile(Cashflow/1e6, 0.75),
    q95 = quantile(Cashflow/1e6, 0.95)
  ) %>%
  ggplot(aes(x=Year, y = median)) +
  geom_ribbon(aes(ymin = q5, ymax = q95), fill = '#D3B2D8', alpha =.5) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = '#D3B2D8', alpha = 1)+
  geom_line(color = 'darkred', size = 1) +
  labs(x="Project time period (Year)", y= 'Annual Cashflow in million KES')+
  theme_bw()

ggsave('Graph/Farmer/cashflow.png', width = 7, height = 5, units = 'in',
       dpi = 300)

##PLS ####
vipplot<-decisionSupport::plot_pls(pls_result,
    input_table = read.csv("Input_tables/FarmerLC_inputdata.csv", sep=";"),
                          threshold = 1,
    pos_color = "#1a80bb", #blue
    neg_color = "#ea801c") #orange

vipplot<- vipplot +
              theme(
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 12, face="bold"),
              legend.text = element_text(size = 10),
              axis.ticks = element_blank())

vipplot <- vipplot +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10))

print(vipplot)


ggsave('Graph/Farmer/vip.png', width = 7, height = 5, units = 'in', dpi = 300)

##EVPI ####

evpidata <- read.csv('Outcome/evpi.csv')

evpiplot <- evpidata %>%
  select(NPV_decision_kes.EVPI_do, NPV_decision_kes.variable)  %>%
  filter(NPV_decision_kes.EVPI_do > 0)  %>%
  ggplot(aes( y = NPV_decision_kes.variable, x = NPV_decision_kes.EVPI_do))+
  geom_col( fill = '#1a80bb', position = position_dodge()) +
  labs(x = 'Expected Value of Perfect Information (KES)', y = "") +
  scale_y_discrete(
    labels = c("ha_per_hh" = str_wrap( "Household land size (Ha)", width = 10),
               "compensation_income_pm_acre" = str_wrap("Lease income (KES/acre)",
               width =10))) +
  theme_bw()+
  scale_x_continuous(expand = c(0, 0), labels = comma) + #library(scales)
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face="bold"),
        axis.ticks = element_blank())


print(evpiplot)
ggsave('Graph/Farmer/evpi.png', width = 7, height = 5, units  = 'in', dpi = 300)

# library(ggpubr)
ggarrange(vipplot, evpiplot, ncol = 2, nrow = 1,
          widths =1,heights = 1,
          labels = c("a)", "b)"))

ggsave('Graph/Farmer/vip_evpi_plot.png', width = 12, height = 5 ,
       units = 'in', dpi = 300)

# combining distribution of NPVs
ggarrange(farmer1.2, farmer3.1, ncol = 2, nrow = 1,
          widths =  1,
          labels = c("a)", "b)"))
ggsave("Graph/Farmer/npv_bcr.png",width = 12, height = 5, units = 'in',
       dpi = 300)



# Executing suggestions from the meeting ####
# 1. calculate individual costs and benefits and show in the text.

table1.1 <- outcome %>%
  select(Food_money_saved:Annual_yield) %>%
  pivot_longer(cols = everything(),
               names_to = "variables",
               values_to = "cost_benefits") %>%
  group_by(variables) %>%
  summarise(
    # min_value = min(cost_benefits),
    # max_value = max(cost_benefits),
    lower_bound = quantile(cost_benefits, 0.05, na.rm = TRUE),
    median_value = median(cost_benefits),
    upper_bound = quantile(cost_benefits, 0.95, na.rm = TRUE))

stat.table <- as.data.frame(table1.1)
names(stat.table) <- c("Cost/Benefit", "Lower bound", "Median", "Upper Bound")

nicer_table <- rempsyc::nice_table(stat.table)
??nice_table
print(nicer_table, preview = "docx")

flextable::save_as_docx(nicer_table, path = "Graph/Farmer/Table_1.docx")

# Suggestion 2 ####
# maize price scenario
outcome %>% 
  select(NPV_decision_kes, NPV_decision_kes1, NPV_decision_kes2) %>% 
  mutate(NPV_per_ha0 = NPV_decision_kes/ha_per_hh,
         NPV_per_ha1 = NPV_decision_kes1/ha_per_hh,
         NPV_per_ha2 = NPV_decision_kes2/ha_per_hh) %>% 
  pivot_longer(., cols =c("NPV_per_ha0", "NPV_per_ha1", "NPV_per_ha2"),
               names_to = "variables", values_to = "decision_do") %>%
  group_by(variables) %>%
  mutate(lower_bound = quantile(decision_do, 0.05, na.rm = T), # 90%
         upper_bound = quantile(decision_do, 0.95, na.rm = T)) %>%
  filter(decision_do >= lower_bound & decision_do <= upper_bound) %>%
  ungroup() %>%
  mutate(variables = factor(variables,
                            levels=c('NPV_per_ha0', 'NPV_per_ha1',
                                     'NPV_per_ha2'))) %>%
  ggplot(., aes(y=decision_do/1e6,x= variables)) +
  geom_boxplot(width = 0.2, show.legend = F, colour = "black", fill = "grey") +
  theme_bw() +
  labs(x = 'Maize price in KES per Kg', 
       y = str_wrap("(NPV with LC - NPV without LC) per hectare in million KES"), 
       width = 10) +
  scale_x_discrete(
    labels = c("NPV_per_ha0" = "5 - 86.67", "NPV_per_ha1" = "86.68 - 168.33",
               "NPV_per_ha2" = "168.34 - 250")) +
  scale_y_continuous(expand = c(0, 0), labels = comma) + #library(scales)
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        axis.ticks = element_blank())+
  stat_compare_means(method = "kruskal.test", label = "p.signif",
                     aes(label = after_stat(p.signif)))

ggsave("Scenario/Farmer2/maize_price.png", width = 7, height = 5, units = "in", dpi = 300)



processed_data <- outcome %>%
  select(NPV_decision_kes, NPV_decision_kes1, NPV_decision_kes2) %>%
  mutate(
    NPV_per_ha0 = NPV_decision_kes / ha_per_hh,
    NPV_per_ha1 = NPV_decision_kes1 / ha_per_hh,
    NPV_per_ha2 = NPV_decision_kes2 / ha_per_hh
  ) %>%
  pivot_longer(
    cols = c("NPV_per_ha0", "NPV_per_ha1", "NPV_per_ha2"),
    names_to = "variables", values_to = "decision_do"
  ) %>%
  mutate(decision_do2 = decision_do/1e6) %>% 
  group_by(variables) %>%
  mutate(
    lower_bound = quantile(decision_do, 0.05, na.rm = TRUE), # 90%
    upper_bound = quantile(decision_do, 0.95, na.rm = TRUE)
  ) %>%
  filter(decision_do >= lower_bound & decision_do <= upper_bound) %>%
  ungroup() %>%
  mutate(
    variables = factor(
      variables,
      levels = c('NPV_per_ha0', 'NPV_per_ha1', 'NPV_per_ha2')
    ))



# Bar plot of mean +/-se
a <- ggboxplot(processed_data, x = "variables", y = "decision_do2", add = "mean_se",
               width = 0.2) #+
  # stat_compare_means(method = "anova") +     # Global p-value
  # stat_compare_means( label = "p.signif",
  #                     label.y = c(15,15,15))       # compare to ref.group
a + 
  theme_light() +
  scale_x_discrete(labels = c(
      "NPV_per_ha0" = "5 - 86.67",
      "NPV_per_ha1" = "86.68 - 168.33",
      "NPV_per_ha2" = "168.34 - 250" )) +
  labs(x = " Maize prices in KES per Kg",
       y = " (NPV with LC - NPV without LC) per hectare in Million KES") +
  # annotate("text",
  #          label = "Significane level: **** p < 0.0001", 
  #          x= 2.5, y = 5, size = 4, colour = "black")
annotate("text",
         label = "Anova: p < 2.2e -16", 
         x= 2.5, y = 5, size = 4, colour = "black")


head(processed_data, 5)
ggdensity(processed_data, x = "decision_do2",
          add = "median", rug = TRUE,
          color = "variables", fill = "variables", alpha = 0.5,
          palette = c("#00AFBB", "pink", "green"))


gghistogram(processed_data, x = "decision_do2",
            add = "median", rug = TRUE, bins = 1000,alpha = 0.2,
            color = "variables", fill = "variables",
            palette = c( "#E7B800", "#00AFBB","green"))






# Define comparisons (list of pairs)
comparisons <- list(
  c("NPV_per_ha0", "NPV_per_ha1"),
  c("NPV_per_ha0", "NPV_per_ha2"),
  c("NPV_per_ha1", "NPV_per_ha2")
)

# Plot the data
ggplot(processed_data, aes(x = variables, y = decision_do / 1e6)) +
  geom_boxplot(width = 0.2, colour = "black", fill = "grey",
               position = position_dodge(width = NULL)) +
  theme_bw() +
  labs(
    x = 'Maize prices in KES per Kg',
    y = "(NPV with LC - NPV without LC) per hectare in Million KES"
  ) +
  scale_x_discrete(
    labels = c(
      "NPV_per_ha0" = "5 - 86.67",
      "NPV_per_ha1" = "86.68 - 168.33",
      "NPV_per_ha2" = "168.34 - 250"
    )
  ) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-20, 5),
                     labels = scales::comma) +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.ticks = element_blank()) +
  # stat_compare_means(
  #   comparisons = comparisons,
  #   method = "t.test",  # Use t-test for pairwise comparisons
  #   label = "p.signif",
  #   aes(color = ..p.signif..)) + 
  # annotate("text",
  #          label = "Significane level: **** p < 0.0001", 
  #          x= 2.5, y = 2.5, size = 4, colour = "black")
annotate("text",
         label = "Anova: p < 2.2e -16",
         x= 2.5, y = 2.5, size = 4, colour = "black")


ggsave("Scenario/Farmer2/maize_price_anova.png", width = 7, height = 5, units = "in",
       dpi = 300)


