
# assumptions - general
#

test_decision <- function(x, varnames)
{
  source(file = "Scripts/test3.R")
  source(file = "Scripts/government_model.R")
  return(list(Interv_NPV = NPV_intervention,
              No_Interv_NPV = NPV_n_intervention,
              NPV_decision_do = NPV_intervention - NPV_n_intervention, 
              Cashflow_decision_do = result_intervention - result_n_intervention,
              GOVT_Interv_NPV = GOVT_NPV_intervention,
              GOVT_No_Interv_NPV = GOVT_NPV_n_intervention,
              GOVT_NPV_decision_do = GOVT_NPV_intervention - GOVT_NPV_n_intervention, 
              GOVT_Cashflow_decision_do = GOVT_result_intervention - 
                                          GOVT_result_n_intervention))
}

#simulate results
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = estimate_read_csv("Input_tables/farmer_input_table.csv"),
  model_function = test_decision,
  numberOfModelRuns = 1e2, #100
  functionSyntax = "plainNames")

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

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], 
                                ncomp = 1)
# 
# input_table <- read.csv("Input_tables/farmer_input_table.csv")
# 
# decisionSupport::plot_pls(pls_result, 
#                           input_table = input_table, 
#                           threshold = 0)

mcSimulation_table <- data.frame(mcSimulation_results$x, 
                                 mcSimulation_results$y[1:40])

evpi <- decisionSupport::multi_EVPI(mc = mcSimulation_table, 
                                    first_out_var = "GOVT_Interv_NPV")

# decisionSupport::plot_evpi(evpi, decision_vars = "GOVT_NPV_decision_do")


compound_figure(mcSimulation_object = mcSimulation_results, 
                input_table = read.csv("Input_tables/farmer_input_table.csv"), 
                plsrResults = pls_result, 
                EVPIresults = evpi,
                decision_var_name = "GOVT_NPV_decision_do", 
                cashflow_var_name = "GOVT_Cashflow_decision_do",
                base_size = 7)

stakeholder_NPV <- mcSimulation_table %>% 
  dplyr::select(Interv_NPV, GOVT_Interv_NPV)

stakeholder_NPV <-pivot_longer(stakeholder_NPV, 
             cols = c("Interv_NPV", "GOVT_Interv_NPV"),
             names_to = "Stakeholder",
             values_to = "Project_Outcome")
  
ggplot2::ggplot(stakeholder_NPV,aes( x = Project_Outcome, fill = Stakeholder)) +
        geom_smooth()
