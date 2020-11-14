#' Module to produce graphical outputs to visualize simulations' results
#'
#' @param res object with simulations results (deaths, new infections, # patients off and on treatment, # of lost to follow-up)
#'  from both the baseline and intervention scenarios
#' @param daly object with simulations results in terms of DALYs from both the baseline and intervention scenarios
#' @return 
#' This function returns summary plots of the simulations' results. 
#' @importFrom data.table melt.data.table
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 facet_wrap
#' @importFrom stats quantile
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 ggplot
#' @importFrom data.table data.table
#' @importFrom dplyr filter
#' @export
result_comparison_plot <- function(res){
  res <- res %>% group_by(scenario, simul) %>% mutate("New infection" = cumsum(`New infection`),
                                               "Death" = cumsum(Death)) 
  res <- res[!names(res) == "simul"]
  p <- suppressWarnings(res %>%
                          data.table() %>%
                          melt.data.table(id.vars = c("time","scenario")) %>%
                          group_by(time,scenario, variable) %>%
                          summarize(mean = mean(value),
                                    q2.5 = quantile(value, probs = .025),
                                    q97.5 = quantile(value, probs = .975)))
  
  gg <- ggplot(data = p, aes(x = time, y = mean, group = scenario)) +
    geom_line(aes(colour = scenario)) +
    geom_ribbon(data = p, aes(ymin= q2.5, ymax=q97.5, fill = scenario), linetype=2, alpha=0.1) +
    facet_wrap(~variable, scales = "free") +
    labs(x ='Time', y = 'Outcome',
         title = paste0("Simulation results")) +
    theme_bw() +
    scale_fill_manual(values = c("#0d4e93", "#ffdc00")) +
    scale_color_manual(values=c("#0d4e93", "#ffdc00"))+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA))+
    guides(fill=FALSE) +
    theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
          strip.text = element_text(size = 10),
          panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
          axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14))

  return(gg)
}

#' @title Making Daly Comparison plots
#' @param daly object with simulations results in terms of DALYs from both the baseline and intervention scenarios
#' @return 
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_density
DALY_comparison <- function(daly){
  
  ggplot(daly, aes(x=dalys, fill=scenario)) + geom_density(alpha=.3)+
    theme_bw() +
    scale_fill_manual(values = c("#0d4e93", "#ffdc00")) +
    scale_color_manual(values=c("#0d4e93", "#ffdc00")) +
    labs(x ='DALYs', y = '',
         title = paste0("Simulation results")) +
    theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
          strip.text = element_text(size = 10),
          panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
          axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14))
  
}



make_pyramid_data <- function(summary_results){
  summary_results$pop$age <- floor(summary_results$pop$age) 
  pyr_data <- summary_results$pop %>% group_by(male, age) %>% tally()
  pyr_data$gender[pyr_data$male == 1] <- "Male"
  pyr_data$gender[pyr_data$male == 0] <- "Female"
  pyr_data$n[pyr_data$male == 1] <- -pyr_data$n[pyr_data$male == 1]
  return(summary_results)
}

#' @title Making Age Pyramid
#' @param summary_results object with simulations results (deaths, new infections, # patients off and on treatment, # of lost to follow-up)
#'  from both the baseline and intervention scenarios
#' @return 
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 scale_fill_brewer
make_pyramid <- function(summary_results){
  pyr_data <- make_pyramid(summary_results)
  
  ggplot(pyr_data, aes(x = age, y = n, fill = gender)) + 
    geom_bar(data = subset(pyr_data, gender == "Female"), stat = "identity") + 
    geom_bar(data = subset(pyr_data, gender == "Male"), stat = "identity") + 
    scale_y_continuous(breaks = seq(-max(pyr_data$n), max(pyr_data$n), max(pyr_data$n) / 10), 
                       labels = as.character(c(seq(max(pyr_data$n), 0, -max(pyr_data$n) / 10), seq(max(pyr_data$n) / 10, max(pyr_data$n), max(pyr_data$n) / 10)))) + 
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") + 
    theme_bw()
}





mean_sample <- function(value, scenario, scen_1, scen_2){
  dv <- sample(value[scenario == scen_2],1000,replace=T) - sample(value[scenario == scen_1], 1000,replace=T)
  out <- mean(dv, na.rm = T)
  return(out)
}

mean_quantile <- function(value, scenario, scen_1, scen_2, probs){
  dv <- sample(value[scenario == scen_2],1000,replace=T) - sample(value[scenario == scen_1], 1000,replace=T)
  out <- quantile(dv, probs)
  return(out)
}

#' @title Return comparison table
#' @param summary_results object with simulations results (deaths, new infections, # patients off and on treatment, # of lost to follow-up)
#'  from both the baseline and intervention scenarios
#' @return 
#' @export
compare_table <- function(data, scen_1, scen_2){
  data <- data %>% group_by(scenario, simul) %>% mutate("New infection" = cumsum(`New infection`),
                                                        "Death" = cumsum(Death)) 
  tab_out <- data[data$time == max(data$time),] %>%
    data.table() %>%
    melt.data.table() %>%
    group_by(variable) %>%
    summarize(baseline = mean(value[scenario == scen_1]),
              intervention= mean(value[scenario == scen_2]),
              mean = mean_sample(value, scenario ,scen_2=scen_2, scen_1=scen_1),
              q2.5 = mean_quantile(value, scenario ,scen_2=scen_2, scen_1=scen_1, probs = .025),
              q97.5 = mean_quantile(value, scenario ,scen_2=scen_2, scen_1=scen_1, probs = .975)) %>%
    ungroup()
  return(tab_out)
}