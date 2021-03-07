#' Module to produce graphical outputs to visualize simulations' results
#'
#' @param res object with simulations results (deaths, new infections, # patients off and on treatment, # of lost to follow-up)
#'  from both the baseline and intervention scenarios
#' @param daly object with simulations results in terms of DALYs from both the baseline and intervention scenarios
#' @return 
#' This function returns summary plots of the simulations' results. 
#' @importFrom data.table melt.data.table
#' @importFrom data.table setDT
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
#' @importFrom ggplot2 geom_vline
#' @importFrom data.table data.table
#' @importFrom dplyr filter
#' @importFrom viridis viridis
#' @export
result_comparison_plot <- function(res,h=3){
  p <- suppressWarnings(res %>%
                          data.table() %>%
                          melt.data.table(id.vars = c("time","scenario")) %>%
                          group_by(time,scenario, variable) %>%
                          summarize(mean = mean(value),
                                    q2.5 = quantile(value, probs = .025, na.rm = T),
                                    q97.5 = quantile(value, probs = .975, na.rm = T)) )
  p1 <- p %>% filter(variable != "Death" & variable != "New infection" & variable != "simul")
  p2 <- suppressWarnings(res %>%
                           data.table() %>%
                           melt.data.table(id.vars = c("time","scenario","simul"))) %>%
    filter(variable == "Death" | variable == "New infection") %>%
    group_by(scenario, variable,simul) %>%
    summarize(deaths_cum = sum(value))
  
  tmp <- suppressWarnings(setDT(res) %>%
                            data.table::melt(id.vars = c("time","scenario","simul"),
                                             value.name = "outcome"))
  
  p1 <- tmp[(variable != "Death" & variable != "New infection" & variable != "simul"),
            list(mean = mean(outcome, na.rm = T),
                 `2.5%` = quantile(outcome, probs = .025,na.rm=T),
                 `97.5%` = quantile(outcome, probs = .975,na.rm=T)),by = 'time,scenario,variable']
  
  p2 <- tmp[(variable == "Death" | variable == "New infection"),
            list(sum = sum(outcome, na.rm = T)),by = 'scenario,variable,simul']
  
  gg1 <- ggplot(data = p1, aes(x = time, y = mean, group = scenario)) +
    geom_line(aes(colour = scenario)) +
 #   geom_ribbon(data = p1, aes(ymin= q2.5, ymax=q97.5, fill = scenario), linetype=2, alpha=0.1) +
    geom_ribbon(data = p1, aes(ymin= `2.5%`, ymax=`97.5%`, fill = scenario), linetype=2, alpha=0.1) +
    facet_wrap(~variable, scales = "free") +
    labs(x ='Week', y = 'Outcome',
         title = paste0("Simulation results")) +
    theme_bw() +
    scale_fill_manual(values = c("#0d4e93", "#ffdc00")) +
    scale_color_manual(values=c("#0d4e93", "#ffdc00"))+
    scale_fill_manual(values = viridis(length(unique(tmp$scenario)))) +
    scale_color_manual(values=viridis(length(unique(tmp$scenario))))+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA))+
    guides(fill=FALSE) +
    theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
          strip.text = element_text(size = 10),
          panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
          axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14)) +
    geom_vline(xintercept = h*52, linetype = "dotted")
  
  gg2 <- ggplot(data = p2,
                  aes(x=sum, fill=scenario)) + geom_density(alpha=.3)+
    facet_wrap(~variable, scales = "free") +
    theme_bw() +
    scale_fill_manual(values = c("#0d4e93", "#ffdc00")) +
    scale_color_manual(values=c("#0d4e93", "#ffdc00")) +
    scale_fill_manual(values = viridis(length(unique(tmp$scenario)))) +
    scale_color_manual(values=viridis(length(unique(tmp$scenario))))+
    labs(x ='Outcome', y = 'Probability') +
    theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
          strip.text = element_text(size = 10),
          panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
          axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14))
  
  gg <- gridExtra::grid.arrange(gg1,gg2)
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
    scale_fill_manual(values = viridis(length(unique(daly$scenario)))) +
    scale_color_manual(values=viridis(length(unique(daly$scenario))))+
    labs(x ='DALYs', y = 'Probability',
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
  pyr_data$n[pyr_data$male == 1] <- -pyr_data$n[pyr_data$male == 1] / sum(pyr_data$n[pyr_data$male == 1] )
  pyr_data$n[pyr_data$male == 0] <- pyr_data$n[pyr_data$male == 0] / sum(pyr_data$n[pyr_data$male == 0] )
  return(pyr_data)
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
#' @importFrom ggplot2 fortify
#' @importFrom dplyr tally
make_pyramid <- function(summary_results){
  pyr_data <- make_pyramid_data(summary_results)
  m_axis <- max(pyr_data$n)
  
  ggplot(pyr_data, aes(x = age, y = n, fill = gender)) + 
    geom_bar(data = subset(pyr_data, gender == "Female"), stat = "identity") + 
    geom_bar(data = subset(pyr_data, gender == "Male"), stat = "identity") + 
    scale_y_continuous(breaks = seq(-m_axis, m_axis, m_axis / 10), 
                       labels = as.character(round(c(seq(m_axis, 0, -m_axis / 10), 
                                                     seq(m_axis / 10,m_axis, m_axis / 10)), 3))) + 
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
  out <- quantile(dv, probs, na.rm=TRUE)
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
  data <- data[data$time == max(data$time), !names(data) %in% c("simul","time")]
  tab_out <- data %>%
    data.table() %>%
    melt.data.table() %>%
    group_by(variable) %>%
    summarize(baseline = mean(value[scenario == scen_1]),
              intervention= mean(value[scenario == scen_2]),
              mean = mean_sample(value, scenario ,scen_2=scen_2, scen_1=scen_1),
              q2.5 = mean_quantile(value, scenario ,scen_2=scen_2, scen_1=scen_1, probs = .025),
              q97.5 = mean_quantile(value, scenario ,scen_2=scen_2, scen_1=scen_1, probs = .975))
  return(tab_out)
}