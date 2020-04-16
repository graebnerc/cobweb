supply_func <- function(p, intercept_s=-0.5, slope_s=0.7){
  intercept_s + slope_s * p
}

demand_func <- function(p, intercept_d=5.0, slope_d=-1.0){
  intercept_d + slope_d * p
}

make_supply_demand_plot <- function(price_range, 
                                    intercept_nachfrage, intercept_angebot, 
                                    slope_nachfrage, slope_angebot, plot_n){
  demand <-demand_func(
    price_range, 
    intercept_d = intercept_nachfrage, 
    slope_d = slope_nachfrage)
  
  supply <-supply_func(price_range, 
                       intercept_s = intercept_angebot, 
                       slope_s = slope_angebot)
  
  demand_supply_data <- tibble(
    Nachfrage=demand, Angebot=supply, Preis=price_range, Menge=price_range)
  
  p_eq <- (intercept_angebot-intercept_nachfrage)/(slope_nachfrage-slope_angebot)
  q_eq <- demand_func(p_eq, 
                      intercept_d = intercept_nachfrage, 
                      slope_d = slope_nachfrage)
  
  ggplot(data=demand_supply_data,
         mapping=aes(x=Preis)) +
    geom_line(aes(x=Preis, y=Nachfrage, color="Nachfrage")) +
    geom_line(aes(x=Preis, y=Angebot, color="Angebot")) +
    xlab("Preis") + ylab("Menge") +
    coord_cartesian(ylim = c(0, 10), xlim = c(0, 11), 
                    expand = 0) + # expansion(mult = 0, add = 0)
    scale_color_manual(values = c(col_1, col_2)) +
    geom_segment(aes(x = -Inf, y = q_eq, 
                     xend = p_eq, yend = q_eq), 
                 linetype="dashed", alpha=0.8, color="#1a171b") +
    geom_segment(aes(x = p_eq, y = -Inf, 
                     xend = p_eq, yend = q_eq), 
                 linetype="dashed", alpha=0.8, color="#1a171b") +
    ggtitle(paste0("Angebot-Nachfrage Diagram ", plot_n)) +
    theme_bw() + 
    theme(panel.border = element_blank(), 
          axis.line = element_line(), 
          legend.title = element_blank(), 
          legend.position = "bottom")
}

get_dyn_ggplot <- function(data, y_val, eq_price){
  ggplot(data, aes_string(x="Zeit", y=y_val)) +
    geom_line(color=col_3, alpha=0.85) + 
    geom_point(color=col_3, alpha=0.5) +
    geom_hline(yintercept = eq_price, alpha=0.8, color="#1a171b") +
    ylab("Preis") +
    theme_bw() +
    theme(panel.border = element_blank(), 
          axis.line = element_line(), 
          legend.title = element_blank())
}

get_eq_price <- function(intercept_supply, intercept_demand,
                         slope_supply, slope_demand){
  p_eq <- (intercept_supply-intercept_demand)/(slope_demand-slope_supply)
  return(p_eq)
}

price_func <- function(p_0, intercept_demand, slope_demand, 
                       intercept_supply, slope_supply, 
                       timesteps){
  p_eq <- (intercept_supply-intercept_demand)/(slope_demand-slope_supply)
  p_t <- p_eq - (slope_supply/slope_demand)**timesteps * (p_eq-p_0)
  return(p_t)
}