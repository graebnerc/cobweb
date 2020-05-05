col_1 <- "#006600"
col_2 <- "#800000"
col_3 <- "#004c93"

#' A linear supply funtion
#' 
#' Gives the quantity supplied for a given price or price range. The form of 
#' the function is:
#' \deqn{q_S=\alpha+\beta\cdot p}
#' where \eqn{\alpha} is \code{intercept_s} and \eqn{\beta} is \code{slope_s}.
#' So it is assumed that \code{slope_s} is positive.
#' 
#' @param p The price for which a quantity should be computed. Can be a single
#'  number or a vector
#' @param intercept_s The intercept of the supply curve 
#' @param slope_s The slope of the supply curve; assumed to be positive
#' @return The quantity supplied
supply_func <- function(p, intercept_s, slope_s){
  if (slope_s<=0){
    slope_s <- -1*slope_s
    warning(
      paste0("Cannot use supply curves with negative slope, switch sign to ",
             slope_s))
  }
  intercept_s + slope_s * p
}

#' A linear demand funtion
#' 
#' Gives the quantity demanded for a given price or price range.The form of 
#' the function is:
#' \deqn{q_D=\gamma+\delta\cdot p}
#' where \eqn{\gamma} is \code{intercept_d} and \eqn{\delta} is \code{slope_d}.
#' So it is assumed that \code{slope_d} is negative
#' 
#' @param p The price for which a quantity should be computed. Can be a single
#'  number or a vector
#'  @param intercept_d The intercept of the demand curve
#'  @param slope_d The slope of the demand curve; assumed to be negative
#' @return The quantity demanded
demand_func <- function(p, intercept_d, slope_d){
  if (slope_d>=0){
    slope_d <- -1*slope_d
    warning(
      paste0("Cannot use demand curves with positive slope, switch sign to ",
             slope_d))
  }
  intercept_d + slope_d * p
}

#' Creates a supply-demand diagram
#' 
#' Creates a supply-demand diagram with the equilibrium price being marked. 
#' Only works if there actually exists an intersection between supply and
#' demand. Currently, axes are fixed to be between 0 and 10.
#'   
#' @param price_range The price range; corresponds to x-axis.
#' @param plot_n The number of the plot; will be added to the title
#' @param equil Should the equilibrium price be marked?
#' @return A ggplot object
make_supply_demand_plot <- function(price_range, 
                                    intercept_nachfrage, intercept_angebot, 
                                    slope_nachfrage, slope_angebot, plot_n,
                                    equil=TRUE){
  demand <-demand_func(
    price_range, 
    intercept_d = intercept_nachfrage, 
    slope_d = slope_nachfrage)
  
  supply <-supply_func(price_range, 
                       intercept_s = intercept_angebot, 
                       slope_s = slope_angebot)
  
  demand_supply_data <- data.frame(
    Nachfrage=demand, Angebot=supply, Preis=price_range, Menge=price_range)
  
  p_eq <- (intercept_angebot-intercept_nachfrage)/
    (slope_nachfrage-slope_angebot)
  q_eq <- demand_func(p_eq, 
                      intercept_d = intercept_nachfrage, 
                      slope_d = slope_nachfrage)
  
  sp_plot <- ggplot(data=demand_supply_data,
         mapping=aes(x=Preis)) +
    geom_line(aes(x=Preis, y=Nachfrage, color="Nachfrage"), 
              key_glyph = draw_key_rect) +
    geom_line(aes(x=Preis, y=Angebot, color="Angebot"), 
              key_glyph = draw_key_rect) +
    xlab("Preis") + ylab("Menge") +
    coord_cartesian(ylim = c(0, 10), xlim = c(0, 11), 
                    expand = 0) + # expansion(mult = 0, add = 0)
    scale_color_manual(values = c(col_1, col_2)) +
    ggtitle(paste0("Angebot-Nachfrage Diagram ", plot_n)) +
    theme_bw() + 
    theme(panel.border = element_blank(), 
          axis.line = element_line(), 
          legend.title = element_blank(), 
          legend.position = "bottom")
  
  if (equil){
    sp_plot <- sp_plot +
      geom_segment(aes(x = -Inf, y = q_eq, 
                       xend = p_eq, yend = q_eq), 
                   linetype="dashed", alpha=0.8, color="#1a171b") +
      geom_segment(aes(x = p_eq, y = -Inf, 
                       xend = p_eq, yend = q_eq), 
                   linetype="dashed", alpha=0.8, color="#1a171b")
  }
  return(sp_plot)
}

#' Create a plot with price adjustment dynamics
#' 
#' The plot shows the price on the y-axis and the time on the x-axis
#' 
#' @param data A data.frame that should contain the columns \code{Zeit} and 
#'  \code{y_val}
#' @param y_val The name of the column that contains prices at given time steps
#' @param eq_price The equilibrium price
#' @return A ggplot object
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

#' Compute the equilibrium price for given supply-demand curves
#' 
#' Uses the following formula to compute the equilibrium price:
#' \deqn{p^*=\frac{\alpha-\gamma}{\delta-\beta}}
#' where \eqn{\alpha} is \code{intercept_supply}, \eqn{\beta} is 
#' \code{slope_supply}, \eqn{\gamma} is \code{intercept_demand} and 
#' \eqn{\delta} is \code{slope_demand}.
#' Assumes that \code{slope_supply} is positive and \code{slope_demand} is
#' negative, otherwise the values get corrected..
get_eq_price <- function(intercept_supply, intercept_demand,
                         slope_supply, slope_demand){
  if (slope_demand>=0){
    slope_demand <- -1*slope_demand
    warning(
      paste0("Cannot use demand curves with positive slope, switch sign to ",
             slope_demand))
  }
  if (slope_supply<=0){
    slope_supply <- -1*slope_supply
    warning(
      paste0("Cannot use supply curves with negative slope, switch sign to ",
             slope_supply))
  }
  p_eq <- (intercept_supply-intercept_demand)/(slope_demand-slope_supply)
  return(p_eq)
}

#' Computes price at time t
#' 
#' Gets price at time t by using the following formmula:
#' \deqn{p_t=p^* - \frac{\beta}{\delta}^t \cdot (p^*-p_0)}
#' where:
#' \deqn{p^*=\frac{\alpha-\gamma}{\delta-\beta}}
#' where \eqn{\alpha} is \code{intercept_supply}, \eqn{\beta} is 
#' \code{slope_supply}, \eqn{\gamma} is \code{intercept_demand} and
#'  \eqn{\delta} is \code{slope_demand}.
price_func <- function(p_0, intercept_demand, slope_demand, 
                       intercept_supply, slope_supply, 
                       timesteps){
  p_eq <- (intercept_supply-intercept_demand)/(slope_demand-slope_supply)
  p_t <- p_eq - (slope_supply/slope_demand)**timesteps * (p_eq-p_0)
  return(p_t)
}
