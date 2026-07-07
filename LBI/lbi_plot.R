library(ggplot2)
library(gridExtra)

# =========================================================
# BASE: cálculo comum
# =========================================================
lb_base <- function(data, binwidth, l_units, linf, lmat, mk_ratio, weight) {
  
  Ind <- lb_ind(
    data = data,
    binwidth = binwidth,
    linf = linf,
    lmat = lmat,
    mk_ratio = mk_ratio,
    weight = weight
  )
  
  ymax_a <- max(Ind$L95, Ind$Lmax5, Ind$Lmean, Ind$Lc, Ind$Linf, Ind$L25, na.rm = TRUE)
  ymax_b <- max(Ind$Lmax5_Linf, Ind$L95_Linf, Ind$Pmega, Ind$Pmegaref, Ind$Lc_Lmat, Ind$L25_Lmat, na.rm = TRUE)
  ymax_c <- max(Ind$L75, Ind$Lmean, Ind$Lopt, Ind$Lmaxy, Ind$Lmat, Ind$L25, na.rm = TRUE)
  ymax_d <- max(Ind$Lmean_Lopt, Ind$Lmaxy_Lopt, na.rm = TRUE)
  ymax_e <- max(Ind$Lmat, Ind$Lmean, Ind$LFeM, na.rm = TRUE)
  ymax_f <- max(Ind$Lmean_LFeM, na.rm = TRUE)
  
  list(Ind = Ind,
       l_units = l_units,
       ymax = list(a = ymax_a,
                   b = ymax_b,
                   c = ymax_c,
                   d = ymax_d,
                   e = ymax_e,
                   f = ymax_f))
}

# =========================================================
# (a) Conservation
# =========================================================
lb_plot_a <- function(data, binwidth, l_units, linf, lmat, mk_ratio, weight) {
  
  obj <- lb_base(data, binwidth, l_units, linf, lmat, mk_ratio, weight)
  Ind <- obj$Ind
  ymax <- obj$ymax$a
  
  ggplot(Ind, aes(x = Year)) +
    
    geom_line(aes(y = L95,  color = "L95"), linewidth = 1) +
    geom_line(aes(y = Lmax5,color = "Lmax5"), linewidth = 1) +
    geom_line(aes(y = Lmat, color = "Lmat"), linetype = "dashed") +
    geom_line(aes(y = Lc,   color = "Lc"), linewidth = 1) +
    geom_line(aes(y = Linf, color = "Linf"), linetype = "dashed") +
    geom_line(aes(y = L25,  color = "L25")) +
    
    scale_color_manual(
      values = c(
        "L95"   = "purple",
        "Lmax5" = "black",
        "Lmat"  = "grey40",
        "Lc"    = "blue",
        "Linf"  = "black",
        "L25"   = "red"
      ),
      labels = c(
        expression(L["95%"]),
        expression(L["max5%"]),
        expression(L["mat"]),
        expression(L["c"]),
        expression(L["inf"]),
        expression(L["25%"])
      ),
      name = NULL
    ) +
    
    labs(title = "(a) Conservation",
         x = "Year",
         y = paste("Length (", obj$l_units, ")", sep = "")) +
    
    coord_cartesian(ylim = c(0, ymax * 1.1)) +
    theme_bw()
}

# =========================================================
# (b) Conservation ratios
# =========================================================
lb_plot_b <- function(data, binwidth, l_units, linf, lmat, mk_ratio, weight) {
  
  obj <- lb_base(data, binwidth, l_units, linf, lmat, mk_ratio, weight)
  Ind <- obj$Ind
  ymax <- obj$ymax$b
  
  ggplot(Ind, aes(x = Year)) +
    
    geom_line(aes(y = Lmax5_Linf, color = "Lmax5/Linf")) +
    geom_line(aes(y = L95_Linf,   color = "L95/Linf")) +
    geom_line(aes(y = Pmega,      color = "Pmega")) +
    geom_line(aes(y = Pmegaref,   color = "ref"), linetype = "dashed") +
    geom_line(aes(y = Lc_Lmat,    color = "Lc/Lmat")) +
    geom_line(aes(y = L25_Lmat,   color = "L25/Lmat")) +
    
    scale_color_manual(
      values = c(
        "Lmax5/Linf" = "black",
        "L95/Linf"   = "purple",
        "Pmega"      = "blue",
        "ref"        = "black",
        "Lc/Lmat"    = "red",
        "L25/Lmat"   = "darkred"
      ),
      labels = c(
        expression(L["max5%"]/L["inf"]),
        expression(L["95%"]/L["inf"]),
        expression(P["mega"]),
        "30%",
        expression(L["c"]/L["mat"]),
        expression(L["25"]/L["mat"])
      ),
      name = NULL
    ) +
    
    labs(title = "(b) Conservation",
         x = "Year",
         y = "Indicator Ratio") +
    
    coord_cartesian(ylim = c(0, ymax * 1.1)) +
    theme_bw()
}

# =========================================================
# (c) Optimal Yield
# =========================================================
lb_plot_c <- function(data, binwidth, l_units, linf, lmat, mk_ratio, weight) {
  
  obj <- lb_base(data, binwidth, l_units, linf, lmat, mk_ratio, weight)
  Ind <- obj$Ind
  ymax <- obj$ymax$c
  
  ggplot(Ind, aes(x = Year)) +
    
    geom_line(aes(y = L75,   color = "L75")) +
    geom_line(aes(y = Lmean, color = "Lmean")) +
    geom_line(aes(y = Lopt,  color = "Lopt"), linetype = "dashed") +
    geom_line(aes(y = Lmaxy, color = "Lmaxy")) +
    geom_line(aes(y = Lmat,  color = "Lmat"), linetype = "dashed") +
    geom_line(aes(y = L25,   color = "L25")) +
    
    scale_color_manual(
      values = c(
        "L75"   = "red",
        "Lmean" = "darkred",
        "Lopt"  = "black",
        "Lmaxy" = "green",
        "Lmat"  = "grey40",
        "L25"   = "red"
      ),
      labels = c(
        expression(L["75%"]),
        expression(L["mean"]),
        expression(L["opt"]),
        expression(L["maxy"]),
        expression(L["mat"]),
        expression(L["25%"])
      ),
      name = NULL
    ) +
    
    labs(title = "(c) Optimal Yield",
         x = "Year",
         y = paste("Length (", obj$l_units, ")", sep = "")) +
    
    coord_cartesian(ylim = c(0, ymax * 1.1)) +
    theme_bw()
}

# =========================================================
# (d) Ratios optimal yield
# =========================================================
lb_plot_d <- function(data, binwidth, l_units, linf, lmat, mk_ratio, weight) {
  
  obj <- lb_base(data, binwidth, l_units, linf, lmat, mk_ratio, weight)
  Ind <- obj$Ind
  ymax <- obj$ymax$d
  
  ggplot(Ind, aes(x = Year)) +
    
    geom_line(aes(y = Lmean_Lopt, color = "mean/opt"), linewidth = 1) +
    geom_line(aes(y = Lmaxy_Lopt, color = "maxy/opt"), linewidth = 1) +
    
    scale_color_manual(
      values = c(
        "mean/opt" = "darkred",
        "maxy/opt" = "green"
      ),
      labels = c(
        expression(L["mean"]/L["opt"]),
        expression(L["maxy"]/L["opt"])
      ),
      name = NULL
    ) +
    
    labs(title = "(d) Optimal Yield",
         x = "Year",
         y = "Indicator Ratio") +
    
    coord_cartesian(ylim = c(0, ymax * 1.1)) +
    theme_bw()
}

# =========================================================
# (e) MSY
# =========================================================
lb_plot_e <- function(data, binwidth, l_units, linf, lmat, mk_ratio, weight) {
  
  obj <- lb_base(data, binwidth, l_units, linf, lmat, mk_ratio, weight)
  Ind <- obj$Ind
  ymax <- obj$ymax$e
  
  ggplot(Ind, aes(x = Year)) +
    
    geom_line(aes(y = Lmat,  color = "Lmat"), linetype = "dashed") +
    geom_line(aes(y = Lmean, color = "Lmean")) +
    geom_line(aes(y = LFeM,  color = "LFeM"), linetype = "dashed") +
    
    scale_color_manual(
      values = c(
        "Lmat"  = "grey40",
        "Lmean" = "darkred",
        "LFeM"  = "blue"
      ),
      labels = c(
        expression(L["mat"]),
        expression(L["mean"]),
        expression(L["F=M"])
      ),
      name = NULL
    ) +
    
    labs(title = "(e) Maximum Sustainable Yield",
         x = "Year",
         y = paste("Length (", obj$l_units, ")", sep = "")) +
    
    coord_cartesian(ylim = c(0, ymax * 1.1)) +
    theme_bw()
}

# =========================================================
# (f) MSY ratio
# =========================================================
lb_plot_f <- function(data, binwidth, l_units, linf, lmat, mk_ratio, weight) {
  
  obj <- lb_base(data, binwidth, l_units, linf, lmat, mk_ratio, weight)
  Ind <- obj$Ind
  ymax <- obj$ymax$f
  
  ggplot(Ind, aes(x = Year)) +
    
    geom_line(aes(y = Lmean_LFeM, color = "mean/F=M"), linewidth = 1) +
    
    scale_color_manual(
      values = c(
        "mean/F=M" = "blue"
      ),
      labels = c(
        expression(L["mean"]/L["F=M"])
      ),
      name = NULL
    ) +
    
    labs(title = "(f) MSY ratio",
         x = "Year",
         y = "Indicator Ratio") +
    
    coord_cartesian(ylim = c(0, ymax * 1.1)) +
    theme_bw()
}

# =========================================================
# WRAPPER: todos juntos
# =========================================================
lb_plot_all <- function(data, binwidth, l_units, linf, lmat, mk_ratio, weight, ncol = 2) {
  
  gridExtra::grid.arrange(
    lb_plot_a(data, binwidth, l_units, linf, lmat, mk_ratio, weight),
    lb_plot_b(data, binwidth, l_units, linf, lmat, mk_ratio, weight),
    lb_plot_c(data, binwidth, l_units, linf, lmat, mk_ratio, weight),
    lb_plot_d(data, binwidth, l_units, linf, lmat, mk_ratio, weight),
    lb_plot_e(data, binwidth, l_units, linf, lmat, mk_ratio, weight),
    lb_plot_f(data, binwidth, l_units, linf, lmat, mk_ratio, weight),
    ncol = ncol
  )
}