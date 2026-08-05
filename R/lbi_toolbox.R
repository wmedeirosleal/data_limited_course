##libraries
library(ggplot2)
library (tidyverse)
library(gridExtra)
library(reshape2)
library(flextable)
library(dplyr)
library(officer)

#___
# 1 | lb_plot_freq()
## Function to plot the length frequency distribution by year
# The function first reshapes the data frame to long format and then creates a faceted bar plot of length frequency distributions by year.

lb_plot_freq <- function(
  df,
  length_col = 1,
  ncol = 2,
  x_lab = "Length (cm)",
  y_lab = "Count",
  fill_col = "#595959",
  bin = 1,
  boundary= 0.5
) {
  # length column name
  length_name <- names(df)[length_col]

  # long format
  df_long <- df %>%
    pivot_longer(
      cols = -all_of(length_name),
      names_to = "Year",
      values_to = "Count",
      names_prefix = "X"
    ) %>%
    rename(Length = all_of(length_name)) %>%
    mutate(
      Length = as.numeric(Length),
      Year   = as.factor(Year),
      Count  = as.numeric(Count))

  # plot
  ggplot(df_long, aes(x = Length, weight = Count)) +
  geom_histogram(
      binwidth = bin,
      fill = fill_col,
      color = "white",
      boundary = boundary
    ) +
  facet_wrap(~Year, ncol = ncol, scales = "fixed") +
  theme_bw() +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "#d8d9da", color = NA)
    ) +
  labs(x = x_lab, y = y_lab)
}
####--------------------####

#___
# 2 | freq_to_weight()
## Function to convert length frequency data to biomass data

freq_to_weight <- function(df, a, b) {
  weight <- a * (df$Length^b)
  df[-1] <- sweep(df[-1], 1, weight, "*")
  return(df)
}
####--------------------####

#___
# 3 | lb_plot_all
# Functions for lbi plots
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

#___
# 4 | lb_table()
##### Function for generating status table -> lb_table. ######

lb_table <- function(
  data,
  binwidth,
  l_units = "cm",
  linf,
  lmat,
  mk_ratio,
  weight
) {
  #------------------------------------------------------------
  # Calculate indicators
  #------------------------------------------------------------

  Ind <- lb_ind(
    data = data,
    binwidth = binwidth,
    linf = linf,
    lmat = lmat,
    mk_ratio = mk_ratio,
    weight = weight
  )

  #------------------------------------------------------------
  # Years
  #------------------------------------------------------------

  years <- seq(max(Ind$Year, na.rm = TRUE) - 2, max(Ind$Year, na.rm = TRUE))

  #------------------------------------------------------------
  # Reference levels (with labels for table)
  #------------------------------------------------------------

  ref_level <- c(
    Lc_Lmat = 1,
    L25_Lmat = 1,
    Lmax5_Linf = 0.8,
    Pmega = 0.3,
    Lmean_Lopt = 0.9,
    Lmean_LFeM = 1
  )

  #------------------------------------------------------------
  # Build main table
  #------------------------------------------------------------

  tab <- Ind |>
    dplyr::filter(Year %in% years) |>
    dplyr::select(
      Year,
      Lc_Lmat,
      L25_Lmat,
      Lmax5_Linf,
      Pmega,
      Lmean_Lopt,
      Lmean_LFeM
    ) |>
    dplyr::mutate(
      Year = as.integer(Year),
      dplyr::across(-Year, ~ round(.x, 2))
    )

  tab <- dplyr::mutate(tab, dplyr::across(everything(), as.character))
  #------------------------------------------------------------
  # reference row (Insert on last row)
  #------------------------------------------------------------

  # bind row on top
  tab$Year <- as.character(tab$Year)

  data_i <- seq_len(nrow(tab)) # sequence of all rows except first
  #------------------------------------------------------------
  # Flextable
  #------------------------------------------------------------

  ft <- flextable::flextable(tab)

  ## Add Header row ##
  ft <- flextable::add_header_row(
    ft,
    values = c("", "Conservation", "Optimizing Yield", "MSY"),
    colwidths = c(1, 4, 1, 1)
  )

  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bg(ft, part = "header", bg = "#E8EAEA")

  #  FIX DEFINITIVO DO ANO (sem separador de milhar)
  ft <- flextable::colformat_num(
    ft,
    j = "Year",
    digits = 0,
    big.mark = ""
  )

  #------------------------------------------------------------
  # Header labels
  #------------------------------------------------------------

  ft <- flextable::set_header_labels(
    ft,
    Year = "",
    Lc_Lmat = "Lc / Lmat",
    L25_Lmat = "L25% / Lmat",
    Lmax5_Linf = "Lmax5% / Linf",
    Pmega = "Pmega",
    Lmean_Lopt = "Lmean / Lopt",
    Lmean_LFeM = "Lmean / LF=M"
  )

  # Add Reference header row (one value per column)
  ft <- flextable::add_header_row(
    ft,
    values = c(
      "Reference",
      ">1.00",
      ">1.00",
      ">0.80",
      ">0.30",
      "≈0.90",
      "≥1.00"
    ),
    colwidths = rep(1, 7),
    top = FALSE
  )
  #------------------------------------------------------------
  # Style
  #------------------------------------------------------------

  header_bg <- "#E8EAEA"

  ft <- flextable::border(
    ft,
    i = 3,
    border.bottom = fp_border(color = "black", width = 1.5),
    part = "header"
  )

  #------------------------------------------------------------
  # Conditional colors (robusto)
  #------------------------------------------------------------

  green <- "#aec640"
  red <- "#f15d2a"

  cols <- intersect(names(ref_level), names(tab))

  for (column in cols) {
    threshold <- ref_level[[column]]
    vals <- suppressWarnings(as.numeric(tab[[column]][data_i]))
    ft <- flextable::bg(
      ft,
      i = data_i, # <-- somente linhas de dados
      j = column,
      bg = ifelse(vals >= threshold, green, red)
    )
  }

  #------------------------------------------------------------
  # Formatting
  #------------------------------------------------------------

  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")
  ft <- flextable::autofit(ft)
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::border(
    ft,
    i = nrow(tab),
    border.bottom = fp_border(color = "black", width = 1.5),
    part = "body"
  )
  return(ft)
}

####--------------------####

#___
# 5 | lbi_report ()
# Function to word output of LBI analysis.

lbi_report <- function(
  output_file = "LBI_report.docx",
  output_dir = getwd(),
  title = "Length Based Indicators",
  intro_text = "This report summarizes the LBI results.",
  stock.name = species,
  lbiplot = plots.LBI,
  lab1 = "Indicator trends",
  tab = status_table,
  lab2 = "Indicator status for the most recent three years",
  plot1 = p1
) {
  # ---- checks ----
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Please install package 'rmarkdown'")
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Please install package 'knitr'")
  }
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Please install package 'flextable'")
  }

  # ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # absolute intended output path
  out_doc <- file.path(output_dir, output_file)

  # rendering environment
  report_env <- new.env(parent = globalenv())
  report_env$intro_text <- intro_text
  report_env$stock.name <- stock.name
  report_env$lbiplot <- lbiplot
  report_env$lab1 <- lab1
  report_env$tab <- tab
  report_env$lab2 <- lab2
  report_env$title <- title
  report_env$plot1 <- plot1

  # temp files
  rmd_file <- tempfile(fileext = ".Rmd")
  table_img <- tempfile(fileext = ".png")

  # Rmd content
  rmd_text <- c(
    "---",
    "title: \"\"",
    "output:",
    "  word_document:",
    "    toc: false",
    "fontsize: 11pt",
    "geometry: margin=1in",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "```",
    "",
    "# `r title`",
    "",
    "`r intro_text`",
    "",
    "Stock name: *`r stock.name`*",
    "",
    "```{r fig1, fig.width=10, fig.height=8}",
    "grid::grid.newpage()",
    "grid::grid.draw(plot(lbiplot))",
    "```",
    "",
    "**Figure 1:** `r lab1`",
    "\\vspace{\\baselineskip}",
    "",
    "**Table 1:** `r lab2`",
    "",
    "```{r tab1}",
    "tab",
    "```",
    "",
    "```{r fig2, fig.width=10, fig.height=8}",
    "grid::grid.newpage()",
    "grid::grid.draw(plot1)",
    "```",
    "",
    "**Figure 2:** Binned length frequency distributions by year"
  )

  writeLines(rmd_text, con = rmd_file)

  # render safely
  rendered_path <- NULL
  tryCatch(
    {
      rendered_path <- rmarkdown::render(
        input = rmd_file,
        output_file = basename(out_doc),
        output_dir = dirname(out_doc),
        envir = report_env,
        quiet = FALSE
      )
    },
    error = function(e) {
      stop("Render failed: ", conditionMessage(e))
    }
  )

  # verify output exists
  if (is.null(rendered_path) || !file.exists(rendered_path)) {
    stop("Render finished but DOCX was not found. Expected path: ", out_doc)
  }

  final_path <- normalizePath(rendered_path, winslash = "/", mustWork = TRUE)
  message("Word document created: ", final_path)

  invisible(final_path)
}

## END ##

