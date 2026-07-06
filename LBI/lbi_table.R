lb_table <- function(data,
                     binwidth,
                     l_units = "cm",
                     linf,
                     lmat,
                     mk_ratio,
                     weight){
  
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
  # Reference levels
  #------------------------------------------------------------
  
  ref_level <- c(
    Lc_Lmat    = 1,
    L25_Lmat   = 1,
    Lmax5_Linf = 0.8,
    Pmega      = 0.3,
    Lmean_Lopt = 0.9,
    Lmean_LFeM = 1
  )
  
  #------------------------------------------------------------
  # Years
  #------------------------------------------------------------
  
  years <- seq(max(Ind$Year, na.rm = TRUE) - 2,
               max(Ind$Year, na.rm = TRUE))
  
  #------------------------------------------------------------
  # Build table
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
      dplyr::across(-Year, ~round(.x, 2))
    )
  
  #------------------------------------------------------------
  # Flextable
  #------------------------------------------------------------
  
  ft <- flextable::flextable(tab)
  
  # 🔥 FIX DEFINITIVO DO ANO (sem separador de milhar)
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
    Year = "Year",
    Lc_Lmat = "Lc/Lmat",
    L25_Lmat = "L25/Lmat",
    Lmax5_Linf = "Lmax5/Linf",
    Pmega = "Pmega",
    Lmean_Lopt = "Lmean/Lopt",
    Lmean_LFeM = "Lmean/LF=M"
  )
  
  #------------------------------------------------------------
  # Style
  #------------------------------------------------------------
  
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::bg(ft, part = "header", bg = "#E8EAEA")
  
  #------------------------------------------------------------
  # Conditional colors (robusto)
  #------------------------------------------------------------
  
  green <- "#aec640"
  red   <- "#f15d2a"
  
  cols <- intersect(names(ref_level), names(tab))
  
  for (column in cols) {
    
    threshold <- ref_level[[column]]
    
    ft <- flextable::bg(
      ft,
      j = column,
      bg = ifelse(
        !is.na(tab[[column]]) & tab[[column]] >= threshold,
        green,
        red
      )
    )
  }
  
  #------------------------------------------------------------
  # Formatting
  #------------------------------------------------------------
  
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")
  ft <- flextable::autofit(ft)
  
  return(ft)
}
