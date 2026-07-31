lb_table <- function(data,
                     binwidth,
                     l_units = "cm",
                     linf,
                     lmat,
                     mk_ratio,
                     weight) {
  
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
  
  years <- seq(max(Ind$Year, na.rm = TRUE) - 2,
               max(Ind$Year, na.rm = TRUE))
  
  #------------------------------------------------------------
  # Reference levels (with labels for table)
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
      dplyr::across(-Year, ~round(.x, 2))
    )
  
  tab <- dplyr::mutate(tab, dplyr::across(everything(), as.character))
  
  # bind row on top
  tab$Year <- as.character(tab$Year)
 


 
  data_i <- seq_len(nrow(tab))   # sequence of all data rows 
  
  #------------------------------------------------------------
  # Flextable
  #------------------------------------------------------------
  
  ft <- flextable::flextable(tab)

  ## Add Header row ##
  ft <- flextable::add_header_row(ft, values = c("", "Conservation", "Optimizing Yield", "MSY"),
  colwidths = c(1, 4, 1, 1))  
   
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::bg(ft, part = "header", bg = "#E8EAEA")
  
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
    Year = "",
    Lc_Lmat = "Lc / Lmat",
    L25_Lmat = "L25% / Lmat",
    Lmax5_Linf = "Lmax5% / Linf",
    Pmega = "Pmega",
    Lmean_Lopt = "Lmean / Lopt",
    Lmean_LFeM = "Lmean / LF=M"
  )

# Add Reference header row 
  ft <- flextable::add_header_row(
  ft,
  values = c("Reference", ">1.00", ">1.00", ">0.80", ">0.30", "≈0.90", "≥1.00"),
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
  red   <- "#f15d2a"
  
  cols <- intersect(names(ref_level), names(tab))
  
  for (column in cols) {
    
    threshold <- ref_level[[column]]
    vals <- suppressWarnings(as.numeric(tab[[column]][data_i]))
    ft <- flextable::bg(
      ft,
      i = data_i,   # <-- somente linhas de dados
      j = column,
      bg = ifelse(vals >= threshold, green, red
      )
    )
  }
  
  
  
  #------------------------------------------------------------
  # Formatting
  #------------------------------------------------------------
  
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")
  ft <- flextable::autofit(ft)
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::border(ft, i = nrow(tab), border.bottom = fp_border(color = "black", width = 1.5), part = "body")
 
  return(ft)
}
