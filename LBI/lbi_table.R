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
  
  ref_label <- c(
    Lc_Lmat    = ">1.00",
    L25_Lmat   = ">1.00",
    Lmax5_Linf = ">0.80",
    Pmega      = ">0.30",
    Lmean_Lopt = "≈0.90",
    Lmean_LFeM = "≥1.00"
  )
  
  #------------------------------------------------------------
  # Years
  #------------------------------------------------------------
  
  years <- seq(max(Ind$Year, na.rm = TRUE) - 2,
               max(Ind$Year, na.rm = TRUE))
  
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
  #------------------------------------------------------------
  # 🔥 NEW: reference row (INSERTED BELOW HEADER)
  #------------------------------------------------------------
  
  ref_row <- as.data.frame(
    as.list(c(
      "Reference",
      ">1.00",
      ">1.00",
      ">0.80",
      ">0.30",
      "≈0.90",
      "≥1.00"
    ))
  )
  
  names(ref_row) <- names(tab)
  
  # bind row on top
  tab$Year <- as.character(tab$Year)
  tab <- dplyr::bind_rows(ref_row, tab)
  
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
  
  header_bg <- "#E8EAEA"
  
  ft <- flextable::bg(
    ft,
    i = 1,
    bg = header_bg,
    part = "body"
  )
  
  ft <- flextable::bold(
    ft,
    i = 1,
    part = "body"
  )
  
  ft <- flextable::border(
    ft,
    i = 1,
    border.bottom = fp_border(color = "black", width = 1.5),
    part = "body"
  )
  
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
      i = 2:nrow(tab),   # <-- somente linhas de dados
      j = column,
      bg = ifelse(
        as.numeric(tab[[column]][2:nrow(tab)]) >= threshold,
        green,
        red
      )
    )
  }
  
  # Sobrescreve a linha Reference
  ft <- flextable::bg(
    ft,
    i = 1,
    bg = "#E8EAEA",
    part = "body"
  )
  
  ft <- flextable::bold(ft, i = 1, part = "body")
  
  #------------------------------------------------------------
  # Formatting
  #------------------------------------------------------------
  
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")
  ft <- flextable::autofit(ft)
  
  return(ft)
}
