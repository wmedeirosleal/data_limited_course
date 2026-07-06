###############################################################################
#                                                                             #
#              ICES MSY Reference Points Framework                            #
#                  Length-Based Indicators (LBI)                              #
#                                                                             #
# Adaptado de: Wendell Medeiros-Leal                                          #
# GitHub: https://github.com/ices-tools-dev/LBIndicator_shiny                 #
#                                                                             #
###############################################################################

#==============================================================================
# 1. Limpar ambiente
#==============================================================================

rm(list = ls())

#==============================================================================
# 2. Diretório de trabalho
#==============================================================================

# Defina o diretório onde estão os arquivos do projeto
setwd("CAMINHO/DO/SEU/PROJETO")

#==============================================================================
# 3. Carregar pacotes
#==============================================================================


library(readxl)
library(flextable)
library(dplyr)
library(officer)
library(reshape2)
library(readr)

#==============================================================================
# 4. Carregar funções do ICES LBI e para construir a tabela de resultados
#==============================================================================

source("https://raw.githubusercontent.com/ices-tools-dev/LBIndicator_shiny/master/utilities.R")
source("https://raw.githubusercontent.com/wmedeirosleal/data_limited_course/main/LBI/lbi_table.R")

#==============================================================================
# 5. Parâmetros do estudo
#==============================================================================

# Espécie
species <- "Lutjanus purpureus"

# Arquivo de entrada

# Relação peso-comprimento (W = aL^b)
a <- 0.084
b <- 2.55

# Parâmetros biológicos
binwidth <- 1
Linf     <- 76.87
Lmat     <- 32.31
MK_ratio <- 1.55

# Arquivo de saída
output_file <- "SBR_Results_LBI.csv"

#==============================================================================
# 6. Importar dados
#==============================================================================

SBR_Freq  <- read.csv("PAR_Freq.csv")


#==============================================================================
# 7. Converter frequência em biomassa
#==============================================================================

freq_to_weight <- function(df, a, b){
  
  weight <- a * (df$MeanLength ^ b)
  
  df[-1] <- sweep(df[-1], 1, weight, "*")
  
  return(df)
}

SBR_Wal <- freq_to_weight(
  SBR_Freq,
  a = a,
  b = b
)

head(SBR_Wal)

#==============================================================================
# 8. Calcular os indicadores LBI
#==============================================================================

LBI_results <- lb_ind(
  data     = SBR_Freq,
  binwidth = binwidth,
  linf     = Linf,
  lmat     = Lmat,
  mk_ratio = MK_ratio,
  SBR_Wal
)



#==============================================================================
# 9. Plotar os indicadores LBI
#==============================================================================


pdf("LBI_plot.pdf", width = 10, height = 12)

lb_plot(
  data = SBR_Freq,
  binwidth = binwidth,
  l_units = "cm",
  linf = Linf,
  lmat = Lmat,
  mk_ratio = MK_ratio,
  weight = SBR_Wal
)

dev.off()



#==============================================================================
# 11. Tabela (traffic ligth) os indicadores LBI
#==============================================================================



Tabela <- lb_table(
  data = SBR_Freq,
  binwidth = binwidth,
  l_units = "cm",
  linf = Linf,
  lmat = Lmat,
  mk_ratio = MK_ratio,
  weight = SBR_Wal
)

Tabela # Para visualizar a tabela 

doc <- read_docx()

doc <- body_add_flextable(doc, Tabela)

print(doc, target = "LBI_Table.docx")



#==============================================================================
# 10. Exportar resultados
#==============================================================================

write_excel_csv(
  LBI_results,
  output_file,
  delim = ","
)

#==============================================================================
# 10. Fim
#==============================================================================

message("Análise concluída com sucesso!")