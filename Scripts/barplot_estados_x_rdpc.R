# 


table <- read.csv("Desktop/dados_uf_2010.csv", header = TRUE, sep = ";")
rdpc = table$RDPC
estados = table$UFN
barplot(
  rdpc, 
  main = "Renda Domicilar Per Capita x Estados",
  ylab = "Renda Domicilar Per Capita",
  horiz = FALSE, 
  names.arg = estados,
  cex.names = 0.8,
  las = 3,
  beside = TRUE,
  ylim = c(0,2000)
)
