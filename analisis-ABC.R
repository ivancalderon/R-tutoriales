# Cargar_datos ------------------------------------------------------------
# No olvides cambiar tu directorio de trabajo a la carpeta que contiene
# el archivo de Excel que vas a analizar.
library("XLConnect")
df = readWorksheetFromFile("ABC_Analysis.xlsx",
                           sheet=1)

# Cálculos ----------------------------------------------------------------
df$Valor_Anual = df$Precio_unitario*df$Unidaes_vendidas
df = df[order(df$Valor_Anual, decreasing = TRUE),]
df$Suma = cumsum(df$Valor_Anual)
total = sum(df$Valor_Anual)
df$Pct_acumulado = lapply(df$Suma, (function (x) x/total))
df$Pct_acumulado = as.numeric(df$Pct_acumulado)*100
df$Clase = ifelse(df$Pct_acumulado <= 80, "A",
                  ifelse((df$Pct_acumulado > 80 & df$Pct_acumulado <= 95), "B",
                  ifelse(df$Pct_acumulado > 95, "C", NA)))
df$Clase = as.factor(df$Clase)

# Gráfico -----------------------------------------------------------------
library(ggplot2)
plot = (ggplot(df, aes(x=factor(reorder(Codigo, Pct_acumulado)),  
                       y=df$Pct_acumulado, 
                       fill=Clase))  
      + geom_col()
      + geom_hline(yintercept=80, color = "blue4")
      + labs(x="SKU", y="Porcentaje de Ventas en el Trimestre ")
      + scale_fill_manual(values=c("orange", "cyan3", "mediumpurple1"))
      + theme(axis.text=element_text(size=6)))

plot
# fin del archivo                                    
