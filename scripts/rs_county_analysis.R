# loading dataframe from a CSV
df_county <- read.csv("data/county_data_91-00-10.csv", header = TRUE, sep = ";")

# applying a filter into dataframe by year and state Rio Grande do Sul
df_county_rs_1991 <- subset.data.frame(df_county, ANO==1991 & UF==43)
df_county_rs_2000 <- subset.data.frame(df_county, ANO==2000 & UF==43)
df_county_rs_2010 <- subset.data.frame(df_county, ANO==2010 & UF==43)

df_poa_2010 <- subset.data.frame(df_county, ANO==2010 & UF==43 & Município== "PORTO ALEGRE")

plot(df_poa_2010$PEA18M, df_poa_2010$RDPC)


new <- df_county_rs_2010[order(-df_county_rs_2010$PEA18M),]

show(new$PEA18M, new$Município)

plot(df_county_rs_2010$IDHM, df_county_rs_2010$RDPC, ylab = "RDPC", xlab = "IDHM")
plot(df_county_rs_2010$T_SUPER25M, df_county_rs_2010$RDPC, ylab = "RDPC", xlab = "T_SUPER25M")
plot(df_county_rs_2010$TRABPUB, df_county_rs_2010$RDPC, ylab = "RDPC", xlab = "TRABPUB")
plot(df_county_rs_2010$PEA18M, df_county_rs_2010$RDPC, ylab = "RDPC", xlab = "PEA18M")
plot(df_county_rs_2010$T_ENV, df_county_rs_2010$RDPC, ylab = "RDPC", xlab = "T_ENV")


cor(df_county_rs_2010$T_ENV, df_county_rs_2010$RDPC)
hist(df_county_rs_2010$T_SUPER25M, xlab = "GINI")


# checking normal distribution
shapiro.test(df_county_rs_2010$IDHM)
shapiro.test(df_county_rs_2010$T_SUPER25M)
shapiro.test(df_county_rs_2010$TRABPUB)
shapiro.test(df_county_rs_2010$PEA18M)
shapiro.test(df_county_rs_2010$T_ENV)

shapiro.test(df_county$IDHM)



plot

