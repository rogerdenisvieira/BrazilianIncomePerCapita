# loading dataframe from a CSV
df_county <- read.csv("data/county_data_91-00-10.csv", header = TRUE, sep = ";")

# applying a filter into dataframe by year and state Rio Grande do Sul
#mun_rs_1991 <- subset.data.frame(df_county, ANO==1991 & UF==43)
mun_rs_2000 <- subset.data.frame(df_county, ANO==2000 & UF==43)
#mun_rs_2010 <- subset.data.frame(df_county, ANO==2010 & UF==43)

#poa_2000 <- subset.data.frame(df_county, ANO==2010 & UF==43 & Município== "PORTO ALEGRE")



# plotting scatter graphics
plot(mun_rs_2000$IDHM, mun_rs_2000$RDPC, ylab = "RDPC", xlab = "IDHM")
plot(mun_rs_2000$T_SUPER25M, mun_rs_2000$RDPC, ylab = "RDPC", xlab = "T_SUPER25M")
plot(mun_rs_2000$TRABPUB, mun_rs_2000$RDPC, ylab = "RDPC", xlab = "TRABPUB")

# removing scientific notation from x-axis
format(mun_rs_2000$PEA18M,scientific=FALSE)
plot(mun_rs_2000$PEA18M, mun_rs_2000$RDPC, ylab = "RDPC", xlab = "PEA18M")
plot(mun_rs_2000$T_ENV, mun_rs_2000$RDPC, ylab = "RDPC", xlab = "T_ENV")



# plotting histograms
hist(mun_rs_2000$RDPC, xlab = "RPDC", ylab="Frequência", ylim = c(0,150), xlim = c(0,1500), main = "")
hist(mun_rs_2000$IDHM, xlab = "IDHM", ylab="Frequência", ylim = c(0,200), xlim = c(0.4,0.8),main = "")
hist(mun_rs_2000$T_SUPER25M, xlab = "T_SUPER25M", ylab="Frequência", ylim = c(0,200), xlim = c(0,25), main = "")
hist(mun_rs_2000$TRABPUB, xlab = "TRABPUB", ylab="Frequência", xlim = c(0,25), ylim = c(0,140), main = "")
hist(mun_rs_2000$PEA18M, xlab = "PEA18M", ylab="Frequência", ylim = c(0,500), main = "")
hist(mun_rs_2000$T_ENV, xlab = "T_ENV", ylab="Frequência", ylim = c(0,150), xlim = c(0, 20), main = "")


reg <- lm(mun_rs_2000$RDPC ~ mun_rs_2000$IDHM 
          + mun_rs_2000$PEA18M 
          + mun_rs_2000$TRABPUB
          + mun_rs_2000$T_SUPER25M 
          + mun_rs_2000$T_ENV
)

summary(reg)



