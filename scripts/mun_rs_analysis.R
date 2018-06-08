# loading dataframe from a CSV
df_county <- read.csv("data/county_data_91-00-10.csv", header = TRUE, sep = ";")

# applying a filter into dataframe by year and state Rio Grande do Sul
df_county_rs_1991 <- subset.data.frame(df_county, ANO==1991 & UF==43)
df_county_rs_2000 <- subset.data.frame(df_county, ANO==2000 & UF==43)
df_county_rs_2010 <- subset.data.frame(df_county, ANO==2010 & UF==43)

plot(df_county_rs_2010$TRABPUB, df_county_rs_2010$RDPC)

cor(df_county_rs_2010$IDHM, df_county_rs_2010$RDPC)




# initializing variables
RDPC <- df_mun_rs$RDPC
IDHM <- df_mun_rs$IDHM
T_SUPER25M <- df_mun_rs$T_SUPER25M
TRABPUB <- df_mun_rs$TRABPUB
PEA18M <- df_mun_rs$PEA18M
T_ENV <- df_mun_rs$T_ENV


