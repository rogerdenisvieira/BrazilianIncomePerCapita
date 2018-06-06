
# loading dataframe from a CSV
df_mun_rs <- read.csv("dados/dados_mun_rs_2010.csv", header = TRUE, sep = ";")

# initializing variables
RDPC <- df_mun_rs$RDPC
IDHM <- df_mun_rs$IDHM
T_SUPER25M <- df_mun_rs$T_SUPER25M
TRABPUB <- df_mun_rs$TRABPUB
PEA18M <- df_mun_rs$PEA18M
T_ENV <- df_mun_rs$T_ENV

# calculating mean values
RDPC_mean <- mean(RDPC)
IDHM_mean <- mean(IDHM)
T_SUPER25M_mean <- mean(T_SUPER25M)
TRABPUB_mean <- mean(TRABPUB)
PEA18M_mean <- mean(PEA18M)
T_ENV_mean <- mean(T_ENV)

# calculating standard deviation
RDPC_sd <- sd(RDPC)
IDHM_sd <- sd(IDHM)
T_SUPER25M_sd <- sd(T_SUPER25M)
TRABPUB_sd <- sd(TRABPUB)
PEA18M_sd <- sd(PEA18M)
T_ENV_sd <- sd(T_ENV)

# calculating median
RDPC_median <- median(RDPC)
IDHM_median <- median(IDHM)
T_SUPER25M_median <-median(T_SUPER25M)
TRABPUB_median <- median(TRABPUB)
PEA18M_median <- median(PEA18M)
T_ENV_median <- median(T_ENV)

# calculating linear regression between RDPC x IDHM
regressao <- lm(RDPC~IDHM)
plot(RDPC~IDHM, col="black", pch=1)
abline(coef(regressao), col="red1")

# calculating linear regression between RDPC x T_SUPER25M
regressao <- lm(RDPC~T_SUPER25M)
plot(RDPC~T_SUPER25M, col="black", pch=1)
abline(coef(regressao), col="red1")

# calculating linear regression between RDPC x PEA18M
regressao <- lm(RDPC~PEA18M)
plot(RDPC~PEA18M, col="black", pch=1)
abline(coef(regressao), col="red1")

# calculating linear regression between RDPC x TRABPUB
regressao <- lm(RDPC~TRABPUB)
plot(RDPC~TRABPUB, col="black", pch=1)
abline(coef(regressao), col="red1")

# calculating linear regression between RDPC x T_ENV
regressao <- lm(RDPC~T_ENV)
plot(RDPC~T_ENV, col="black", pch=1)
abline(coef(regressao), col="red1")
