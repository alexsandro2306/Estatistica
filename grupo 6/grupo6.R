getwd()

setwd("C:/Users/alexs/OneDrive/Ambiente de Trabalho/DWDM/2º Ano/1º Semestre/Estatística/grupo 6")

#install.packages("readxl")
library(readxl)
grupo6=read_excel("grupo6.xlsx")


attach(grupo6)

Avaliacao <- as.factor(Avaliacao)

Condutor <- as.factor(Condutor)

#Caracterizacao das Variaveis
#Qualitativa Nominal: Condutor
#Qualitativa Ordinal: Avaliacao
#Quantitativa Discreta: N_Avarias
#Quantitativa Continua: Lucro, Reparacoes

# Estatistica Descritiva

#Freq. abs.
fa_c=table(Condutor)
fa_c

fa_n=table(N_Avarias)
fa_n

fa_a=table(Avaliacao)
fa_a

#Freq. rel.
fr_c=prop.table(fa_c)
fr_c
round(fr_c, 2)

fr_n=prop.table(fa_n)
fr_n
round(fr_n, 2)

fr_a=prop.table(fa_a)
fr_a
round(fr_a, 2)

summary(Lucro)

summary(Reparacoes)

#Tabelas de dupla entrada

#Tabela Condutor x Avaliação
TDa_CA=table(Condutor,Avaliacao)
TDa_CA

TDr_CA = prop.table(TDa_CA)
round(TDr_CA, 2)

#Tabela Condutor x N_Avarias
TDa_CN = table(Condutor, N_Avarias)
TDa_CN

TDr_CN = prop.table(TDa_CN)
round(TDr_CN, 2)

#Tabela Avaliacao x N_Avarias
TDa_AN = table(Avaliacao, N_Avarias)
TDa_AN

TDr_AN = prop.table(TDa_AN)
round(TDr_AN, 2)

#Grafico circular - Condutor
#fa_c=table
pie(fa_c)

nomes_c <- c("Feminino", "Masculino")
cores <- c("pink", "skyblue")
rotulo <- paste(nomes_c, "(", paste(fa_c), ")", sep=" ")
pie(fa_c, main="Distribuição de Veículos por Condutor", labels=rotulo, col=cores)

#Grafico circular - Avaliação
#fa_a=table
fa_a <- table(Avaliacao)
fa_a

fr_a <- prop.table(fa_a)
fr_a
av_nomes <- c("Muito Frequente", "Frequente", "Pouco frequente")
cores_av <- c("green", "yellow", "red")
texto_av <- paste(av_nomes, " (", round(fr_a * 100, 0), "%)", sep="")
pie(fr_a, main="Percentagem de Veículos por Avaliação", labels=texto_av, col=cores_av)

#Grafico barras - N_Avarias
barplot(fa_n, main="Distribuição do Número de Avarias", xlab="Número de Avarias", ylab="Número de Veículos", col="skyblue", ylim=c(0,5))

#Grafico barras - Avaliaçao
nomes_av <- c("Muito Frequente", "Frequente", "Pouco frequente")
cores_av <- c("green", "yellow", "red")

barplot(fa_a, main="Distribuição da Avaliação dos Veículos", xlab="Avaliação", ylab="Número de Veículos", names.arg=nomes_av, col=cores_av, ylim=c(0,10))

barplot(fr_a, main="Distribuição da Avaliação dos Veículos (%)", xlab="Avaliação", ylab="Proporção de Veículos", names.arg=nomes_av, col=cores_av, ylim=c(0,0.6))

#Histograma - Lucro
h_l <- hist(Lucro, main="Distribuição do Lucro", 
            xlab="Lucro (€)", ylab="Número de Veículos", 
            col="seagreen", ylim=c(0,5), xlim=c(0,2000))
h_l

freq_rel_l = h_l$counts / length(Lucro)
freq_rel_l

#Histograma - Reparaçoes
h_r <- hist(Reparacoes, main="Distribuição do Custo de Reparações", xlab="Reparações (€)", ylab="Número de Veículos", col="coral", ylim=c(0,8), xlim=c(0,200))
h_r

freq_rel_r = h_r$counts / length(Reparacoes)
freq_rel_r

#Boxplot simples - Lucro
b_l = boxplot(Lucro, main="Diagrama de Extremos e Quartis - Lucro", ylab="Lucro (€)", col="seagreen")
b_l

summary(Lucro)
IQR(Lucro)

#Boxplot simples - Reparacoes
b_r = boxplot(Reparacoes, main="Diagrama de Extremos e Quartis - Reparações", ylab="Reparações (€)", col="coral")
b_r

summary(Reparacoes)
IQR(Reparacoes)

#Boxplot simples - N_Avarias
b_n = boxplot(N_Avarias, main="Diagrama de Extremos e Quartis - N° Avarias", ylab="Número de Avarias", col="skyblue")
b_n

summary(N_Avarias)
IQR(N_Avarias)

#Boxplot multiplo - Lucro por Condutor
bx_lc = boxplot(Lucro ~ Condutor, main="Comparação do Lucro por Tipo de Condutor", ylab="Lucro (€)", xlab="", names=c("Feminino", "Masculino"), col=c("pink", "skyblue"))
bx_lc

tapply(Lucro, Condutor, summary)

mean_lc = tapply(Lucro, Condutor, mean)
mean_lc

sd_lc = tapply(Lucro, Condutor, sd)
sd_lc

CV_lc = sd_lc / mean_lc
CV_lc

#Boxplot multiplo - Reparaçoes por Condutor
bx_rc = boxplot(Reparacoes ~ Condutor, main="Comparação das Reparações por Tipo de Condutor", ylab="Reparações (€)", xlab="", names=c("Feminino", "Masculino"), col=c("pink", "skyblue"))
bx_rc

tapply(Reparacoes, Condutor, summary)

mean_rc = tapply(Reparacoes, Condutor, mean)
mean_rc

sd_rc = tapply(Reparacoes, Condutor, sd)
sd_rc

CV_rc = sd_rc / mean_rc
CV_rc

#Boxplot multiplo - N_Avarias por Condutor
bx_nc = boxplot(N_Avarias ~ Condutor, main="Comparação do N° de Avarias por Tipo de Condutor", ylab="Número de Avarias", xlab="", names=c("Feminino", "Masculino"), col=c("pink", "skyblue"))
bx_nc

tapply(N_Avarias, Condutor, summary)

mean_nc = tapply(N_Avarias, Condutor, mean)
mean_nc

sd_nc = tapply(N_Avarias, Condutor, sd)
sd_nc

CV_nc = sd_nc / mean_nc
CV_nc

#Boxplot multiplo - Lucro por Avaliaçao
bx_la = boxplot(Lucro ~ Avaliacao, main="Comparação do Lucro por Avaliação do Veículo", ylab="Lucro (€)", xlab="Avaliação", names=c("Muito frequente", "Frequente", "Pouco frequente"), col=c("green", "yellow", "red"))
bx_la

tapply(Lucro, Avaliacao, summary)
table(Avaliacao, Lucro)

#Boxplot multiplo - Reparacoes por Avaliacao
bx_ra = boxplot(Reparacoes ~ Avaliacao, main="Comparação das Reparações por Avaliação do Veículo", ylab="Reparações (€)", xlab="Avaliação", names=c("Muito frequente", "Frequente", "Pouco frequente"), col=c("green", "yellow", "red"))
bx_ra

tapply(Reparacoes, Avaliacao, summary)

#Grafico de Barras Empilhado - Avaliaçao por Condutor
cores_emp <- c("green", "yellow", "red")
t_ac <- table(Avaliacao, Condutor)
t_ac

colnames(t_ac) <- c("Feminino", "Masculino")
rownames(t_ac) <- c("Muito Frequente", "Frequente", "Pouco Frequente")

barplot(t_ac, col=cores_emp, ylab="Número de Veículos", xlab="", main="Frequência de Utilização por Tipo de Condutor", legend=TRUE, ylim=c(0,12))

#Regressao Linear

#1. Lucro x Reparacoes

# Diagrama de dispersão
plot(Reparacoes, Lucro, main="Diagrama de Dispersão: Lucro vs Reparações", xlab="Reparações (€)", ylab="Lucro (€)", col="blue", pch=19)

# Reta de regressão
rl_lr = lm(Lucro ~ Reparacoes) # Y em função de X
rl_lr

# Equação: Lucro = a + b * Reparacoes
# Coeficientes:
# (Intercept) = a
# Reparacoes = b

abline(rl_lr, col="red", lwd=2) # Desenhar a reta

# Análise dos coeficientes
summary(rl_lr)

# Coeficiente de correlação
cor_lr = cor(Reparacoes, Lucro)
cor_lr

# Coeficiente de determinação
r2_lr = cor_lr^2
r2_lr

# Interpretação:
# cor < 0: relação negativa (quando um aumenta, o outro diminui)
# cor > 0: relação positiva (quando um aumenta, o outro também)
# |cor| próximo de 1: relação forte
# |cor| próximo de 0: relação fraca

# Previsões (exemplos)
# Se Reparacoes = 100€, qual o Lucro estimado?
coef(rl_lr)[1] + coef(rl_lr)[2] * 100

# Se Reparacoes = 150€, qual o Lucro estimado?
coef(rl_lr)[1] + coef(rl_lr)[2] * 500

# 2. Lucro x N_Avarias

plot(N_Avarias, Lucro, main="Diagrama de Dispersão: Lucro vs N° de Avarias", xlab="Número de Avarias", ylab="Lucro (€)", col="darkgreen", pch=19)

rl_ln = lm(Lucro ~ N_Avarias)
rl_ln
abline(rl_ln, col="red", lwd=2)

summary(rl_ln)

cor_ln = cor(N_Avarias, Lucro)
cor_ln

r2_ln = cor_ln^2
r2_ln

# 3. Reparacoes vs N_Avarias

plot(N_Avarias, Reparacoes, main="Diagrama de Dispersão: Reparações vs N° de Avarias", xlab="Número de Avarias", ylab="Reparações (€)", col="coral", pch=19)

rl_rn = lm(Reparacoes ~ N_Avarias)
rl_rn
abline(rl_rn, col="red", lwd=2)

summary(rl_rn)

cor_rn = cor(N_Avarias, Reparacoes)
cor_rn

r2_rn = cor_rn^2
r2_rn

# Medidas descritivas globais

#Lucro
cat("\n=== LUCRO ===\n")
summary(Lucro)
cat("Desvio Padrão:", sd(Lucro), "\n")
cat("Variância:", var(Lucro), "\n")
cat("Coeficiente de Variação:", sd(Lucro)/mean(Lucro), "\n")

#Reparacoes
cat("\n=== REPARAÇÕES ===\n")
summary(Reparacoes)
cat("Desvio Padrão:", sd(Reparacoes), "\n")
cat("Variância:", var(Reparacoes), "\n")
cat("Coeficiente de Variação:", sd(Reparacoes)/mean(Reparacoes), "\n")

#N_Avarias
cat("\n=== NÚMERO DE AVARIAS ===\n")
summary(N_Avarias)
cat("Desvio Padrão:", sd(N_Avarias), "\n")
cat("Variância:", var(N_Avarias), "\n")
cat("Coeficiente de Variação:", sd(N_Avarias)/mean(N_Avarias), "\n")

# CONCLUSÕES E INTERPRETAÇÕES
# 1. Caracterização das variáveis:
#    - Condutor: variável qualitativa nominal (M/F)
#    - N_Avarias: variável quantitativa discreta
#    - Lucro: variável quantitativa contínua
#    - Reparacoes: variável quantitativa contínua
#    - Avaliacao: variável qualitativa ordinal (1=Muito Frequente, 2=Frequente, 3=Pouco Frequente)

# 2. Distribuições:
#    - Analisar os histogramas e gráficos de barras
#    - Identificar assimetrias e outliers nos boxplots

# 3. Comparações entre grupos:
#    - Comparar Lucro, Reparações e N_Avarias entre condutores M/F
#    - Comparar estas variáveis por Frequência de Utilização (Muito Freq./Freq./Pouco Freq.)
#    - Usar coeficiente de variação para avaliar representatividade

# 4. Correlações:
#    - Analisar relação entre Lucro e Reparações
#    - Analisar relação entre Lucro e N_Avarias
#    - Analisar relação entre Reparações e N_Avarias
#    - Interpretar coeficientes de correlação e determinação


### INFERENCIA ESTATISTICA


# 1. TESTES PARA VARIÁVEIS QUALITATIVAS

### Teste de Proporção: Condutor (F vs M)
# H0: p(F) = p(M) = 0.5 (proporção 50-50)
# H1: Proporções diferentes de 0.5

fa_c = table(Condutor)
prop.test(fa_c)
# Interpretar: p > 0.05 ou p < 0.05?


### Qui-Quadrado: Condutor × Frequência de Utilização
# H0: Condutor e Avaliação são independentes
# H1: Há associação entre Condutor e Avaliação

TDa_CA = table(Condutor, Avaliacao)
chisq.test(TDa_CA)


### Qui-Quadrado: Condutor × N° Avarias
# H0: Condutor e N_Avarias são independentes
# H1: Há associação entre Condutor e N_Avarias

TDa_CN = table(Condutor, N_Avarias)
chisq.test(TDa_CN)



# 2. TESTES DE NORMALIDADE

# IMPORTANTE: n = 17 < 30 → TLC NÃO se aplica!
# Temos de testar a normalidade para decidir que testes usar.

### Lucro
# H0: A variável segue distribuição Normal
# H1: Não segue Normal

shapiro.test(Lucro)
# Se p < 0.05 → NÃO é normal → usar testes não-paramétricos
# Se p > 0.05 → Pode ser normal → usar testes paramétricos

qqnorm(Lucro, main="Q-Q Plot: Lucro")
qqline(Lucro, col="red", lwd=2)


### Reparações
shapiro.test(Reparacoes)

qqnorm(Reparacoes, main="Q-Q Plot: Reparações")
qqline(Reparacoes, col="red", lwd=2)

### N° Avarias
shapiro.test(N_Avarias)

qqnorm(N_Avarias, main="Q-Q Plot: Número de Avarias")
qqline(N_Avarias, col="red", lwd=2)



# 2.5. ESTIMAÇÃO DE MÉDIAS POPULACIONAIS E INTERVALOS DE CONFIANÇA

# Esta secção estima as médias populacionais com intervalos de confiança a 95%
# IMPORTANTE: Requisito obrigatório do enunciado!

cat("\n")
cat("###############################################################\n")
cat("## INTERVALOS DE CONFIANÇA PARA MÉDIAS POPULACIONAIS (95%) ##\n")
cat("###############################################################\n")

### IC 95% para a média do Lucro
cat("\n=== INTERVALO DE CONFIANÇA 95%: Lucro ===\n")
ic_lucro <- t.test(Lucro)
ic_lucro
# Interpretar: 
# - Média amostral (mean of x)
# - IC 95%: [limite inferior, limite superior]
# - Com 95% de confiança, a média populacional do lucro está neste intervalo

### IC 95% para a média das Reparações
cat("\n=== INTERVALO DE CONFIANÇA 95%: Reparações ===\n")
ic_rep <- t.test(Reparacoes)
ic_rep

### IC 95% para a média de N_Avarias
cat("\n=== INTERVALO DE CONFIANÇA 95%: N° Avarias ===\n")
ic_av <- t.test(N_Avarias)
ic_av


### IC 95% por Grupo: Lucro por Condutor
cat("\n=== IC 95% LUCRO POR CONDUTOR ===\n")
cat("Feminino:\n")
ic_lucro_f <- t.test(Lucro[Condutor == "F"])
ic_lucro_f

cat("\nMasculino:\n")
ic_lucro_m <- t.test(Lucro[Condutor == "M"])
ic_lucro_m


### IC 95% por Grupo: Reparações por Condutor
cat("\n=== IC 95% REPARAÇÕES POR CONDUTOR ===\n")
cat("Feminino:\n")
ic_rep_f <- t.test(Reparacoes[Condutor == "F"])
ic_rep_f

cat("\nMasculino:\n")
ic_rep_m <- t.test(Reparacoes[Condutor == "M"])
ic_rep_m


### Resumo dos Intervalos de Confiança
cat("\n")
cat("============================================================\n")
cat("       RESUMO DOS INTERVALOS DE CONFIANÇA (95%)            \n")
cat("============================================================\n")

cat("\nLUCRO GLOBAL:\n")
cat("  Média:", round(ic_lucro$estimate, 2), "€\n")
cat("  IC 95%: [", round(ic_lucro$conf.int[1], 2), "€ ;", round(ic_lucro$conf.int[2], 2), "€]\n")

cat("\nREPARAÇÕES GLOBAL:\n")
cat("  Média:", round(ic_rep$estimate, 2), "€\n")
cat("  IC 95%: [", round(ic_rep$conf.int[1], 2), "€ ;", round(ic_rep$conf.int[2], 2), "€]\n")

cat("\nN° AVARIAS GLOBAL:\n")
cat("  Média:", round(ic_av$estimate, 2), "\n")
cat("  IC 95%: [", round(ic_av$conf.int[1], 2), ";", round(ic_av$conf.int[2], 2), "]\n")

cat("\n")
cat("============================================================\n\n")



# 3. COMPARAÇÃO DE MÉDIAS (Testes Paramétricos)

# Usar APENAS se a normalidade for verificada (p > 0.05)
# Se normalidade falhar, ir para secção 4 (testes não-paramétricos)

### Teste t: Lucro por Condutor
# H0: μ(F) = μ(M) → Médias iguais
# H1: μ(F) ≠ μ(M) → Médias diferentes

t.test(Lucro ~ Condutor)
# Interpretar: p > 0.05 ou p < 0.05?
# Se p < 0.05 → Há diferenças significativas no lucro médio


### Teste t: Reparações por Condutor
# H0: μ(F) = μ(M)
# H1: μ(F) ≠ μ(M)

t.test(Reparacoes ~ Condutor)


### Teste t: N° Avarias por Condutor
# H0: μ(F) = μ(M)
# H1: μ(F) ≠ μ(M)

t.test(N_Avarias ~ Condutor)


### ANOVA: Lucro por Frequência de Utilização (3 grupos)
# H0: μ(Muito Freq.) = μ(Freq.) = μ(Pouco Freq.)
# H1: Pelo menos uma média é diferente

anova(lm(Lucro ~ Avaliacao))
# Interpretar: p > 0.05 ou p < 0.05?


### ANOVA: Reparações por Frequência de Utilização
# H0: μ(Muito Freq.) = μ(Freq.) = μ(Pouco Freq.)
# H1: Pelo menos uma média é diferente

anova(lm(Reparacoes ~ Avaliacao))


# 4. TESTES NÃO-PARAMÉTRICOS (Comparação de Medianas)

# Usar SE a normalidade FALHAR (p < 0.05 no Shapiro-Wilk)
# Estes testes comparam MEDIANAS em vez de MÉDIAS

### Wilcoxon: Lucro por Condutor
# H0: Mediana(F) = Mediana(M)
# H1: Medianas diferentes

wilcox.test(Lucro ~ Condutor)


# Wilcoxon: Reparações por Condutor
# H0: Mediana(F) = Mediana(M)
# H1: Medianas diferentes

wilcox.test(Reparacoes ~ Condutor)


# Kruskal-Wallis: Lucro por Frequência de Utilização
# H0: Todas as medianas são iguais
# H1: Pelo menos uma mediana é diferente

kruskal.test(Lucro ~ Avaliacao)


### Kruskal-Wallis: Reparações por Frequência de Utilização
# H0: Todas as medianas são iguais
# H1: Pelo menos uma mediana é diferente

kruskal.test(Reparacoes ~ Avaliacao)

# 5. REGRESSÃO LINEAR - MODELO 1: Lucro ~ Reparações

# Diagrama de dispersão
plot(Reparacoes, Lucro, 
     main="Diagrama de dispersão: Lucro vs Reparações", 
     xlab="Reparações (€)", ylab="Lucro (€)", 
     col="blue", pch=19)

# Modelo de regressão
rl_lr = lm(Lucro ~ Reparacoes)
rl_lr
# Equação: Lucro = a + b × Reparacoes

# Desenhar a reta
abline(rl_lr, col="red", lwd=2)

# Correlação
cor(Reparacoes, Lucro)        # r (força e direção da relação)
cor(Reparacoes, Lucro)^2      # r² (% variabilidade explicada)

# Previsões
# Se Reparacoes = 100€, qual o Lucro?
coef(rl_lr)[1] + coef(rl_lr)[2] * 100

# Se Reparacoes = 150€?
coef(rl_lr)[1] + coef(rl_lr)[2] * 500


# TESTES AO MODELO

# Teste aos coeficientes (a e b são significativos?)
summary(rl_lr)

# Normalidade das variáveis
shapiro.test(Reparacoes)
shapiro.test(Lucro)

# Normalidade dos resíduos
# H0: Resíduos seguem distribuição Normal
# H1: Resíduos NÃO seguem Normal
shapiro.test(rl_lr$residuals)
# Se p < 0.05 → Modelo pode não ser adequado

qqnorm(rl_lr$residuals, main="Q-Q Plot: Resíduos do Modelo 1 (Lucro ~ Reparações)")
qqline(rl_lr$residuals, col="red", lwd=2)

# Independência dos resíduos (Durbin-Watson)
# H0: ρ = 0 (sem autocorrelação)
# H1: ρ ≠ 0 (há autocorrelação)
library(car)
durbinWatsonTest(rl_lr)
# Se p > 0.05 → Resíduos são independentes (bom!)


# 6. REGRESSÃO LINEAR - MODELO 2: Lucro ~ N_Avarias

# Diagrama de dispersão
plot(N_Avarias, Lucro, 
     main="Diagrama de dispersão: Lucro vs N° Avarias", 
     xlab="N° Avarias", ylab="Lucro (€)", 
     col="darkgreen", pch=19)

# Modelo
rl_ln = lm(Lucro ~ N_Avarias)
rl_ln
abline(rl_ln, col="red", lwd=2)

# Correlação
cor(N_Avarias, Lucro)
cor(N_Avarias, Lucro)^2

# Previsões
# Se N_Avarias = 3, qual o Lucro?
coef(rl_ln)[1] + coef(rl_ln)[2] * 3

# Se N_Avarias = 5?
coef(rl_ln)[1] + coef(rl_ln)[2] * 5


# TESTES AO MODELO

summary(rl_ln)

shapiro.test(N_Avarias)
shapiro.test(Lucro)

shapiro.test(rl_ln$residuals)

qqnorm(rl_ln$residuals, main="Q-Q Plot: Resíduos do Modelo 2 (Lucro ~ N_Avarias)")
qqline(rl_ln$residuals, col="red", lwd=2)

durbinWatsonTest(rl_ln)


# 7. REGRESSÃO LINEAR - MODELO 3: Reparações ~ N_Avarias
# Diagrama de dispersão
plot(N_Avarias, Reparacoes, 
     main="Diagrama de dispersão: Reparações vs N° Avarias", 
     xlab="N° Avarias", ylab="Reparações (€)", 
     col="coral", pch=19)

# Modelo
rl_rn = lm(Reparacoes ~ N_Avarias)
rl_rn
abline(rl_rn, col="red", lwd=2)

# Correlação
cor(N_Avarias, Reparacoes)
cor(N_Avarias, Reparacoes)^2

# Previsões
# Se N_Avarias = 3, qual o custo de Reparações?
coef(rl_rn)[1] + coef(rl_rn)[2] * 3

# Se N_Avarias = 5?
coef(rl_rn)[1] + coef(rl_rn)[2] * 5

# TESTES AO MODELO

summary(rl_rn)

shapiro.test(N_Avarias)
shapiro.test(Reparacoes)

shapiro.test(rl_rn$residuals)

qqnorm(rl_rn$residuals, main="Q-Q Plot: Resíduos do Modelo 3 (Reparações ~ N_Avarias)")
qqline(rl_rn$residuals, col="red", lwd=2)

durbinWatsonTest(rl_rn)

