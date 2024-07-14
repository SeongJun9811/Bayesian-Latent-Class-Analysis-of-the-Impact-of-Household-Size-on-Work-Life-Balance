library(ggplot2)
library(BayesLCA)
work <- read.csv('C:/Users/seongjun/Desktop/산업안전/산업안전/work6.csv', header = T)
View(work)
# 1인,2인,3인이상 가구로 나누기
hh1 <- work[work$hh_num == 1, ]
hh2 <- work[work$hh_num == 2, ]
hh3 <- work[work$hh_num > 2, ]

# work$hh_num의 빈도를 계산
hh_num_freq <- table(work$hh_num)

# 막대 그래프 생성
ggplot(data = as.data.frame(hh_num_freq), aes(x = as.factor(Var1), y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "hh_num", y = "Frequency", title = "Frequency of hh_num") +
  theme_minimal()
###############################################################################
# 데이터 전처리

# 변수 선택
hh1 <- hh1[,c('wtime_resilience','wtime_length1','wtime_length2',
              'wtime_arr1','wbalance','wwa1',
              'wwa2','wwa3','wwa4','wwa5')]

hh2 <- hh2[,c('wtime_resilience','wtime_length1','wtime_length2',
              'wtime_arr1','wbalance','wwa1',
              'wwa2','wwa3','wwa4','wwa5')]

hh3 <- hh3[,c('wtime_resilience','wtime_length1','wtime_length2',
              'wtime_arr1','wbalance','wwa1',
              'wwa2','wwa3','wwa4','wwa5')]

# 결측치 포함행 제거
hh1 <- na.omit(hh1)
hh2 <- na.omit(hh2)
hh3 <- na.omit(hh3)

# 모름,무응답, 거절행 제거
hh1 <- hh1[!rowSums(hh1 == 7 | hh1 == 8 | hh1 == 9), ]
hh2 <- hh2[!rowSums(hh2 == 7 | hh2 == 8 | hh2 == 9), ]
hh3 <- hh3[!rowSums(hh3 == 7 | hh3 == 8 | hh3 == 9), ]



# 이진변수화
hh1$wtime_resilience <- ifelse(hh1$wtime_resilience == 1, 1, 0)
hh1$wtime_length1 <- ifelse(hh1$wtime_length1 == 1, 1,0)
hh1$wtime_length2 <- ifelse(hh1$wtime_length2 == 1, 1,0)
hh1$wtime_arr1 <- ifelse(hh1$wtime_arr1 == 2 ,1,ifelse(hh1$wtime_arr1 == 3,1,ifelse(hh1$wtime_arr1 == 4,1,0)))
hh1$wbalance <- ifelse(hh1$wbalance %in% c(1, 2), 1, 0)
hh1$wwa1 <- ifelse(hh1$wwa1 %in% c(1,2,3), 1, 0)
hh1$wwa2 <- ifelse(hh1$wwa2 %in% c(1,2,3), 1, 0)
hh1$wwa3 <- ifelse(hh1$wwa3 %in% c(1,2,3), 1, 0)
hh1$wwa4 <- ifelse(hh1$wwa4 %in% c(1,2,3), 1, 0)
hh1$wwa5 <- ifelse(hh1$wwa5 %in% c(1,2,3), 1, 0)

# hh2 데이터프레임 변환
hh2$wtime_resilience <- ifelse(hh2$wtime_resilience == 1, 1, 0)
hh2$wtime_length1 <- ifelse(hh2$wtime_length1 == 1, 1, 0)
hh2$wtime_length2 <- ifelse(hh2$wtime_length2 == 1, 1, 0)
hh2$wtime_arr1 <- ifelse(hh2$wtime_arr1 == 2 ,1,ifelse(hh2$wtime_arr1 == 3,1,ifelse(hh2$wtime_arr1 == 4,1,0)))
hh2$wbalance <- ifelse(hh2$wbalance %in% c(1, 2), 1, 0)
hh2$wwa1 <- ifelse(hh2$wwa1 %in% c(1,2,3), 1, 0)
hh2$wwa2 <- ifelse(hh2$wwa2 %in% c(1,2,3), 1, 0)
hh2$wwa3 <- ifelse(hh2$wwa3 %in% c(1,2,3), 1, 0)
hh2$wwa4 <- ifelse(hh2$wwa4 %in% c(1,2,3), 1, 0)
hh2$wwa5 <- ifelse(hh2$wwa5 %in% c(1,2,3), 1, 0)

# hh3 데이터프레임 변환
hh3$wtime_resilience <- ifelse(hh3$wtime_resilience == 1, 1, 0)
hh3$wtime_length1 <- ifelse(hh3$wtime_length1 == 1, 1, 0)
hh3$wtime_length2 <- ifelse(hh3$wtime_length2 == 1, 1, 0)
hh3$wtime_arr1 <- ifelse(hh3$wtime_arr1 == 2 ,1,ifelse(hh3$wtime_arr1 == 3,1,ifelse(hh3$wtime_arr1 == 4,1,0)))
hh3$wbalance <- ifelse(hh3$wbalance %in% c(1, 2), 1, 0)
hh3$wwa1 <- ifelse(hh3$wwa1 %in% c(1,2,3), 1, 0)
hh3$wwa2 <- ifelse(hh3$wwa2 %in% c(1,2,3), 1, 0)
hh3$wwa3 <- ifelse(hh3$wwa3 %in% c(1,2,3), 1, 0)
hh3$wwa4 <- ifelse(hh3$wwa4 %in% c(1,2,3), 1, 0)
hh3$wwa5 <- ifelse(hh3$wwa5 %in% c(1,2,3), 1, 0)
###############################################################################
# 베이지안 잠재계층모형 적용(Bayesian latent class model)
hh1_blca2 <- blca.gibbs(hh1, G = 2, alpha = 1, beta = 1, delta = 1,
                        start.vals = c("prior"),
                        counts.n = NULL, iter = 5000, thin = 1,
                        accept=0.3, burn.in = 2000, relabel = TRUE
                        )

hh1_blca3 <- blca.gibbs(hh1, G = 3, alpha = 1, beta = 1, delta = 1,
                        start.vals = c("prior"),
                        counts.n = NULL, iter = 5000, thin = 1,
                        accept=0.3, burn.in = 2000, relabel = TRUE
                        )

hh1_blca4 <- blca.gibbs(hh1, G = 4, alpha = 1, beta = 1, delta = 1,
                        start.vals = c("prior"),
                        counts.n = NULL, iter = 5000, thin = 1,
                        accept=0.3, burn.in = 2000, relabel = TRUE
                        )


# grop1 = red, group = green
#plot(hh1_blca2, which = 5)

hh1_blca2$DIC
hh1_blca3$DIC
hh1_blca4$DIC
###############################################################################
hh2_blca2 <- blca.gibbs(hh2, G = 2, alpha = 1, beta = 1, delta = 1,
                        start.vals = c("prior"),
                        counts.n = NULL, iter = 5000, thin = 1,
                        accept=0.3, burn.in = 2000, relabel = TRUE
                        )


hh2_blca3 <- blca.gibbs(hh2, G = 3, alpha = 1, beta = 1, delta = 1,
                        start.vals = c("prior"),
                        counts.n = NULL, iter = 5000, thin = 1,
                        accept=0.3, burn.in = 2000, relabel = TRUE
                        )


hh2_blca4 <- blca.gibbs(hh2, G = 4, alpha = 1, beta = 1, delta = 1,
                       start.vals = c("prior"),
                       counts.n = NULL, iter = 5000, thin = 1,
                       accept=0.3, burn.in = 2000, relabel = TRUE
                       )

#plot(hh2_blca2, which = 5)

hh2_blca2$DIC
hh2_blca3$DIC
hh2_blca4$DIC
###############################################################################
hh3_blca2 <- blca.gibbs(hh3, G = 2, alpha = 1, beta = 1, delta = 1,
                        start.vals = c("prior"),
                        counts.n = NULL, iter = 5000, thin = 1,
                        accept=0.3, burn.in = 2000, relabel = TRUE
                        )

hh3_blca3 <- blca.gibbs(hh3, G = 3, alpha = 1, beta = 1, delta = 1,
                        start.vals = c("prior"),
                        counts.n = NULL, iter = 5000, thin = 1,
                        accept=0.3, burn.in = 2000, relabel = TRUE
                        )

hh3_blca4 <- blca.gibbs(hh3, G = 4, alpha = 1, beta = 1, delta = 1,
                       start.vals = c("prior"),
                       counts.n = NULL, iter = 5000, thin = 1,
                       accept=0.3, burn.in = 2000, relabel = TRUE
                       )

#plot(hh3_blca2, which = 5)

hh3_blca2$DIC
hh3_blca3$DIC
hh3_blca4$DIC
###############################################################################
#classprob의 수렴 확인

#방법1) Gelman and Rubin's

#raftery.diag(as.mcmc(hh1_blca2))


#방법2) traceplot
traceplot(as.mcmc(hh1_blca2$samples$classprob[,2]))
traceplot(as.mcmc(hh2_blca2$samples$classprob[,2]))
traceplot(as.mcmc(hh3_blca2$samples$classprob[,2]))


###############################################################################
#classprob 값의 비교(ANOVA검정)

#전처리
hh1_cp <- hh1_blca2$samples$classprob[,2]
hh2_cp <- hh2_blca2$samples$classprob[,2]
hh3_cp <- hh3_blca2$samples$classprob[,2]

# 데이터프레임 생성
data <- data.frame(
  group = factor(c(rep("hh1", length(hh1_cp)), rep("hh2", length(hh2_cp)), rep("hh3", length(hh3_cp)))),
  classprob = c(hh1_cp, hh2_cp, hh3_cp)
)

# ANOVA 수행
anova_result <- aov(classprob ~ group, data = data)

# ANOVA 결과 출력
summary(anova_result)

#ANOVA 검정 결과가 유의미하므로 이에 따른 사후검정
library(agricolae)
duncan <- duncan.test(anova_result, "group")
duncan

