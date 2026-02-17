##############################
# 1) KÜTÜPHANELER
##############################
library(readxl)
library(forecast)     
library(trend)        
library(dplyr)
library(ggplot2)
library(car)
library(tseries)
library(xts)
library(Kendall)


##############################
# 2) VERi
##############################

data <- read_excel("C:/Users/MONSTER/OneDrive - Dokuz Eylül Üniversitesi/Masaüstü/MSFT Geçmiş Verileri.xlsx")

names(data) <- c("tarih", "close", "acilis", "yuksek", "dusuk", "hacim", "fark_yuzde")

data$tarih <- as.Date(data$tarih, format = "%d.%m.%Y")

data$close <- as.numeric(gsub(",", ".", data$close))

data <- data[order(data$tarih), ]
msft_close <- xts(data$close, order.by = data$tarih)
plot(msft_close,
     main = "MSFT - Kapanış (Şimdi) Günlük Zaman Serisi",
     ylab = "Fiyat", xlab = "Tarih")
########################################################################
##############################
# 3) ZAMAN SERİSİ 
##############################

Y <- data$close
Y_ts <- ts(Y,frequency = 21)

data$ay <- format(data$tarih, "%Y-%m")   

istatistikler <- data %>%
  group_by(ay) %>%
  summarise(
    Gözlem = n(),
    Ortalama = mean(close, na.rm = TRUE),
    Medyan = median(close, na.rm = TRUE),
    Minimum = min(close, na.rm = TRUE),
    Maksimum = max(close, na.rm = TRUE),
    Standart_Sapma = sd(close, na.rm = TRUE)
  )
print(istatistikler)
ggplot(data, aes(x = ay, y = close)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Aylık Kapanış Fiyatı Dağılımları (Boxplot)",
       x = "Ay",
       y = "Kapanış Fiyatı")

is_outlier <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
}

data <- data %>%
  mutate(ay = format(as.Date(date), "%Y-%m"))

outlier_summary <- data %>%
  group_by(ay) %>%
  summarise(
    n_total = n(),
    n_outlier = sum(is_outlier(close))
  )

print(outlier_summary)
########################################################
#normallik ve durağanlık testler
par(mfrow=c(1,2))
hist(data$close,
     breaks = 30,
     col = "lightblue",
     main = "MSFT – Close Fiyatlarının Histogramı",
     xlab = "Fiyat")

jarque.bera.test(data$close)
shapiro.test(sample(data$close))   
qqnorm(data$close, main = "QQ Plot – MSFT Close")
qqline(data$close, col="red")
adf.test(data$close)
dev.off()
######################################
n <- length(Y)
group <- rep(c("ilk_yari", "ikinci_yari"), each = n/2)
Y_cut <- Y[1:(2 * floor(n/2))]  
# Levene testi
leveneTest(Y_cut ~ as.factor(group))

n <- length(Y)

group1 <- Y[1:round(n*0.4)]   
group2 <- Y[(round(n*0.4)+1):n]   

leveneTest(c(group1, group2), 
           group = factor(c(rep(1, length(group1)), rep(2, length(group2)))))  




##############################
# 4)DECOMPOSITION
##############################
print("--- TEST 3: BOX-COX PARAMETRESI (LAMBDA) ---")
lambda_val <- BoxCox.lambda(Y_ts)
print(paste("Hesaplanan Lambda Degeri:", round(lambda_val, 4)))

if(abs(lambda_val - 1) < 0.5) {
  print("YORUM: Lambda 1'e yakin. Veri 'ADDITIVE' (Toplamsal) yapiya uygun.")
  oneri_boxcox <- "additive"
} else { 
  print("YORUM: Lambda 0'a yakin. Veri 'MULTIPLICATIVE' (Carpimsal) yapiya uygun.")
  oneri_boxcox <- "multiplicative"
}


# Additive decomposition 
decomp_add <- decompose(Y_ts)

plot(decomp_add)


##############################
# 5) TREND ve MEVSİMSELLİK TESTLERİ
##############################
mk_result <- MannKendall(Y_ts)  
print(mk_result)            


MannKendall(Y_ts)  


gun_ay <- cycle(Y_ts)                  
kw_result <- kruskal.test(as.numeric(Y_ts) ~ as.factor(gun_ay))
kw_result

adf.test(Y_ts)              

ggtsdisplay(Y_ts, main="ACF ve PACF Grafikleri")

######################################################
# 5) HOLT ÜSSEL DÜZLEŞTİRME 


# =========================
# =========================
df <- data %>%
  mutate(
    tarih = as.Date(tarih, "%d.%m.%Y"),
    y = as.numeric(gsub(",", ".", close))
  ) %>%
  filter(!is.na(tarih), !is.na(y)) %>%
  arrange(tarih) %>%
  distinct(tarih, .keep_all = TRUE) %>%
  mutate(t = row_number()) %>%
  select(tarih, y, t)

# =========================
# =========================
m_lin  <- lm(y ~ t, data=df)
m_quad <- lm(y ~ t + I(t^2), data=df)
m_exp  <- lm(log(y) ~ t, data=df)   

# =========================
# =========================
rmse <- function(a,p) sqrt(mean((a-p)^2, na.rm=TRUE))
mape <- function(a,p) mean(abs((a-p)/a), na.rm=TRUE)*100

pred_y <- function(m, data, exp_back=FALSE){
  p <- as.numeric(predict(m, newdata=data))
  if(exp_back) p <- exp(p)
  p
}

sum_tab <- function(m, pred_vec){
  s <- summary(m)
  data.frame(
    R2        = s$r.squared,
    Adj_R2    = s$adj.r.squared,
    max_p_coef = max(s$coefficients[,4], na.rm=TRUE),
    RMSE      = rmse(df$y, pred_vec),
    MAPE      = mape(df$y, pred_vec)
  )
}

tab <- rbind(
  cbind(Model="Linear",      sum_tab(m_lin,  pred_y(m_lin, df))),
  cbind(Model="Quadratic",   sum_tab(m_quad, pred_y(m_quad, df))),
  cbind(Model="Exponential", sum_tab(m_exp,  pred_y(m_exp,  df, exp_back=TRUE)))
)

print(tab)

# =========================
# =========================
next_bdays <- function(last_date, h){
  cand <- seq.Date(last_date + 1, by="day", length.out=h*3)
  cand[!(weekdays(cand) %in% c("Saturday","Sunday"))][1:h]
}

h <- 21
future_dates <- next_bdays(max(df$tarih), h)
future_df <- data.frame(t = (nrow(df)+1):(nrow(df)+h))

PI  <- predict(m_quad, newdata=future_df, interval="prediction", level=0.95)
fit <- PI[,"fit"]; lo <- PI[,"lwr"]; hi <- PI[,"upr"]

future_table <- data.frame(
  Tarih   = future_dates,
  Tahmin  = round(as.numeric(fit), 2),
  PI_Low  = round(as.numeric(lo), 2),
  PI_High = round(as.numeric(hi), 2)
)
print(head(future_table, 10))

ggplot() +
  geom_line(data=df, aes(x=tarih, y=y), linewidth=0.9) +
  geom_vline(xintercept=max(df$tarih), linetype="dashed") +
  geom_line(data=data.frame(tarih=future_dates, fit=fit),
            aes(x=tarih, y=fit), linewidth=1.0) +
  geom_ribbon(data=data.frame(tarih=future_dates, lo=lo, hi=hi),
              aes(x=tarih, ymin=lo, ymax=hi), alpha=0.15) +
  labs(title="21 İş Günü Forecast (Quadratic)", x="Tarih", y="Fiyat") +
  theme_minimal()

# --- Artık analizi ---
checkresiduals(m_quad)



##############################
# 6) HOLT ÜSSEL DÜZLEŞTİRME 
##############################
alphas <- seq(0.1, 0.9, by = 0.1)
SSE_vec <- numeric(length(alphas))

# Holt'un trendli modeli 
for(i in seq_along(alphas)) {
  fit_i <- holt(Y_ts, seasonal = "additive", alpha = alphas[i])
  
  SSE_vec[i] <- sum(residuals(fit_i)^2, na.rm = TRUE)
}

data.frame(alpha = alphas, SSE = SSE_vec)
fit_holt <- holt(Y_ts, h = 20)

fit_holt           

plot(fit_holt,
     main = "Holt Modeli – 21 Adım Tahmin",
     ylab = "Fiyat",
     xlab = "Zaman")

# Holt model artık analizi
checkresiduals(fit_holt)

