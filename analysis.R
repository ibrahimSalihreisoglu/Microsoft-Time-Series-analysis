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


data <- read_excel("MSFT Geçmiş Verileri.xlsx")
names(data) <- c("tarih", "close", "acilis", "yuksek", "dusuk", "hacim", "fark_yuzde")
write.csv(data, "MSFT_Gecmis_Verileri.csv", row.names = FALSE)

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

# ay zaten oluşturulmuşsa tekrar oluşturma:
# data$ay <- format(data$tarih, "%Y-%m")

is_outlier <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
}

# ay'ı doğru kolondan üret
data <- data %>%
  mutate(ay = format(tarih, "%Y-%m"))

outlier_summary <- data %>%
  group_by(ay) %>%
  summarise(
    n_total = n(),
    n_outlier = sum(is_outlier(close), na.rm = TRUE),
    .groups = "drop"
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

######################################################
