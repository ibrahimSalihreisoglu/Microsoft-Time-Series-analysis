# Microsoft (MSFT) Time Series Analysis (R)

This repository contains an end-to-end **time series analysis and forecasting** workflow for Microsoft (MSFT) daily prices using **R**.

> Educational project. Not financial advice.

---

## Objective
- Analyze MSFT **daily closing prices** as a time series  
- Inspect **trend, stationarity, and seasonality**  
- Produce a short-horizon forecast (e.g., **~20–21 trading days**) using **Holt’s exponential smoothing** and validate residuals

---

## Dataset
- File: `MSFT Geçmiş Verileri.xlsx`
- The script expects these columns (renamed inside `analysis.R`):
  - `tarih` (date), `close`, `acilis`, `yuksek`, `dusuk`, `hacim`, `fark_yuzde`

> If your Excel column order differs, update the `names(data) <- c(...)` line in `analysis.R`.

---

## Workflow
1. **Load data** from Excel (`readxl`)
2. **Preprocess**
   - parse dates (`tarih`)
   - convert `close` to numeric (comma → dot if needed)
   - sort by date, convert to `xts`
3. **Exploratory analysis**
   - time series plot
   - monthly summary stats + boxplots
   - outlier check (IQR-based)
4. **Statistical tests**
   - normality checks (Histogram, QQ plot, Jarque–Bera, Shapiro sample)
   - stationarity (ADF / KPSS)
   - trend (Mann–Kendall)
   - seasonality check (Kruskal–Wallis + ACF/PACF)
5. **Decomposition** (additive)
6. **Forecasting**
   - Holt’s method + residual diagnostics (`checkresiduals`)
   - forecast horizon: 20–21 steps

---

