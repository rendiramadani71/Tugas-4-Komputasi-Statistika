#TUGAS SEBARAN DISKRIT
#Rendi Ramadani(3338250041)
#1. Jika rata-rata pelanggan datang ke toko adalah 3 orang per jam,modelkan dengan Poisson dan hitung P(X >= 5).
# Model: X ~ Poisson(lambda = 3)
lambda <- 3

# Menghitung P(X >= 5) = 1 - P(X <= 4)
prob_ge_5 <- 1 - ppois(4, lambda)
cat("Nilai P(X >= 5) untuk Poisson(lambda = 3) adalah:", prob_ge_5, "\n")


#2. Dari 100 bola (20 berwarna merah), diambil 10 tanpa pengembalian. Modelkan jumlah bola merah yang diambil dengan distribusi yang tepat.
# Parameter Populasi dan Sampel
N <- 100  # Ukuran populasi total
K <- 20   # Jumlah bola merah (sukses) di populasi
n <- 10   # Ukuran sampel yang diambil tanpa pengembalian

# Domain k (banyaknya bola merah yang mungkin terambil)
k_domain <- seq(from = max(0, n + K - N), to = min(n, K))

# Menghitung PMF P(X = k)
pmf_hyper <- dhyper(k_domain, m = K, n = N - K, k = n)

# Menampilkan tabel distribusi peluang
tabel_hiper <- data.frame(
  k_sukses = k_domain, 
  Peluang = round(pmf_hyper, 5)
)
print(tabel_hiper)

# Menghitung Ekspektasi dan Varian Teoretis
exp_hyper <- n * (K / N)
var_hyper <- n * (K / N) * (1 - K / N) * ((N - n) / (N - 1))
cat("\nEkspektasi E[X] :", exp_hyper, "\n")
cat("Varian Var(X)   :", var_hyper, "\n")


#3. Simulasikan 1.000 percobaan Binomial (n = 15, p = 0.4) dan bandingkan histogram hasil simulasi dengan PMF teoretis.
set.seed(2025)

# Parameter Binomial
n_sim <- 1000  # Jumlah simulasi
n_trial <- 15  # Jumlah percobaan (size)
p_success <- 0.4  # Probabilitas sukses

# 1. Generate 1.000 data acak Binomial
data_simulasi <- rbinom(n_sim, size = n_trial, prob = p_success)

# 2. Hitung PMF Teoretis
x_vals <- 0:n_trial
pmf_teoretis <- dbinom(x_vals, size = n_trial, prob = p_success)

# 3. Visualisasi Perbandingan Histogram vs PMF Teoretis
hist(
  data_simulasi,
  breaks = seq(-0.5, n_trial + 0.5, by = 1),
  freq = FALSE,
  col = "lightblue",
  main = expression(bold("Perbandingan Simulasi Binomial vs PMF Teoretis") ~ (n==15 * "," ~ p==0.4)),
  xlab = "Jumlah Sukses (k)",
  ylab = "Probabilitas / Frekuensi Relatif"
)

# Menambahkan titik dan garis PMF Teoretis di atas Histogram
lines(x_vals, pmf_teoretis, type = "b", col = "red", lwd = 2, pch = 19)

# Menambahkan Legenda
legend(
  "topright",
  legend = c("Simulasi (Histogram Empiris)", "PMF Teoretis (dbinom)"),
  fill = c("lightblue", NA),
  col = c(NA, "red"),
  lwd = c(NA, 2),
  pch = c(NA, 19),
  bty = "n"
)