rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(htmlTable)
set.seed(1)
##　試行数について
trial <- 100
## 選択刺激の報酬フィードバックの確率設定
#刺激Aについて
######報酬の確率を0.8に設定
A_probability <- 0.8
#刺激Bについて
######報酬の確率を0.2に設定
B_probability <- 0.2
## alphaの値を設定する。
alpha_input <- c(0.1, 0.5, 0.9)
## rhoの値を設定する。
rho_input <- c(1, 5, 9)
alpha_length <- length(alpha_input)
rho_length <- length(rho_input)
## 学習率alphaのパラメータ作成
alpha　<- rep(alpha_input, each = rho_length)
## 感受性rhoのパラメータ設定
rho <- rep(rho_input, times = alpha_length)
## idを設定する
id <- c(1:(alpha_length * rho_length))
###### alphaとrhoのセットを作る。
parameter_DF <- cbind(id, alpha, rho) %>% data.frame(.)
DT::datatable(parameter_DF)
ids <- rep(id, each = trial)
alphas <- rep(alpha, each = trial)
rhos <- rep(rho, each = trial)
trials <- rep(1:trial, times = alpha_length * rho_length)
##simulation_DFという名前でデータフレーム作っとく。
simulation_DF <- cbind(ids, alphas, rhos, trials) %>% data.frame(.)
##ついでに、simulation_DFにQ値と、刺激の選択確率(P_A)と、行動価値(Q_A, Q_B)、予測誤差(delta)と、選択行動の履歴(choice,Aを選んだ場合1、Bを選んだ場合2を符号化)と、選択刺激から得られたフィードバック(feedback,報酬時には1を罰時には-1を符号化)と、行動選択のログを付け足しておく。
##変数の意味(変数名,変数の符号化の補足説明)
simulation_DF <- simulation_DF %>%
mutate(.,
P_A = rep(
0,
times = alpha_length  * rho_length * trial
),
Q_A = rep(
0,
times = alpha_length * rho_length * trial
),
Q_B = rep(
0,
times = alpha_length * rho_length * trial
),
delta = rep(
0,
times = alpha_length * rho_length * trial
),
choice = rep(
0,
times = alpha_length * rho_length * trial
),
feedback = rep(
0,
times = alpha_length * rho_length * trial
),
)
##エージェントごとに分けていく。
for(n in id) {
id_simulation_DF <- simulation_DF %>% filter(., ids == n)
###試行ごとにfor文
for(t in 1:trial) {
id_simulation_DF$P_A[t] <-
1/
(
1 + exp(
-1 *(id_simulation_DF$Q_A[t] - id_simulation_DF$Q_B[t])
)
)
####提示された刺激の行動価値に応じた行動選択の確率を算出して、
####その確率に応じた行動選択をしていく。
if( runif(1,0,1) < id_simulation_DF$P_A[t] ) {
##runif(◯, △, □)は、[△ ~ □]範囲の乱数を◯個生成する。
id_simulation_DF$choice[t] <- 1
id_simulation_DF$feedback[t] <- runif(1,0,1) < A_probability %>%
as.numeric(.)
}
else{
id_simulation_DF$choice[t] <- 2
id_simulation_DF$feedback[t] <- runif(1,0,1) < B_probability %>%
as.numeric(.)
}
###行動価値の更新
if(t < trial){
####Aを選択した場合
if(id_simulation_DF$choice[t] == 1){
id_simulation_DF$delta[t] <- id_simulation_DF$feedback[t] *
rho[n] - id_simulation_DF$Q_A[t]
id_simulation_DF$Q_A[t+1] <- id_simulation_DF$Q_A[t] +
alpha[n] * id_simulation_DF$delta[t]
id_simulation_DF$Q_B[t+1] <- id_simulation_DF$Q_B[t]
}
#####Bを選択した場合
else{
id_simulation_DF$delta[t] <- id_simulation_DF$feedback[t] *
rho[n] - id_simulation_DF$Q_B[t]
id_simulation_DF$Q_B[t+1] <- id_simulation_DF$Q_B[t] +
alpha[n] * id_simulation_DF$delta[t]
id_simulation_DF$Q_A[t+1] <- id_simulation_DF$Q_A[t]
}
}
simulation_DF[( trial * (n-1) + 1 ) :( trial * n ), ] <- id_simulation_DF
}
}
all_rhos_color_point <- simulation_DF %>%
ggplot(
aes(
x = trials,
y = delta,
shape = factor(alphas),
color = factor(rhos)
)
) +
geom_point()
print(all_rhos_color_point)
all_alphas_color_point <- simulation_DF %>%
ggplot(
aes(
x = trials,
y = delta,
shape = factor(rhos),
color = factor(alphas)
)
) +
geom_point()
print(all_alphas_color_point)
##mean
with(simulation_DF, tapply(delta, list(rhos, alphas), mean))
only_gain_trial_simulation_DF <- simulation_DF %>%
filter(., feedback == 1)
only_gain_rhos_color_point <- only_gain_trial_simulation_DF %>%
ggplot(
aes(
x = trials,
y = delta,
shape = factor(alphas),
color = factor(rhos)
)
) +
geom_point()
print(only_gain_rhos_color_point)
only_gain_alphas_color_point <- only_gain_trial_simulation_DF %>%
ggplot(
aes(
x = trials,
y = delta,
shape = factor(rhos),
color = factor(alphas)
)
) +
geom_point()
print(only_gain_alphas_color_point)
##mean
with(only_gain_trial_simulation_DF, tapply(delta, list(rhos, alphas), mean))
rho_input <- c(1, 5, 9)
alpha_input <- c(0.1, 0.5, 0.9)
