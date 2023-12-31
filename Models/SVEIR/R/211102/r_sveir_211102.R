# raw file name: Developing-SVEIR-model-in-R.html


# LOADING LIBRARIES
## Data wrangling,
library(tidyverse)
library(lubridate)
library(vroom)
library(rlist) # for list to tibble combined by rows
library(deSolve)
library(Cairo)
library(scales) # for the comma formatting
ggplot2::theme_set(theme_classic())



# LOADING AND WRANGLING DATA
path <- vroom("../../../../00_path/SVEIR/sveir_path.csv")
input_path <- path$input

output_path <- path$output

case <- vroom(input_path)
case2 <- case %>% 
    distinct() %>%
    arrange(stateDt) %>% 
    select(stateDt, decideCnt, clearCnt, deathCnt) %>% 
    mutate(stateDt = ymd(stateDt)) %>%
    filter(stateDt != ymd("20200101")) %>%
    filter(stateDt >= ymd("20200206")) %>% # 2020/02/06 
    mutate(daily_confirmed = c(5, diff(decideCnt))) %>%  # diff()??? 
    rename(confirmed = decideCnt) %>%
    mutate( 
        daily_MA = slider::slide_dbl(daily_confirmed, mean, .before = 3, .after = 3, .complete = TRUE),
        daily_MA_clearCnt = slider::slide_dbl(clearCnt, mean, .before = 3, .after = 3, .complete = TRUE),
        daily_MA_deathCnt = slider::slide_dbl(deathCnt, mean, .before = 3, .after = 3, .complete = TRUE)
    ) %>% 
    filter(!is.na(daily_MA))



# MAKE A SVIR MODEL FUNCTION
SVEIR <- function(time, state, parameters){
    with(as.list(c(state, parameters)),{
        N = S+I+R
        lambda = beta*(I/N)
        dS = -lambda*S - nu*300000*alpha
        dV = nu*300000*alpha 
        dE = lambda*S - sigma*E
        dI = sigma*E - gamma*I
        dR = gamma*I
        
        return(list(c(dS, dV, dE, dI, dR)))
    }
    )
}


N <- 51822000
v <- 15340827*0.7
i <- case2 %>% pull(daily_MA) %>% tail(1)
e <- i
r <- case2 %>% pull(daily_MA_clearCnt) %>% tail(1) + case2 %>% pull(daily_MA_deathCnt) %>% tail(1)
init <- c(S = N-i-e-r-v, V = v, E = e, I = i, R = r)


r0_1.2 <- c(gamma = 1/30, beta = 1.2/30, sigma = 1/14, nu = 1/14, alpha = 0.7)
r0_1.1 <- c(gamma = 1/30, beta = 1.1/30, sigma = 1/14, nu = 1/14, alpha = 0.7)
r0_1.0 <- c(gamma = 1/30, beta = 1.0/30, sigma = 1/14, nu = 1/14, alpha = 0.7)
r0_0.9 <- c(gamma = 1/30, beta = 0.9/30, sigma = 1/14, nu = 1/14, alpha = 0.7)
r0_0.8 <- c(gamma = 1/30, beta = 0.8/30, sigma = 1/14, nu = 1/14, alpha = 0.7)
r0 <- list(r0_1.2, r0_1.1, r0_1.0, r0_0.9, r0_0.8)


names(r0) <- str_c("R0_", seq(1.2, 0.8, -0.1))
SVEIR_korea <- vector(mode = "list", length = length(r0))
names(SVEIR_korea) <- names(r0)

PREDICT_LENGTH <- 120 
for(i in seq_along(r0)){
  SVEIR_korea[[i]] <- ode(y = init, func = SVEIR, parms = r0[[i]], time = 1:PREDICT_LENGTH) %>%
    as_tibble %>%
    pivot_longer(S:R, names_to = "State") %>%
    mutate(value = as.numeric(value),
           time = as.numeric(time)) %>%
    arrange(State) %>%
    mutate(
      Date = rep(seq(ymd("2021-10-06"), by = "day", length.out = PREDICT_LENGTH), 5) #06-28
    ) %>%
    filter(time != 1)
}


SVEIR_k <-
  bind_rows(SVEIR_korea[[1]], SVEIR_korea[[2]], SVEIR_korea[[3]],
            SVEIR_korea[[4]], SVEIR_korea[[5]]) %>%
  mutate(R = factor(rep(seq(1.2, 0.8, by = -0.1), each =( PREDICT_LENGTH-1)*5))) %>%
  filter(State == "I")


SVEIR_k %>% write.csv(output_path)
