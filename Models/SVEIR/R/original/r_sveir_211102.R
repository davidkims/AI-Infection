# raw file name: Developing-SVEIR-model-in-R.html


# LOADING LIBRARIES
## Data wrangling, 紐⑦삎 援ъ텞 諛? ?떆媛곹솕?뿉 ?븘?슂?븳 R ?뙣?궎吏瑜? 遺덈윭?삤?뒗 怨쇱젙?씠誘濡?, 臾댁떆?븯?뀛?룄 ?맗?땲?떎.
library(tidyverse)
library(lubridate)
library(vroom)
library(rlist) # for list to tibble combined by rows
library(deSolve)
library(Cairo)
library(scales) # for the comma formatting
ggplot2::theme_set(theme_classic())



# LOADING AND WRANGLING DATA
## "肄붾줈?굹19 媛먯뿼?쁽?솴_~20210701.csv"?옄猷뚮뒗 OPEN API濡? 遺?꽣 ?솗蹂댄븳 ?옄猷뚯뿉 ?빐?떦?빀?땲?떎.-> open api瑜? ?넻?빐?꽌 留ㅼ씪 ?뾽?뜲?씠?듃 媛?뒫?빀?땲?떎!!!
## ?떎?쓬?? ?솗蹂댄븳 ?옄猷뚮줈遺?꽣 ?씪 ?솗吏꾩옄 ?닔 怨꾩궛 諛? 7-MA(7?씪 ?씠?룞?룊洹?)?쓣 怨꾩궛?븯?뒗 怨쇱젙?엯?땲?떎. 
## 理쒖쥌?쟻?쑝濡? 紐⑤뜽留? 諛? ?떆媛곹솕?뿉 ?벐?씪 ?옄猷뚮뒗 case2?뿉 ?빐?떦?빀?땲?떎.

case <- vroom("../../01_data/svier.csv")
case2 <- case %>% 
    distinct() %>% # 以묐났?뻾 ?젣嫄?
    arrange(stateDt) %>% # 諛쒗몴?씪 湲곗? ?삤由꾩감?닚 ?젙?젹
    select(stateDt, decideCnt, clearCnt, deathCnt) %>% 
    mutate(stateDt = ymd(stateDt)) %>%
    filter(stateDt != ymd("20200101")) %>%
    filter(stateDt >= ymd("20200206")) %>% # 2020/02/06 ?씠?쟾 ?옄猷뚮뒗 遺덉븞?젙?븯?뿬 ?젣嫄?
    mutate(daily_confirmed = c(5, diff(decideCnt))) %>%  # diff()瑜? ?넻?븳 ?씪蹂? ?솗吏꾩옄 ?닔 怨꾩궛
    rename(confirmed = decideCnt) %>%
    mutate( # 7?씪 ?씠?룞?룊洹? 異붽?.
        daily_MA = slider::slide_dbl(daily_confirmed, mean, .before = 3, .after = 3, .complete = TRUE),
        daily_MA_clearCnt = slider::slide_dbl(clearCnt, mean, .before = 3, .after = 3, .complete = TRUE),
        daily_MA_deathCnt = slider::slide_dbl(deathCnt, mean, .before = 3, .after = 3, .complete = TRUE)
    ) %>% 
    filter(!is.na(daily_MA))

# ?옄猷뚯뿉?꽌 紐⑦삎 援ъ텞?뿉 ?벐?씤 媛꾨왂?븳 蹂?닔 ?꽕紐낆? ?떎?쓬怨? 媛숈뒿?땲?떎:

## stateDt: 諛쒗몴 湲곗??씪 (?썝?옄猷?)
## decideCnt: ?늻?쟻 ?솗吏꾩옄 ?닔 (?썝?옄猷?)
## deathCnt: ?늻?쟻 ?궗留앹옄 ?닔 (?썝?옄猷?)
## clearCnt: ?늻?쟻 寃⑸━?빐?젣 ?닔 (?썝?옄猷?)
## daily_confirmed: ?씪 ?솗吏꾩옄 ?닔 (decideCnt濡? 怨꾩궛, 異붽? ?젙蹂?:2020/02/06 ?솗吏꾩옄 ?닔 5紐?)
## daily_MA: ?씪 ?솗吏꾩옄 ?닔?쓽 7-MA
## daily_MA_clearCnt: ?늻?쟻 寃⑸━?빐?젣 ?닔?쓽 7-MA
## daily_MA_deathCnt: ?늻?쟻 ?궗留앹옄 ?닔?쓽 7-MA

# 洹몃━怨?, ?쐞 肄붾뱶釉붾윮?뿉?꽌 留덉?留됱뿉 ?닔?뻾?븳 filter(!is.na(daily_MA))?쓽 寃쎌슦, 
# 7-MA瑜? ?떎?떆?븯硫? ?옄猷뚯쓽 ?븵?뮘 3?씪?뵫 寃곗륫?씠 諛쒖깮?븯?뒗?뜲 ?씠 遺遺꾩쓣 ?젣嫄고븳 寃껋씠?씪怨? 蹂댁떆硫대맗?땲?떎. 
# 利?, 2020/02/06~2021/07/01 ?옄猷뚯뿉?꽌 ?븵?뮘濡? 3?뵫?쓣 ?젣嫄고븳 寃껋엯?땲?떎.


# MAKE A SVIR MODEL FUNCTION
## ?씪 諛깆떊 1李⑥젒醫? ?솚?옄 ?닔?뒗 30留뚮챸?쑝濡? 媛?젙?븯???뒿?땲?떎:
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

# MODEL INPUTS
# ?떎?쓬?? 紐⑦삎?쓽 ?삁痢≪뿉 ?븘?슂?븳 珥덇퉫媛? ?꽕?젙 怨쇱젙?엯?땲?떎.
## N: 2021?뀈 以묒쐞?씤援? 異붽퀎(李멸퀬)
## V(0): 7?썡 1?씪源뚯? 1李? ?젒醫? ?솚?옄 ?닔*0.8(李멸퀬: 留ㅼ씪 ?뾽?뜲?씠?듃 ?맖)
## I(0): ?씪 ?솗吏꾩옄 ?닔(daily_confirmed)?쓽 7-MA?쓽 留덉?留? ?궇吏? 媛믪쓣 珥덇린 媛먯뿼援? ?닔濡? ?궗?슜
## E(0): I(0)濡? 媛?젙
## R(0): 寃⑸━?빐?젣(clearCnt) 諛? ?궗留앹옄 ?닔(deathCnt) 媛곴컖?쓽 7-MA 怨꾩궛 ?썑, 媛? 蹂?닔?쓽 留덉?留? ?궇吏? 媛믪쓽 ?빀?쓣 珥덇린 ?쉶蹂듦뎔 ?닔濡? ?궗?슜
## S(0): N?닋V(0)?닋E(0)?닋I(0)?닋R(0)

N <- 51822000
v <- 15340827*0.7
i <- case2 %>% pull(daily_MA) %>% tail(1)
e <- i
r <- case2 %>% pull(daily_MA_clearCnt) %>% tail(1) + case2 %>% pull(daily_MA_deathCnt) %>% tail(1)
init <- c(S = N-i-e-r-v, V = v, E = e, I = i, R = r)

# ?떎?쓬?? 紐⑦삎?쓽 紐⑥닔?뱾?뿉 愿?븳 ?꽕?젙?엯?땲?떎. 
## Rp(prespecified reproduction number)?뒗 泥⑤??븳 ?뿊?? ?뙆?씪?쓽 specification?뿉?꽌 0.8,0.9,1,1.1,1.2濡? ?븯???떎怨? ?뻽?뒗?뜲, 
## 棺?? 款媛믪뿉 ?쓽?빐 ?떎?쓬怨? 媛숈씠 寃곗젙?맗?땲?떎. 
## ?삁瑜? ?뱾?뼱, Rp=0.8?씤 寃쎌슦 款=1/30?씠誘濡? ?옄?룞?쑝濡? 媛먯뿼瑜? 棺=1.2/30?쑝濡? 寃곗젙?씠 ?맗?땲?떎. 
## ?씠?뿉 ?뵲瑜? 濡쒖쭅?쑝濡? ?떎?쓬怨? 媛숈씠 5媛쒖쓽 紐⑥닔 ?꽕?젙?쑝濡? ?떆?굹由ъ삤瑜? 援ъ꽦?븯???뒿?땲?떎.

r0_1.2 <- c(gamma = 1/30, beta = 1.2/30, sigma = 1/14, nu = 1/14, alpha = 0.7)
r0_1.1 <- c(gamma = 1/30, beta = 1.1/30, sigma = 1/14, nu = 1/14, alpha = 0.7)
r0_1.0 <- c(gamma = 1/30, beta = 1.0/30, sigma = 1/14, nu = 1/14, alpha = 0.7)
r0_0.9 <- c(gamma = 1/30, beta = 0.9/30, sigma = 1/14, nu = 1/14, alpha = 0.7)
r0_0.8 <- c(gamma = 1/30, beta = 0.8/30, sigma = 1/14, nu = 1/14, alpha = 0.7)
r0 <- list(r0_1.2, r0_1.1, r0_1.0, r0_0.9, r0_0.8)

# ?떎?쓬?? 蹂멸꺽?쟻?씤 紐⑦삎 ?삁痢? 怨쇱젙?엯?땲?떎. ?씠 遺遺꾨??꽣?뒗 紐щ뱶由ъ븞AI?뿉?꽌 ?궗?슜?븯?떆?뒗 ?뼵?뼱濡? ?옄?쑀濡?寃? 吏꾪뻾?븯?떆硫? ?맆 寃? 媛숈뒿?땲?떎. 
# ?쐵 遺遺꾩쓽 SVEIR 紐⑦삎?뿉 ???븳 ?븿?닔 留뚮뱶?뒗 怨쇱젙, 珥덇퉫媛? ?꽕?젙, 紐⑥닔 ?꽕?젙 ?젙?룄留? 李멸퀬?븯?떆硫? ?맆 寃? 媛숈뒿?땲?떎. 
# 李멸퀬濡?, ?떎?쓬 肄붾뱶 釉붾윮?뿉?꽌 ode()?븿?닔媛 R?쓽 {deSolve} ?뙣?궎吏?뿉 ?냽?빐?엳?뒗 ?긽誘몃텇 諛⑹젙?떇(ordinary differential equations, ODE)?쓣 ?닔?뻾?빐二쇰뒗 ?븿?닔?엯?땲?떎.

names(r0) <- str_c("R0_", seq(1.2, 0.8, -0.1))
SVEIR_korea <- vector(mode = "list", length = length(r0))
names(SVEIR_korea) <- names(r0)

PREDICT_LENGTH <- 120 # ?썝?옒 肄붾뱶?뿉?뒗 ?뾾?뒗 遺遺꾩엯?땲?떎! ?삁痢≫븯怨? ?떢?? ?궇吏쒕줈 諛붽씀硫? ?맗?땲?떎.


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
# ?삁吏꾨떂 ?닔?젙?븳 肄붾뱶..?븞?룎?븘媛?

# for(i in seq_along(r0)){
#   SVEIR_korea[[i]] <- ode(y = init, func = SVEIR, parms = r0[[i]], time = 1:PREDICT_LENGTH+1) %>%
#     as_tibble %>%
#     pivot_longer(S:R, names_to = "State") %>%
#     mutate(value = as.numeric(value),
#            time = as.numeric(time)) %>%
#     arrange(State) %>%
#     mutate(
#       Date = rep(seq(ymd("2021-07-12"), by = "day", length.out = PREDICT_LENGTH+1), 5)
#     ) %>%
#     filter(time != 1)
# }
# 
# SVEIR_k <-
#   bind_rows(SVEIR_korea[[1]], SVEIR_korea[[2]], SVEIR_korea[[3]],
#             SVEIR_korea[[4]], SVEIR_korea[[5]]) %>%
#     mutate(R = factor(rep(seq(1.2, 0.8, by = -0.1), each = PREDICT_LENGTH*5))) %>%
#     filter(State == "I")

SVEIR_k <-
  bind_rows(SVEIR_korea[[1]], SVEIR_korea[[2]], SVEIR_korea[[3]],
            SVEIR_korea[[4]], SVEIR_korea[[5]]) %>%
  mutate(R = factor(rep(seq(1.2, 0.8, by = -0.1), each =( PREDICT_LENGTH-1)*5))) %>%
  filter(State == "I")


SVEIR_k %>% write.csv("../../04_result/SVEIR/SVEIR_k.csv")



# VISUALIZATION
case2 %>%
  mutate(stateDt=case2$stateDt+3) %>%
  ggplot() +
  geom_line(aes(x = stateDt, y = daily_MA), col = "grey") +
  geom_line(data = SVEIR_k,
            aes(x = Date, y = value, col = R)) +
  scale_x_date(date_breaks = "1 months",
               date_labels = "%Y-%m") +
  scale_y_continuous(label = comma) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "Daily confirmed cases",
    col = "Prespecified reproduction number"
  )

# past_df <- case %>%
#   select(stateDt, daily_confirmed, deathCnt, examCnt) %>%
#   mutate(?뜲?씠?꽣援щ텇 = 'Reported') %>%
#   rename(?궇吏?=stateDt) %>%
#   rename(?궗留앹옄?닔=deathCnt) %>%
#   rename(諛쒖깮?옄?닔=daily_confirmed) %>%
#   rename(寃?궗?닔=examCnt) %>%
#   pivot_longer(cols=c(諛쒖깮?옄?닔, ?궗留앹옄?닔, 寃?궗?닔), names_to="variable", values_to="吏묎퀎") %>%
#   arrange(variable)
# past_df

# future_df <- SVEIR_k %>%
#   rename(?뜲?씠?꽣援щ텇=R) %>%
#   rename(吏묎퀎="value") %>%
#   rename(?궇吏?=Date) %>%
#   mutate(variable='諛쒖깮?옄?닔') %>%
#   select(?궇吏?, ?뜲?씠?꽣援щ텇, variable, 吏묎퀎)
# future_df

# new_data2 <- rbind(past_df, future_df) # column ?닚?꽌 二쇱쓽

# new_data2 <- new_data2 %>%
#   mutate(?삁痢〓え?삎='SVEIR') %>%
#   mutate(留덉뒪?겕?떆?뻾?뿬遺="O") %>%
#   mutate(?궗?쉶?쟻嫄곕━?몢湲?=0) %>%
#   mutate(諛깆떊?젒醫낅쪧=0) %>%
#   mutate(吏?뿭='援??궡') %>%
#   mutate(?룄?떆='?쟾泥?') %>%
#   mutate(?떆援곌뎄="?쟾泥?") %>%
#   mutate(援?媛="???븳誘쇨뎅")

# # new_data2 %>% write.csv("covid/project_1/data_2/20210713_SVEIR.csv")

# # new_data3 <- rbind(new_data, new_data2)
# # new_data3 %>% write.csv("covid/project_1/data_2/20210713_SVEIR_SEIR.csv")
# # new_data3
# #  new_data %>% tail(10)

# # case2 %>% tail(10)

# # new_data %>%  filter(!is.na(吏묎퀎))

# # future_df


# # SVEIR_k


# # SVEIR_kk <- bind_rows(SVEIR_korea[[1]], SVEIR_korea[[2]], SVEIR_korea[[3]],
# #                      SVEIR_korea[[4]], SVEIR_korea[[5]]) %>%
# #   mutate(R = factor(rep(seq(1.2, 0.8, by = -0.1), each = (predict_length-1)*5)))

