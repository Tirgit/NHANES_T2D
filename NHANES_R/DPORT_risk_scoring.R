
############################
##### DPoRT RISK SCORE #####
############################
### Predict 9-y risk ###
imp1 <- imp1 |> 
  mutate(DPoRT.M = 10.5971 + ifelse(hypertension_now == "hypertension", -0.2624, 0)) |> 
  mutate(DPoRT.M = DPoRT.M + ifelse(ethnicity == "white",0 , -0.6316)) |> 
  mutate(DPoRT.M = DPoRT.M + ifelse(heart_disease == "heart disease",-0.5355 , 0)) |> 
  mutate(DPoRT.M = DPoRT.M + ifelse(current_smoker == "smoker",-0.1765 , 0)) |> 
  mutate(DPoRT.M = DPoRT.M + ifelse(education %in% c("GED","some college","college"),0.2344 , 0)) |>
  mutate(DPoRT.M = DPoRT.M + ifelse(BMI<23 & age<45,0,
                                    ifelse(BMI<25 & age<45,-1.2378,
                                           ifelse(BMI<30 & age<45,-1.5490,
                                                  ifelse(BMI<35 & age<45,-2.5437,
                                                         ifelse(BMI>=35 & age<45,-3.4717,
                                                                ifelse(BMI<23 & age>=45,-1.9794,
                                                                       ifelse(BMI<25 & age>=45,-2.4426,
                                                                              ifelse(BMI<30 & age>=45,-2.8488,
                                                                                     ifelse(BMI<35 & age>=45,-3.3179,-3.5857))))))))) ) |>
  mutate(DPoRT.M = (log(365.25*9)-DPoRT.M)/0.8049 ) |> 
  mutate(DPoRT.M = 1-exp(-exp(DPoRT.M)) )

imp1 <- imp1 |> 
  mutate(DPoRT.F = 10.5474 + ifelse(hypertension_now == "hypertension",0 ,-0.2865)) |> 
  mutate(DPoRT.F = DPoRT.F + ifelse(ethnicity == "white",-0.4309,0)) |> 
  mutate(DPoRT.F = DPoRT.F + ifelse(born_USA == "immigrant",0,-0.2930)) |> 
  mutate(DPoRT.F = DPoRT.F + ifelse(education %in% c("GED","some college","college"),0.2042 , 0)) |>
  mutate(DPoRT.F = DPoRT.F + ifelse(BMI<23 & age<45,0,
                                    ifelse(BMI<25 & age<45,-0.5432,
                                           ifelse(BMI<30 & age<45,-0.8453,
                                                  ifelse(BMI<35 & age<45,-1.4104,
                                                         ifelse(BMI>=35 & age<45,-2.0483,
                                                                ifelse(is.na(BMI)==TRUE & age<45,-1.1328,
                                                                       
                                                                       ifelse(BMI<23 & age<65,0.0711,
                                                                              ifelse(BMI<25 & age<65,-0.7011,
                                                                                     ifelse(BMI<30 & age<65,-1.4167,
                                                                                            ifelse(BMI<35 & age<65,-2.2150,
                                                                                                   ifelse(BMI>=35 & age<65,-2.2695,
                                                                                                          ifelse(is.na(BMI)==TRUE & age<65,-1.7260,
                                                                                                                 
                                                                                                                 
                                                                                                                 ifelse(BMI<23 & age>=65,-1.0823,
                                                                                                                        ifelse(BMI<25 & age>=65,-1.1419,
                                                                                                                               ifelse(BMI<30 & age>=65,-1.5999,
                                                                                                                                      ifelse(BMI<35 & age>=65,-1.9254,
                                                                                                                                             ifelse(BMI>=35 & age>=65,-2.1959,-1.8284))))))))))))))))) ) |>
  mutate(DPoRT.F = (log(365.25*9)-DPoRT.F)/0.7814 ) |> 
  mutate(DPoRT.F = 1-exp(-exp(DPoRT.F)) )

imp1 <- imp1 |> 
  mutate(DPoRT = ifelse(gender == "male",DPoRT.M,DPoRT.F)) 

