x


imp1 <- imp1 |> 
  mutate(Framingham = ifelse(glucose >= 5.55, 10, 0)) |> 
  mutate(Framingham = Framingham + ifelse(BMI >= 25 & BMI <30, 2, 0)) |> 
  mutate(Framingham = Framingham + ifelse(BMI >= 30, 5, 0)) |> 
  mutate(Framingham = Framingham + ifelse(gender == 'male' & HDL < 1.036, 5, 0)) |> 
  mutate(Framingham = Framingham + ifelse(gender == 'female' & HDL < 1.295, 5, 0)) |> 
  mutate(Framingham = Framingham + ifelse(famhist_T2D == 'family diabetes', 3, 0)) |> 
  mutate(Framingham = Framingham + ifelse(TG >= 1.695, 3, 0 )) |> 
  mutate(Framingham = Framingham + ifelse(SBP >= 130 | DBP >= 85 | now_BP_meds == 'BP meds', 2, 0)) |> 
  mutate(Risk_Framingham = case_when(Framingham <= 10 ~ 3,
                                     Framingham == 11 ~ 4,
                                     Framingham == 12 ~ 4,
                                     Framingham == 13 ~ 5,
                                     Framingham == 14 ~ 6,
                                     Framingham == 15 ~ 7,
                                     Framingham == 16 ~ 9,
                                     Framingham == 17 ~ 11,
                                     Framingham == 18 ~ 13,
                                     Framingham == 19 ~ 15,
                                     Framingham == 20 ~ 18,
                                     Framingham == 21 ~ 21,
                                     Framingham == 22 ~ 25,
                                     Framingham == 23 ~ 29,
                                     Framingham == 24 ~ 33,
                                     Framingham >= 25 ~ 35)) |> 
  mutate(Risk_Framingham = Risk_Framingham * 0.01) |>
  select(-Framingham)
