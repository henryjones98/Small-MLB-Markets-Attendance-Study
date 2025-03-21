# Small-MLB-Markets-Attendance-Study
### 3D Scatterplot- City Population and Win % Impact on Team Attendance ###
NOT_NY_LA <- MLB_Attendance_Study %>% filter(Total_Pro_Sports_Teams <= 7)
library(plotly)
library(ggplot2)
Pop_WinPct_Plot <- plot_ly(NOT_NY_LA, x = ~City_Population, y = ~Win_PCT, z = ~Total_Attendance,
                                   main= "Poplation/Win % impact on MLB Attendance",
                                   xaxis = list(title = "City Population"),
                                   yaxis = list(title = "Win %"),
                                   zaxis = list(title = "Total Attendance"),
                                   marker = list(color = ~Total_Attendance, colorscale = c("blue", "white", "red"), showscale = TRUE))
Pop_WinPct_Plot

### Regression 1 ###
library(ggplot2)
Total_Attendance_Model <-
  lm(Total_Attendance ~ City_Population + Total_Pro_Sports_Teams + Major_Pro_Sports_Teams + Two_MLB_Teams + Win_PCT + Made_Playoffs_Previous_Year + Made_WS_Previous_Year + Won_WS_Previous_Year, data = MLB_Attendance_Study)
summary(Total_Attendance_Model)

### Regression 2- Small vs. Big Markets ###
library(tidyverse)
library(huxtable)
Less_Teams <- MLB_Attendance_Study %>% filter(Total_Pro_Sports_Teams <= 3)
Less_Teams_Not_SD <- Less_Teams %>% filter(!Team == 'Padres')
Less_Teams_Not_SD_Model <- lm(Total_Attendance ~ City_Population + Total_Pro_Sports_Teams + Major_Pro_Sports_Teams + Win_PCT + Made_Playoffs_Previous_Year + Total_WS_Titles + Total_WS_Appearances, data = Less_Teams_Not_SD)
summary(Less_Teams_Not_SD_Model)
Many_Teams <- MLB_Attendance_Study %>% filter(Total_Pro_Sports_Teams >= 4)
Many_Teams_Model <- lm(Total_Attendance ~ City_Population + Total_Pro_Sports_Teams + Major_Pro_Sports_Teams + Win_PCT + Made_Playoffs_Previous_Year + Total_WS_Titles + Total_WS_Appearances, data = Many_Teams)
summary(Many_Teams_Model)
huxreg("Small Markets" = Less_Teams_Not_SD_Model, "Big Markets" = Many_Teams_Model)
