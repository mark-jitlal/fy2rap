devtools::install_github('mark-jitlal/fy2rap')
# Installs package

library(fy2rap)
# Loads package

?fy2
# Assessment of fy2

fy2
# Calls fy2 dataset

df <- fy2_data(fy2)
df <- fy2rap::fy2_data(fy2)
# Doesn't seem to have loaded the QA function correctly.......
df <- fy2
