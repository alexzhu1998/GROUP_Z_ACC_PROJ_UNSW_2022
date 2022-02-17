library(readxl)
library(here)

here::set_here()
GDP_per_cap <- read_excel("data/economic.xlsx",sheet = "Rarita Economic",range= "B12:F22")
GNI_per_cap <- read_excel("data/economic.xlsx",sheet = "Rarita Economic",range= "B12:F22")
