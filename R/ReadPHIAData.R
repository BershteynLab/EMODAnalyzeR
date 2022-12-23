path = "C:/Users/kaftad01/Documents/NAMPHIA 2017 Household Interview and Biomarker Datasets v1.1 (DTA)/namphia2017adultbio.dta"
read.phia.biomarker <- function (path) {

  data = read_dta(path)
  data %>% mutate(hivstatusfinal=as.factor(hivstatusfinal))
}

