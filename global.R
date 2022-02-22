library(data.table);library(magrittr);library(haven);library(labelled);library(jstable)

setwd("/home/heejooko/ShinyApps/HYUPSYML")
#setwd("/home/js/ShinyApps/cogito-ergo-sum/REAP_AP4-ML")

# a <- haven::read_sav("reap_ap4_20220220.sav")
# saveRDS(a, "reap_ap4_20220220.RDS")

a <- data.table(readRDS("reap_ap4_20220220.RDS"))

for (v in names(a)[sapply(a, is.labelled)]){
  a[[v]] <- to_factor(a[[v]])
}

varlist <- list(
  "Outcome" = c("d_clozapine_2", "d_depot", "ect_in_current", "high_dose_antipsychotic_1000", "d_antidepressant"),
  "Variables" = c("region_code_area", "income_code_area", "inpatient", "age", "sex", "bmi", "duration_from_onset",
                  "remission", "continuing_presence_of_symptoms", "delusions", "hallucinations", "disorganized_speech",
                  "grossly_disorganized_or_catatonic_behavior", "negtative_symptoms", "social_or_occupational_dysfunctions",
                  "verbal_aggression", "physical_aggression", "significant_affective_symptoms")
)


out <- a[, .SD, .SDcols = unlist(varlist)] %>% na.omit

vars.factor <- c(varlist$Outcome, setdiff(varlist$Variables, c("age", "bmi")))
out[, (vars.factor) := lapply(.SD, factor), .SDcols = vars.factor]
levels(out$d_clozapine_2)<-c("no","yes")
levels(out$d_depot)<-c("no","yes")
levels(out$d_antidepressant)<-c("no","yes")

out.label <- jstable::mk.lev(out)


#remotes::install_github("cran/DMwR")
#library(DMwR)
#dd <- DMwR::SMOTE(high_dose_antipsychotic_1000 ~ ., data = out)
