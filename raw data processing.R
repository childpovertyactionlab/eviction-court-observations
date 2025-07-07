library(tidyverse)
library(sf)
library(googlesheets4)

# moving all of the data processing here

libDB <- '/Users/anushachowdhury/CPAL Dropbox/'

#dallas county evictions dataset

# write.csv(evictions, 'data/Evictions Court Obs.csv')

courtobs <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1O9P06SgvZ_nRGc64W2OsSjyX9iCS_LkcsG9087HNTuk",
                       sheet = "Form Responses 1") %>%
  janitor::clean_names(.) %>%
  rename("timeStamp"="timestamp",
         "observer"="observer",
         "observationDate"="date_of_observation",
         "jpCourt"="justice_of_the_peace_court",
         "docketStart"="docket_start_time_please_indicate_what_time_the_judge_called_the_first_case_on_the_docket_use_format_hh_mm_and_specify_am_or_pm_e_g_09_32_am",
         "docketEnd"="docket_end_time_please_indicate_the_time_the_judge_finished_hearing_the_last_case_on_the_docket_use_format_hh_mm_and_specify_am_or_pm_e_g_12_34_pm",
         "caseNumber"="case_number",
         "caseType"="is_this_a_commercial_or_residential_eviction_case",
         "plaintiffName"="plaintiff_landlord_name",
         "plaintiffDesignee"="did_the_plaintiff_landlord_or_their_designee_like_a_property_manager_appear",
         "plaintiffRep"="did_the_plaintiff_landlord_have_additional_representation_of_some_kind",
         "defName"="defendant_tenant_name",
         "defAppear"="did_the_defendant_tenant_appear",
         "defRep"="did_the_defendant_tenant_have_additional_representation_of_some_kind",
         "evicReason"="reason_for_eviction_select_multiple_if_applicable",
         "totalOwned"="total_amount_owed_please_indicate_the_total_amount_that_the_judge_or_landlord_states_is_owed_include_the_sum_of_rent_or_damages_owed_plus_court_costs_use_format_xxxx_xx_for_example_1234_50",
         "noticeVacate"="was_there_any_discussion_about_the_notice_to_vacate_ntv",
         "noticeConfirm"="if_yes_did_the_judge_confirm_the_ntv",
         "jpRuled"="the_jp_ruled_in_favor_of",
         "outcomeOther"="if_you_selected_other_please_specify_the_outcome_of_the_case_below",
         "otherNotes"="additional_case_notes"
  ) %>%
  filter(!jpCourt %in% c("5-2", "3-2", "2-1", '2-2'),
         !is.na(jpCourt),
         observationDate <= as.Date("2024-04-26")
  ) %>%
  mutate(jpCourt = paste("Court", jpCourt),
         observationDate = as.Date(observationDate, format = "%Y-%m-%d"))


# deac processing + .csvs for Ashley
deac <- read.csv('data/CPAL_DEAC_Court_Obs_Clean_010325 (2).csv') %>%
  rename(caseNumber = case_number,
         observationDate = appearance_date,
         plaintiffName = plaintiff,
         plaintiffDesignee = plaintiff_present,
         plaintiffRep = plaintiff_rep,
         defName = defendant,
         defAppear = defendant_present,
         defRep = defendant_rep,
         evicReason = reason,
         noticeVacate = ntv,
         jpRuled = outcome,
         jpCourt = court,
         docketStart = docket_start_time,
         docketEnd = docket_end_time,
         caseType = case_type,
         noticeConfirm = ntv_confirm,
         outcomeOther = outcome_other,
         otherNotes = notes)%>%
  filter(!jpCourt %in% c("5-2", "4-2"),
         !is.na(jpCourt),
         observationDate >= as.Date("2024-09-26") & observationDate <= as.Date("2025-01-18")
         ) %>%
  mutate(jpCourt = paste("Court", jpCourt),
         observationDate = as.Date(observationDate, format = "%Y-%m-%d"),
         observationDate = if_else(
           format(observationDate, "%Y-%m") == "2025-12",
           as.Date(paste0("2024-12-", format(observationDate, "%d"))),
           observationDate)
  ) %>%
  filter(jpCourt != 'Court ')

# evicNonDuplicates <- deac %>%
#   filter(!caseNumber %in% evictions$case_number)
# write.csv(evicNonDuplicates, "data/Evic Non Duplicates.csv")



duplicateCases <- deac %>%
  group_by(caseNumber) %>%
  summarise(n = n())

# casesByCourtDeac <- deac %>%
#   group_by(jpCourt) %>%
#   summarise(n = n()) %>%
#   mutate(per = round(n/sum(n)*100, 2))%>%
#   janitor::adorn_totals()

# deacFuture <- deac %>%
#   filter(observationDate > "2025-01-31")
# write.csv(deacFuture, "data/Cases after January 2025.csv")

# deac <- deac %>% anti_join(courtobs)
# deac <- deac %>% anti_join(courtobs, by = "caseNumber")
duplicates <- deac %>%
  filter(caseNumber %in% courtobs$caseNumber) #%>%
deac <- deac %>%
  filter(caseNumber != "Unknown")
write.csv(deac, 'data/DEAC Processed.csv')

# group_by(jpCourt) %>%
# summarise(n = n())%>%
# mutate(percent = (n / sum(n)) * 100) %>%
# janitor::adorn_totals()
# write.csv(duplicates, "data/DEAC and Court Obs Overlap.csv")

# evicReasonAppear <- deac%>%
#   filter(!is.na(jpCourt),
#          defAppear == "Yes") %>%
#   mutate(evicClean = ifelse(str_detect(evicReason, ","), "Multiple Reasons", evicReason),
#          evicClean = case_when(
#            is.na(evicClean) ~ "Unknown",
#            evicClean == "Holdover" ~ "Holdover/lease expiration",
#            evicClean == "Lease expired" ~ "Holdover/lease expiration",
#            evicClean == "Other" ~ "Unknown",
#            evicClean == "LV" ~ "Violated lease",
#            TRUE ~ evicClean
#            )) %>%
#   group_by(#jpCourt,
#            evicClean, #defAppear
#            observer
#            ) %>%
#   summarize(count = n()) %>%
#   mutate(perCases = count/sum(count))#%>%
# ungroup(.) %>%
# rbind(., totReasonDeac)
# write.csv(evicReasonAppear, "data/Eviction Reason Observer Breakdown.csv")

# write.csv(defRepresentDeac, "data/Defendant Representation Breakdown.csv")
# write.csv(deac %>% group_by(defAppear) %>% summarise(n = n()), 'data/Defendant Appearance Breakdown.csv')
# defRepresentDeacCsv <- deac %>%
#   filter(!is.na(jpCourt),
#          defAppear == "Yes",
#          observationDate >= as.Date("2024-09-26") & observationDate < as.Date("2025-01-18")
#          ) %>%
#   mutate(legalRep = ifelse(defRep == "Yes, an attorney was present for the defendant.", "Attorney present for tenant (Defendant)", "No attorney present for tenant")) %>%
#   group_by(#jpCourt,
#            legalRep, observer) %>%
#   summarize(count = n()) %>%
#   mutate(perRep = count/sum(count))
# mutate(perRep = count/sum(count)) %>%
# ungroup(.) %>%
# group_by(legalRep) %>%
# group_map(~ .x %>%
#         janitor::adorn_totals(),.keep = T) %>%
# bind_rows() %>%
# mutate(test = lag(legalRep),
#        legalRep = ifelse(str_detect(legalRep, "tenant"), legalRep, test)) %>%
# select(-test) %>%
# group_by(jpCourt) %>%
# mutate(perRep = count/sum(count)) %>%
# ungroup()
# write.csv(defRepresentDeacCsv, "data/Defendant Representation Observer Breakdown.csv")

# hearingtimeDeacObserver <- deac %>%
#   filter(observationDate >= as.Date("2024-09-26") & observationDate <= as.Date("2025-01-18"))%>%
#   mutate(dockLength = docketEnd-docketStart) %>%
#   filter(!is.na(dockLength))%>%
#   group_by(jpCourt, observationDate, observer) %>%
#   summarize(CasesObs = n(),
#             AvgDockLength = mean(dockLength, na.rm=TRUE)) %>%
#   ungroup() %>%
#   group_by(jpCourt, observer)%>%
#   summarize(CasesObs = sum(CasesObs),
#             SumDockLength = as.numeric(sum(AvgDockLength))) %>%
#   ungroup(.) %>%
#   janitor::adorn_totals() %>%
#   mutate(AvgCaseLength = SumDockLength/CasesObs) %>%
#   select(-SumDockLength)
# write.csv(hearingtimeDeacObserver, 'data/hearingtimeDeacObserver.csv')

# write.csv(outcomeRepDeac, 'data/Outcome Representation Graph Data.csv')
outcomeRepCheck <- deac %>%
  filter(!is.na(jpCourt),
         observationDate >= as.Date("2024-09-26") & observationDate <= as.Date("2025-01-18"),
         # defRep == "Yes, an attorney was present for the defendant." | defRep == "No, the defendant did not have additional representation."
  )  %>%
  group_by(defRep#, jpRuled
  ) %>%
  summarise(n = n())
write.csv(outcomeRepCheck, "data/Legal Rep Breakdown.csv")

jpRuledSummary<- deac %>% group_by(jpRuled) %>% summarise(n = n()) %>%
  mutate(per = n/sum(n)) %>%
  janitor::adorn_totals()

