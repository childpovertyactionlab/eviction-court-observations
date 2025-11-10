library(tidyverse)
library(googlesheets4)

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
  filter(observationDate <= as.Date("2024-04-26"),
         observationDate >= as.Date("2023-06-20")) %>%
  mutate(jpCourt = paste("Court", jpCourt),
         observationDate = as.Date(observationDate, format = "%Y-%m-%d"))


defAppearSum <- courtobs %>%
  group_by(`Did the defendant/tenant appear?`=defAppear) %>%
  summarise(`Number of Observations` = n()) %>%
  mutate(Percent = round(`Number of Observations`/sum(`Number of Observations`)*100, 2)) %>%
  janitor::adorn_totals()
writexl::write_xlsx(defAppearSum,'data/2024 Tenant Appearances.xlsx')

defAppear24 <- courtobs %>%
  filter(!is.na(jpCourt),
         !jpCourt %in% c("5-2", "3-2", "2-1", '2-2'),
         defAppear != "Unknown",
         observationDate >= as.Date("2023-06-20") & observationDate < as.Date("2024-04-26")) %>%
  group_by(jpCourt, defAppear) %>%
  summarize(count = n()) %>%
  filter(count > 1) %>%
  ungroup(.) %>%
  pivot_wider(names_from = defAppear, values_from = count, values_fill = 0) %>%
  janitor::adorn_totals() %>%
  mutate(AppearPer = round(Yes/(Yes+No), 2)*100,
         type = "Tenant (Defendant) Appeared for Hearing",)
writexl::write_xlsx(defAppear24, 'data/2024 Tenant Appearances (archived).xlsx')
