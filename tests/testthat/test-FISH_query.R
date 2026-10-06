
con<-FISHub_connect()


test_that("FISH_query rejects invalid queries", {
  expect_error(FISH_query(con, QueryType = "BadType"))
  expect_error(FISH_query(con, QueryType = "Survey",GearType="BadNet"))
  expect_error(FISH_query(con, QueryType = "Survey",SurveyPurpose="BadPurpose"))
})


test_that("FISH_query works with default parameters", {
  result <- withCallingHandlers(
    FISH_query(con = con),
    warning = function(w) {
      if (grepl(
        "DO NOT USE as part of S&T surveys|DO NOT USE THESE DATA UNTIL VERIFIED",
        conditionMessage(w)
      )) {
        invokeRestart("muffleWarning")
      }
    }
  )
  
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  expect_true(all(c(
    "MDNRID",
    "SurveyId",
    "FMU"
  ) %in% names(result)))
})

test_that("FISH_query is working, filters by SurveyID for each QueryType", {
  survey <- FISH_query(con, QueryType = "Survey",SurveyId = 1162)
  efforts <- FISH_query(con, QueryType = "Efforts",SurveyId = 1162)
  catch <- FISH_query(con, QueryType = "Catch",SurveyId = 1162)
  
  expect_true(all(c("SurveyId","WaterBodyName","MDNRID") %in% names(survey)))
  expect_true(is.data.frame(survey))
  expect_true(is.data.frame(efforts))
  expect_true(is.data.frame(catch))
  
  expect_true(all(c(
    "SurveyId",
    "SurveyEffortKey",
    "Species",
    "TotalNumberCaught",
    "LengthAverage",
    "LengthMinimum",
    "LengthMaximum",
    "CatchTable"
  ) %in% names(catch)))
})


test_that("FISH_query applies individual filters", {
  expect_warning(result <- FISH_query(con,QueryType = "Survey",Year = 2024),"DO NOT USE") #expected until Catch_discrepancies addressed
  expect_gt(nrow(result), 0)
  expect_true(all(result$Year == 2024))
  
  expect_warning(result <- FISH_query(con,QueryType = "Survey",FMU = "CLM"),"DO NOT USE THESE DATA UNTIL VERIFIED") #expected until Catch_discrepancies addressed
  expect_gt(nrow(result), 0)
  expect_true(all(result$FMU == "CLM"))
  
  expect_warning(result <- FISH_query(con,QueryType = "Survey",SurveyPurpose = "Management Evaluation"),"DO NOT USE THESE DATA UNTIL VERIFIED") #expected until Catch_discrepancies addressed
  expect_gt(nrow(result), 0)
  expect_true(all(result$SurveyPurpose == "Management Evaluation"))
  
  result <- FISH_query(con,QueryType = "Survey",MDNRID = "L7844")
  expect_gt(nrow(result), 0)
  expect_true(all(result$MDNRID == "L7844"))
  
  result <- withCallingHandlers(
    FISH_query(con,QueryType = "Survey",FMU = "SLM"),
    warning = function(w) {
      if (grepl(
        "DO NOT USE as part of S&T surveys|DO NOT USE THESE DATA UNTIL VERIFIED",
        conditionMessage(w)
       )) {
         invokeRestart("muffleWarning")
      }
    }
  )
  expect_gt(nrow(result), 0)
  expect_true(all(result$FMU == "SLM"))
})


test_that("FISH_query applies multiple filters", {
  result <- withCallingHandlers(
    FISH_query(con,QueryType = "Catch",GearType = "LMFYKE",Species = "Walleye",WaterTypeAbbr = "IL"),
    warning = function(w) {
      if (grepl(
        "presence/absence|DO NOT USE THESE DATA UNTIL VERIFIED",
        conditionMessage(w)
      )) {
        invokeRestart("muffleWarning")
      }
    }
  )
  
  expect_true(all(result$Species == "Walleye"))
  expect_true(all(result$GearType == "LMFYKE"))
  
  result <- FISH_query(con,QueryType = "Catch",SurveyId = 1162,Species = "Largemouth Bass")
  expect_true(all(result$SurveyId == 1162))
  expect_true(all(result$Species == "Largemouth Bass"))
})

test_that("Catch query returns the requested catch data type", {
  result <- FISH_query(con,QueryType = "Catch",SurveyId = 805)
  expect_true(all(result$SurveyId == 805))
  expect_true(all(result$CatchTable %in% c("InchGroup",NA))) #NAs expected if effort had no catch
  expect_true(8 %in% result$SurveyEffortKey) #no catch
  
  result <- FISH_query(con,QueryType = "Catch",SurveyId = 70)
  expect_true(all(result$CatchTable %in% c("ScaleEnvelope")))
})

test_that("Catch query returns multiple catch data types", {
  result <- FISH_query(con,QueryType = "Catch",SurveyId = 3639)
  expect_true(all(result$CatchTable %in% c("Species","InchGroup"))) 
    
  speciesCount<-sum(result%>%filter(CatchTable=="Species")%>%select(TotalNumberCaught))
  expect_true(speciesCount==59)
  inchCount<-sum(result%>%filter(CatchTable=="InchGroup")%>%select(TotalNumberCaught))
  expect_true(inchCount==21)
})

# #code to look for surveys with multiple catch data types
# catchSpecies <- tbl(con, "ModuleDataCatchBySpecies") 
# catchInch <- tbl(con, "ModuleDataCatchSampleByInchGroup") 
# scaleEnvelope <- tbl(con, "ModuleDataScaleEnvelope")
# 
# moduleData <- tbl(con, "ModuleData") %>%
#   select(ModuleDataId, ModuleId)
# 
# surveyEffort <- tbl(con, "SurveyEffort") %>%
#   select(ModuleId, SurveyId)
# 
# surveyRepeats <- union_all(
#   catchSpecies %>%
#     select(ModuleDataId) %>%
#     distinct() %>%
#     inner_join(moduleData, by = "ModuleDataId") %>%
#     inner_join(surveyEffort, by = "ModuleId") %>%
#     select(SurveyId) %>%
#     distinct() %>%
#     mutate(Source = "CatchSpecies"),
#   
#   catchInch %>%
#     select(ModuleDataId) %>%
#     distinct() %>%
#     inner_join(moduleData, by = "ModuleDataId") %>%
#     inner_join(surveyEffort, by = "ModuleId") %>%
#     select(SurveyId) %>%
#     distinct() %>%
#     mutate(Source = "CatchInch"),
#   
#   scaleEnvelope %>%
#     select(ModuleDataId) %>%
#     distinct() %>%
#     inner_join(moduleData, by = "ModuleDataId") %>%
#     inner_join(surveyEffort, by = "ModuleId") %>%
#     select(SurveyId) %>%
#     distinct() %>%
#     mutate(Source = "ScaleEnvelope")) %>%
#   group_by(SurveyId) %>%
#   summarise(
#     nTables = n(),
#     Tables = str_flatten(Source, collapse = ", "),
#     .groups = "drop") %>%
#   filter(nTables > 1)%>%
#   collect()
# 
# #subset to ones that DON'T have catch discrepancies
# surveyRepeatsSubset<-surveyRepeats%>%
#   filter(!SurveyId%in%Catch_discrepancies$SurveyId)

test_that("Catch query returns P/A data", {
  expect_warning(result <- FISH_query(con,QueryType = "Catch",SurveyId = 142),"presence/absence")
  expect_true(sum(result$TotalNumberCaught == 0)==16)
  expect_true(sum(result$TotalNumberCaught > 0)==2)
  
})

test_that("FISH_query applies default legal size", {
  result <- FISH_query(con,QueryType = "Catch",SurveyId = 805)
  LMB <- result[result$Species == "Largemouth Bass" & !is.na(result$Species), ]
  
  if (nrow(LMB) > 0) {
    expect_true(all(LMB$LegalSize == 14))
  }
})

test_that("FISH_query applies special legal-size overrides to the right species", {
  result <- FISH_query(con,QueryType = "Catch",SurveyId = 805,Special_Legal_Sizes = c("Largemouth Bass" = 10))
  
  LMB <- result[result$Species == "Largemouth Bass" & !is.na(result$Species),]
  expect_true(all(LMB$LegalSize == 10))
  
  NOP <- result[result$Species == "Northern Pike" & !is.na(result$Species),]
  expect_true(all(NOP$LegalSize == 24))
})

#**need to get a verified avg length and # legal for a specific effort**
# test_that("catchByEffort calculates inch-group length correctly", {
#   effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
#   result <- catchByEffort(con, effortData)
#   BLG <- result%>%filter(Species == "Bluegill",ModuleId==85864) 
#   expect_true(all(BLG$LengthAverage == 4.17)) 
# })
# 
# test_that("catchByEffort calculates N_legal correctly", {
#   effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
#   result <- catchByEffort(con, effortData)
#   NOP <- result%>%filter(Species == "Northern Pike") 
#   expect_true(all() 
# })

##**add survey with only species-level catch data**
# test_that("catchByEffort returns NA N_legal for species-level catch", {
#   effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 1162)
#   
#   result <- catchByEffort(con, effortData)
#   
#   species <- result[result$CatchTable == "Species", ]
#   
#   if (nrow(species) > 0) {
#     expect_true(all(is.na(species$N_legal)))
#     expect_true(all(is.na(species$LegalSize)))
#   }
# })

test_that("FISH_query provides warning about catch discrepancy issues", {
  expect_warning(FISH_query(con,QueryType = "Survey",SurveyId = 15204),"DO NOT USE")
  expect_warning(FISH_query(con,QueryType = "Efforts",SurveyId = 15204),"DO NOT USE")
  expect_warning(FISH_query(con,QueryType = "Catch",SurveyId = 15204),"DO NOT USE")
  expect_no_warning(FISH_query(con,QueryType = "Catch",SurveyId = 15222)) #was split out from a survey with a discrepancy, should be no warning
  expect_warning(FISH_query(con,QueryType = "Catch",SurveyId = 15221),"DO NOT USE")
  
})

test_that("FISH_query provides warning and prompts about extra status and trends efforts", {
  expect_warning(FISH_query(con,QueryType = "Survey",SurveyId = 2785),"DO NOT USE")
  expect_warning(result<-FISH_query(con,QueryType = "Catch",SurveyId = 2785),"DO NOT USE")
  expect_warning(result2<-FISH_query(con,QueryType = "Catch",SurveyId = 2785,SurveyPurpose = "Status & Trends"),"Extra efforts removed")
  expect_equal(nrow(result)-nrow(result2),3)
})

