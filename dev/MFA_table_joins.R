# Summary of tables used and joins within MFA

##WaterBody
#Functions:FISH_query
#Fields used: MDNRID,WaterBodyName
#Joins(by): Survey(MDNRID)

##Survey
#Functions:FISH_query
#Fields used: MDNRID,SurveyId,SurveyPurposeId,SurveyStatusId,SurveyBeginTimestamp,FixedOrRandom,SurveyPurposeDescription
#Joins(by): WaterBody(MDNRID),SurveyPurpose(SurveyPurposeId), SurveyStatus(SurveyStatusId)

#SurveyPurpose
#Functions:FISH_query
#Fields used: SurveyPurposeId,Descriptions
#Joins(by): Survey(SurveyPurposeId)

##SurveyStatus
#Functions:FISH_query
#Fields used: SurveyStatusId,Descriptions
#Joins(by): Survey(SurveyStatusId)

##SurveyEffort
#Functions:FISH_query
#Fields used: SurveyId,SurveyEffortId,SurveyEffortKey,GearTypeId,ModuleId,EffortNumberofGearUsed,EffortMeasurement,...
#...EffortQuantity,EffortMeasurement2,EffortQuantity2
#Joins(by): Survey (SurveyId),SurveyEffortDetail(SurveyEffortId)
#Note: need to figure out whether to use this of surveyeffort detail

##SurveyEffortDetail
#Functions:FISH_query
#Fields used: SurveyEffortId, GearId, BeginningEffortTimestamp, EndingEffortTimestamp, EffortNumberofGearUsed, EffortTotalQuantity,... 
#...EffortTotalMeasurement, EffortAlternateQuantity, EffortAlternateMeasurement
#Joins(by): SurveyEffort (SurveyEffortId), Gear(GearId)

##Gear
#Functions:FISH_query
#Fields used: GearId, GearType
#Joins(by): SurveyEffortDetail(GearId)

##ModuleDataCatchBySpecies
#Functions:catch_summary_table
#Fields used: ModuleDataId,SpeciesStrainId,TotalNumberCaught,LengthAverage,LengthMaximum,LengthMinimum
#Joins(by): ModuleData(ModuleDataId), SpeciesStrain(SpeciesStrainId)

##ModuleDataCatchSampleByInchGroup
#Functions:catch_summary_table
#Fields used: ModuleDataId,SpeciesStrainId,InchGroup,NumberCaughtUnmarked,NumberCaughtMarked
#Joins(by): ModuleData(ModuleDataId), SpeciesStrain(SpeciesStrainId)

##ModuleDataScaleEnvelope
#Functions:catch_summary_table, age_length_summary
#Fields used: SurveyId,ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthEntered,AgeClassId
#Joins(by): ModuleData(ModuleDataId), SpeciesStrain(SpeciesStrainId), AgeClass(AgeClassId)

##ModuleData
#Functions:catch_summary_table
#Fields used: ModuleId,ModuleDataId
#Joins(by): ModuleDataCatchBySpecies(ModuleDataId), ModuleDataCatchSampleByInchGroup(ModuleDataId),... 
#...ModuleDataScaleEnvelope(ModuleDataId)

##SpeciesStrain
#Functions:catch_summary_table
#Fields used: SpeciesStrainId,Species,Strain
#Joins(by): ModuleDataCatchBySpecies(SpeciesStrainId), ModuleDataCatchSampleByInchGroup(SpeciesStrainId),...
#...ModuleDataScaleEnvelope(SpeciesStrainId)

##AgeClass
#Functions:age_length_summary
#Fields used: AgeClassId,Descriptions
#Joins(by): ModuleDataScaleEnvelope(AgeClassId)