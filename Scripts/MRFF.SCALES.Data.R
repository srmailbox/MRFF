###############################################################################
# MRFF - SCALES
# 
# This is a study funded by Mark Boyes' MRFF project to examine longitudinal
# relationships between language and mental health (including reading) across
# childhood (and into the early adolescence)
# 
# I'm looking at the SCALES data set from the UK (Norbury et al., 2025)
# 
# This script does data importing, cleaning etc...
# 
# Author: Serje Robidoux
# Created: 2026-10-22
# CHANGELOG: - note this has a github. (Well, it should TODO)

# NOTES: ####
# cc2totaltX is meant to be the total of the irregs, regs, and nw total items
#   but there seems to be an arbitrary error in it somewhere that just adds some
#   particular in t2, less in t3, and not at all in t4.
# SDQ: we have teacher at t1, parent at t2, teacher at t3, child & teacher at t4,
#   teacher at t5.
#   teacher t1 and parent t2 are at basically the same age, though... I think

# 0.0 set up ####

# 1.0 Import Data ####

## Hm. Seems we only have Longitudinal data for about 500 kids, and screening
## data for 7267.
# I guess we use the screen data for some stuff, and then the "intensive" data
# for the main analyses.

scrnRaw = read_csv("SCALES/UKDA-8967-csv/csv/screen_data_t1.csv")
longRaw = read_csv("SCALES/UKDA-8968-csv/csv/intensive_t1t5.csv")

scrnDat = scrnRaw |> 
  select(
    child_number
    , age
    , q5gender
    , q10eal
    , sdqemotionalrs, sdqconductrs
  )

longDat = longRaw |> 
  select(child_number
         , starts_with("age"), starts_with("lag") # Age and time between assessments
         , contains("gender")
         , MEdQual_pt2 # Mother's education
         , starts_with("rowpvt") # Receptive Vocab
         , starts_with("eowpvt") # Expressive Vocab
         , starts_with("sentrep") # Sentence repetition
         , starts_with("narrativecomp") # language comprehension
         , starts_with("cc2") # CC2 Reading
         , -ends_with("t1") # Drop the screening time point since it overlaps with t2 so much
            # except that I want the t1 (teacher) sdq to use at "t2".
         , starts_with("sdqemotion") # SDQ Emotional Problems
         , starts_with("sdqconduct") # SDQ Conduct Problems
         )

# 2.0 Merge data #### 
# Per C Norbury, the child_number values are not related to each other.
# Ultimately we just want the 529 kids who were followed longitudinally, which
# was restricted to non-EAL

mrffDat = longDat

# 3.0 Data adjustments ####

## 3.1 Age at t5 ####
# Age at t5 seems to be overly precise. I'm going to truncate the month.

mrffDat = mrffDat |> 
  mutate(agemthst5=floor(agemthst5))

## 3.2 Ages at t1 and t2 ####
# There is a lot of overlap in the ages at t1 and t2:
# t1 runs 4;9 to 5;10 - with 424 of 529 kids (80%) 5;1 or older
# t2 runs 5;1 to 6;10 - with 213 of 529 kids 5;10 (40%) or younger

# mrffDat |> 
#   select(child_number, starts_with("agem")) |> 
#   pivot_longer(c(agemthst4, agemthst5), names_to="time", values_to="age") |> 
#   mutate(time = factor(substr(time, 8,9)), child_number=factor(child_number)) |>
#   ggplot(aes(x=time, y=age, group=child_number))+geom_line()+geom_point()
# 
# # Yeah, some kids are actually younger at t2 than at t1, so the t1 data is probably
# # best ignored. This doesn't actually make sense, since all of the kids have
# # positive lag1 values, and the lag1 value is meant to be days between t1 and
# # t2.
# 

