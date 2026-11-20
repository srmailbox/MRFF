###############################################################################
# MRFF - SCALES
# 
# This is a study funded by Mark Boyes' MRFF project to examine longitudinal
# relationships between language and mental health (including reading) across
# childhood (and into the early adolescence)
# 
# I'm looking at the SCALES data set from the UK (Norbury et al., 2025)
# 
# This script analyzes the CLPM
# 
# Author: Serje Robidoux
# Created: 2026-10-23
# CHANGELOG: - note this has a github.
# 
# 0.0 set up ####
include(lavaan)
include(semPlot)
include(semptools)

## 0.1 get the data ####

if (!exists("mrffDat")) source("Scripts/MRFF.SCALES.Data.R")

## 0.2 rename stuff ####

mrffAnalysis = mrffDat |> 
  rename_with(~gsub("total", "_", .x)) |> 
  rename_with(~tolower(gsub("(SDQ|sdq)(.{3}).*[r_]", "\\2_", .x)), starts_with("SDQ")) |> 
  rename_with(~tolower(gsub("([re])ow.*t", "\\1ow_t", .x)), matches("[er]ow")) |> 
  rename_with(~tolower(gsub("narrativecomp(.{0,2}).*_", "ace.\\1_", .x)), matches("narrativecomp")) |> 
  rename_with(~tolower(gsub("sentrep(.{0,2}).*_", "sisat.\\1_", .x)), matches("sentrep")) |> 
  rename_with(~gsub("\\._", "_", .x)) |> 
  mutate(gndrM = ifelse(q5gender=="Male", 1, 0))



# 1.0 Define and fit the model ####

## 1.1 t2 through t5 ####
# mrfft2345.mod = '
#   t2Lang =~ row_t2+eow_t2+ace_t2+sisat.se_t2
#   t3Lang =~ row_t3+eow_t3+ace_t3+sisat.se_t3
#   t4Lang =~ row_t4+eow_t4+ace_t4+sisat.se_t4
#   t5Lang =~ row_t5+eow_t5
#   
#   ## Autoregressive / Stability Coefficients + Cross-Lagged
#   t3Lang ~ t2Lang + t2Read + con_pt2 + emo_pt2
#   t4Lang ~ t3Lang + t3Read + con_tt3 + emo_tt3
#   t5Lang ~ t4Lang + t4Read + con_tt4 + emo_tt4
#   
#   t3Read ~ t2Read + t2Lang + con_pt2 + emo_pt2
#   t4Read ~ t3Read + t3Lang + con_tt3 + emo_tt3
# 
#   con_tt3 ~ con_pt2 + t2Read + t2Lang + emo_pt2
#   con_tt4 ~ con_tt3 + t3Read + t3Lang + emo_tt3
#   con_tt5 ~ con_tt4 + t4Read + t4Lang + emo_tt4
# 
#   emo_tt3 ~ con_pt2 + t2Read + t2Lang + emo_pt2
#   emo_tt4 ~ con_tt3 + t3Read + t3Lang + emo_tt3
#   emo_tt5 ~ con_tt4 + t4Read + t4Lang + emo_tt4
#   
#   ## Recprocal / Correlational paths
#   # Not sure I can do this, but it sabes me energy.
#   t2Lang + t2Read + con_pt2 + emo_pt2 ~~ t2Lang + t2Read + con_pt2 + emo_pt2
#   t3Lang + t3Read + con_tt3 + emo_tt3 ~~ t3Lang + t3Read + con_tt3 + emo_tt3
#   t4Lang + t4Read + con_tt4 + emo_tt4 ~~ t4Lang + t4Read + con_tt4 + emo_tt4
#   t5Lang + con_tt5 + emo_tt5 ~~ t5Lang + con_tt5 + emo_tt5
# '
# 
# mrfft2345.sem = sem(mrfft2345.mod, mrffAnalysis, missing="fiml")

# There is very little SDQ data in t5 (coverages drop to around 9-14%)
# Not a lot of eow or row either, so maybe we just drop t5?
# And since we don't have cc2 then either, I think we just drop t5

## 1.2 t2 through t4 ####
mrfft234.mod = '
# Measurement Model for Language
# Ensure measurement invariance
  # t2Lang =~ a*row_t2+b*eow_t2+c*ace_t2+d*sisat.se_t2
  # t3Lang =~ a*row_t3+b*eow_t3+c*ace_t3+d*sisat.se_t3
  # t4Lang =~ a*row_t4+b*eow_t4+c*ace_t4+d*sisat.se_t4

  # Allow measurement models to change over time.
  t2Lang =~ row_t2+eow_t2+ace_t2+sisat.se_t2
  t3Lang =~ row_t3+eow_t3+ace_t3+sisat.se_t3
  t4Lang =~ row_t4+eow_t4+ace_t4+sisat.se_t4
  t2Read =~ cc2nonword_t2+cc2regular_t2+cc2irregular_t2
  t3Read =~ cc2nonword_t3+cc2regular_t3+cc2irregular_t3
  t4Read =~ cc2nonword_t4+cc2regular_t4+cc2irregular_t4
  
  # Modification Indices suggest this additional structure within the Language
  # indicators
  # row_t2 ~~ eow_t2
  # row_t3 ~~ eow_t3
  # row_t4 ~~ eow_t4
  sisat.se_t3~~sisat.se_t2
  sisat.se_t4~~sisat.se_t3+sisat.se_t2
  
  # "Control" for gender
  t2Lang ~ gndrM
  t2Read ~ gndrM
  emo_pt2 ~ gndrM
  con_pt2 ~ gndrM
  # emo_st1 ~ gndrM
  # con_st1 ~ gndrM

  ## Autoregressive / Stability Coefficients + Cross-Lagged
  t3Lang ~ t2Lang + t2Read + con_pt2 + emo_pt2
  # t3Lang ~ t2Lang + t2Read + con_st1 + emo_st1
  t4Lang ~ t3Lang + t3Read + con_tt3 + emo_tt3

  t3Read ~ t2Read + t2Lang + con_pt2 + emo_pt2
  # t3Read ~ t2Read + t2Lang + con_st1 + emo_st1
  t4Read ~ t3Read + t3Lang + con_tt3 + emo_tt3

  con_tt3 ~ con_pt2 + t2Read + t2Lang + emo_pt2
  # con_tt3 ~ con_st1 + t2Read + t2Lang + emo_st1
  con_tt4 ~ con_tt3 + t3Read + t3Lang + emo_tt3

  emo_tt3 ~ con_pt2 + t2Read + t2Lang + emo_pt2
  # emo_tt3 ~ con_st1 + t2Read + t2Lang + emo_st1
  emo_tt4 ~ con_tt3 + t3Read + t3Lang + emo_tt3
  
  ## Recprocal / Correlational paths
  # Not sure I can do this, but it sabes me energy.
  t2Lang + t2Read + con_pt2 + emo_pt2 ~~ t2Lang + t2Read + con_pt2 + emo_pt2
  # t2Lang + t2Read + con_st1 + emo_st1 ~~ t2Lang + t2Read + con_st1 + emo_st1
  t3Lang + t3Read + con_tt3 + emo_tt3 ~~ t3Lang + t3Read + con_tt3 + emo_tt3
  t4Lang + t4Read + con_tt4 + emo_tt4 ~~ t4Lang + t4Read + con_tt4 + emo_tt4
'

mrfft234.sem = sem(mrfft234.mod, mrffAnalysis, missing="fiml")

# summary(mrfft234.sem, standardized=TRUE)

fitmeasures(mrfft234.sem, fit_measures = c("rmsea", "srmr", "cfi", "tli"))
# If I ditch the modification indices additions, fits aren't great.

modificationIndices(mrfft234.sem) |> arrange(mi) |> tail(20)
### Modification indices seem to suggest that allowing the language measures to
### correlate with each other would be good.
### They also want stability correlations across time for the indicators, but...
### I dunno.
### It might also be good to have some measurement invariance

## 1.3 t1/2 to t4 ####
# This version uses teacher SDQ from t1, which is a bit of a "messy" thing.
mrfft1234.mod = '
# Measurement Model for Language

  # Allow measurement models to change over time.
  t2Lang =~ row_t2+eow_t2+ace_t2+sisat.se_t2
  t3Lang =~ row_t3+eow_t3+ace_t3+sisat.se_t3
  t4Lang =~ row_t4+eow_t4+ace_t4+sisat.se_t4
  t2Read =~ cc2nonword_t2+cc2regular_t2+cc2irregular_t2
  t3Read =~ cc2nonword_t3+cc2regular_t3+cc2irregular_t3
  t4Read =~ cc2nonword_t4+cc2regular_t4+cc2irregular_t4
  
  # Modification Indices suggest this additional structure within the Language
  # indicators
  # row_t2 ~~ eow_t2
  # row_t3 ~~ eow_t3
  # row_t4 ~~ eow_t4
  sisat.se_t3~~sisat.se_t2
  sisat.se_t4~~sisat.se_t3+sisat.se_t2
  
  # "Control" for gender
  t2Lang ~ gndrM
  t2Read ~ gndrM
  # emo_pt2 ~ gndrM
  # con_pt2 ~ gndrM
  emo_st1 ~ gndrM
  con_st1 ~ gndrM

  ## Autoregressive / Stability Coefficients + Cross-Lagged
  # t3Lang ~ t2Lang + t2Read + con_pt2 + emo_pt2
  t3Lang ~ t2Lang + t2Read + con_st1 + emo_st1
  t4Lang ~ t3Lang + t3Read + con_tt3 + emo_tt3

  # t3Read ~ t2Read + t2Lang + con_pt2 + emo_pt2
  t3Read ~ t2Read + t2Lang + con_st1 + emo_st1
  t4Read ~ t3Read + t3Lang + con_tt3 + emo_tt3

  # con_tt3 ~ con_pt2 + t2Read + t2Lang + emo_pt2
  con_tt3 ~ con_st1 + t2Read + t2Lang + emo_st1
  con_tt4 ~ con_tt3 + t3Read + t3Lang + emo_tt3

  # emo_tt3 ~ con_pt2 + t2Read + t2Lang + emo_pt2
  emo_tt3 ~ con_st1 + t2Read + t2Lang + emo_st1
  emo_tt4 ~ con_tt3 + t3Read + t3Lang + emo_tt3
  
  ## Recprocal / Correlational paths
  # Not sure I can do this, but it sabes me energy.
  # t2Lang + t2Read + con_pt2 + emo_pt2 ~~ t2Lang + t2Read + con_pt2 + emo_pt2
  t2Lang + t2Read + con_st1 + emo_st1 ~~ t2Lang + t2Read + con_st1 + emo_st1
  t3Lang + t3Read + con_tt3 + emo_tt3 ~~ t3Lang + t3Read + con_tt3 + emo_tt3
  t4Lang + t4Read + con_tt4 + emo_tt4 ~~ t4Lang + t4Read + con_tt4 + emo_tt4
'

mrfft1234.sem = sem(mrfft1234.mod, mrffAnalysis, missing="fiml")

# summary(mrfft1234.sem, standardized=TRUE)

fitmeasures(mrfft1234.sem, fit_measures = c("rmsea", "srmr", "cfi", "tli"))
# If get roughly identical fits whether we use teacher or parent at t1/2

modificationIndices(mrfft1234.sem) |> arrange(mi) |> tail(20)

## 1.4 Measurement Invariance ####
mrfft234.msinv = '
# Ensure measurement invariance
  t2Lang =~ a*row_t2+b*eow_t2+c*ace_t2+d*sisat.se_t2
  t3Lang =~ a*row_t3+b*eow_t3+c*ace_t3+d*sisat.se_t3
  t4Lang =~ a*row_t4+b*eow_t4+c*ace_t4+d*sisat.se_t4

  # Allow measurement models to change over time.
  # t2Lang =~ row_t2+eow_t2+ace_t2+sisat.se_t2
  # t3Lang =~ row_t3+eow_t3+ace_t3+sisat.se_t3
  # t4Lang =~ row_t4+eow_t4+ace_t4+sisat.se_t4
  row_t2 ~~ eow_t2
  row_t3 ~~ eow_t3
  row_t4 ~~ eow_t4
  sisat.se_t3~~sisat.se_t2
  sisat.se_t4~~sisat.se_t3
  
  # Control for Gender
  t2Lang ~ gndrM
  emo_pt2 ~ gndrM
  con_pt2 ~ gndrM
  t2Read ~ gndrM
  

  ## Autoregressive / Stability Coefficients + Cross-Lagged
  t3Lang ~ t2Lang + t2Read + con_pt2 + emo_pt2
  t4Lang ~ t3Lang + t3Read + con_tt3 + emo_tt3

  t3Read ~ t2Read + t2Lang + con_pt2 + emo_pt2
  t4Read ~ t3Read + t3Lang + con_tt3 + emo_tt3

  con_tt3 ~ con_pt2 + t2Read + t2Lang + emo_pt2
  con_tt4 ~ con_tt3 + t3Read + t3Lang + emo_tt3

  emo_tt3 ~ con_pt2 + t2Read + t2Lang + emo_pt2
  emo_tt4 ~ con_tt3 + t3Read + t3Lang + emo_tt3
  
  ## Recprocal / Correlational paths
  # Not sure I can do this, but it sabes me energy.
  t2Lang + t2Read + con_pt2 + emo_pt2 ~~ t2Lang + t2Read + con_pt2 + emo_pt2
  t3Lang + t3Read + con_tt3 + emo_tt3 ~~ t3Lang + t3Read + con_tt3 + emo_tt3
  t4Lang + t4Read + con_tt4 + emo_tt4 ~~ t4Lang + t4Read + con_tt4 + emo_tt4
'

mrfft234msinv.sem = sem(mrfft234.msinv, mrffAnalysis, missing="fiml")

summary(mrfft234msinv.sem)

fitmeasures(mrfft234msinv.sem, fit_measures = c("rmsea", "srmr", "cfi", "tli"))
# Not great, but not terrible

modificationIndices(mrfft234.sem) |> arrange(mi) |> tail(20)
### Modification indices seem to suggest that allowing the language measures to
### correlate with each other would be good.
### They also want stability correlations across time for the indicators, but...
### I dunno.

# 2.0 Figure ####
## Setup
# Ok, so we have a 12 x 5
# semPaths(mrfft234.sem, intercepts=F, DoNotPlot=T)$graphAttributes$Nodes$names
lytMatrix = layout_matrix(
  r_2 = c(1,1), ew_2 = c(1,3), a_2 = c(1,5), s._2 = c(1,7)
  , r_3 = c(1,9), ew_3 = c(1,11), a_3 = c(1,13), s._3 = c(1,15)
  , r_4 = c(1,17), ew_4 = c(1,19), a_4 = c(1,21), s._4 = c(1,23)
  , t2L = c(3,4), t3L = c(3,12), t4L = c(3,20)
  , em_2 = c(5,4), em_3 = c(5,12), em_4 = c(5,20)
  , gnM = c(6,1)
  , c_2 = c(7,4), c_3 = c(7,12), c_4 = c(7,20)
  , t2R= c(9,4), t3R = c(9,12), t4R = c(9,20)
  , cc2n_2 = c(11,2), cc2rg_2 = c(11,4), cc2rr_2 = c(11,6)
  , cc2n_3 = c(11,10), cc2rg_3 = c(11,12), cc2rr_3 = c(11,14)
  , cc2n_4 = c(11,18), cc2rg_4 = c(11,20), cc2rr_4 = c(11,22)
  
)

ndNames = 
  c(t2L = "Lang2", t3L = "Lang3", t4L = "Lang4"
    , em_2 = "Emo2p", em_3 = "Emo3", em_4 = "Emo4"
    , c_2 = "Cond2p", c_3 = "Cond3", c_4 = "Cond4"
    , t2R= "CC2 2", t3R = "CC2 3", t4R = "CC2 4"
    , cc2n_2 = "nw 2", cc2rg_2 = "reg 2", cc2rr_2 = "irr 2"
    , cc2n_3 = "nw 3", cc2rg_3 = "reg 3", cc2rr_3 = "irr 3"
    , cc2n_4 = "nw 4", cc2rg_4 = "reg 4", cc2rr_4 = "irr 4"
    , gnM = "Male"
  )

crvList = list(
  list(from="s._2", to="s._3", new_curve = 1)
  , list(from="s._3", to="s._4", new_curve = 1)
  , list(from="s._2", to="s._4", new_curve = 1)
  , list(from="t2L", to="c_2", new_curve = -1.5)
  , list(from="t2L", to="t2Read", new_curve = -1.5)
  , list(from="em_2", to="t2Read", new_curve = 1.5)
  , list(from="t3L", to="c_3", new_curve = -1.1)
  , list(from="t3L", to="t3Read", new_curve = -1.1)
  , list(from="em_3", to="t3Read", new_curve = 1.1)
  , list(from="t4L", to="c_4", new_curve = 1.5)
  , list(from="t4L", to="t4Read", new_curve = 1.5)
  , list(from="em_4", to="t4Read", new_curve = -1.5)
)

lblSizeMult = 1
lblPos = .2; lblPos2 = .65
indHeight = .5
indWidth = .5

sigEdgeCol = "darkred"
sigLnWdth = 3

semPaths(mrfft234.sem, layout=lytMatrix,intercepts=F, whatLabels = "std"
         , sizeLat = 9, sizeLat2=4, sizeMan=9, sizeMan2=4
         , asize=1.5) |> 
  set_curve(crvList) |> 
  mark_sig(mrfft234.sem, ) |> 
  set_edge_label_position(position=.4) |> 
  set_edge_label_size(label_size_list=0.01) |> 
  set_edge_label_size(
    label_size_list=
      c(
        "em_3 ~ t2L"=lblSizeMult, "c_3 ~ t2L"=lblSizeMult, "t3Read ~ t2L"=lblSizeMult
        , "t3L ~ em_2"=lblSizeMult, "c_3 ~ em_2"=lblSizeMult, "t3Read ~ em_2"=lblSizeMult
        , "t3L ~ c_2"=lblSizeMult, "em_3 ~ c_2"=lblSizeMult, "t3Read ~ c_2"=lblSizeMult
        , "t3L ~ t2Read"=lblSizeMult, "em_3 ~ t2Read"=lblSizeMult, "c_3 ~ t2Read"=lblSizeMult
        , "em_4 ~ t3L"=lblSizeMult, "c_4 ~ t3L"=lblSizeMult, "t4Read ~ t3L"=lblSizeMult
        , "t4L ~ em_3"=lblSizeMult, "c_4 ~ em_3"=lblSizeMult, "t4Read ~ em_3"=lblSizeMult
        , "t4L ~ c_3"=lblSizeMult, "em_4 ~ c_3"=lblSizeMult, "t4Read ~ c_3"=lblSizeMult
        , "t4L ~ t3Read"=lblSizeMult, "em_4 ~ t3Read"=lblSizeMult, "c_4 ~ t3Read"=lblSizeMult
      )
    , how = "value"
  ) |> 
  set_node_height(
    values=c(
      r_2=indHeight, ew_2=indHeight, a_2=indHeight, s._2=indHeight
      , r_3=indHeight, ew_3=indHeight, a_3=indHeight, s._3=indHeight
      , r_4=indHeight, ew_4=indHeight, a_4=indHeight, s._4=indHeight
      , gnM=indHeight
      , cc2n_2=indHeight, cc2rg_2=indHeight, cc2rr_2=indHeight
      , cc2n_3=indHeight, cc2rg_3=indHeight, cc2rr_3=indHeight
      , cc2n_4=indHeight, cc2rg_4=indHeight, cc2rr_4=indHeight
      )
  ) |> 
  set_node_width(
    values=c(
      r_2=indWidth, ew_2=indWidth, a_2=indWidth, s._2=indWidth
      , r_3=indWidth, ew_3=indWidth, a_3=indWidth, s._3=indWidth
      , r_4=indWidth, ew_4=indWidth, a_4=indWidth, s._4=indWidth
      , gnM=indWidth
      , cc2n_2=indWidth, cc2rg_2=indWidth, cc2rr_2=indWidth
      , cc2n_3=indWidth, cc2rg_3=indWidth, cc2rr_3=indWidth
      , cc2n_4=indWidth, cc2rg_4=indWidth, cc2rr_4=indWidth
    )
  ) |> 
  set_edge_label_position(
    position_list=
      c(
        "em_3 ~ t2L"=lblPos, "c_3 ~ t2L"=lblPos, "t3Read ~ t2L"=lblPos
        , "t3L ~ em_2"=lblPos2, "c_3 ~ em_2"=lblPos, "t3Read ~ em_2"=lblPos2
        , "t3L ~ c_2"=lblPos2, "em_3 ~ c_2"=lblPos, "t3Read ~ c_2"=lblPos2
        , "t3L ~ t2Read"=.25, "em_3 ~ t2Read"=lblPos, "c_3 ~ t2Read"=lblPos
        , "em_4 ~ t3L"=lblPos, "c_4 ~ t3L"=lblPos, "t4Read ~ t3L"=lblPos
        , "t4L ~ em_3"=lblPos2, "c_4 ~ em_3"=lblPos, "t4Read ~ em_3"=lblPos2
        , "t4L ~ c_3"=lblPos2, "em_4 ~ c_3"=lblPos, "t4Read ~ c_3"=lblPos2
        , "t4L ~ t3Read"=.25, "em_4 ~ t3Read"=lblPos, "c_4 ~ t3Read"=lblPos
      )
  ) |>
  set_edge_color(
    color_list = 
      c(
        # t2 to t3
        "t3L ~ em_2"=sigEdgeCol, "t3L ~ t2Read"=sigEdgeCol
        , "c_3 ~ t2Read"=sigEdgeCol
        , "t3Read ~ t2L"=sigEdgeCol, "t3Read ~ em_2"=sigEdgeCol
        # t3 to t4
        , "t4Read ~ c_3"=sigEdgeCol, "t4Read ~ em_3"=sigEdgeCol, "t4Read ~ t3L"=sigEdgeCol
      )
  ) |>
  set_edge_line_width(
    line_width_list = 
      c(
        # t2 to t3
        "t3Read ~ t2L"=sigLnWdth, "t3L ~ t2Read"=sigLnWdth
        # t3 to t4
        , "t4Read ~ c_3"=sigLnWdth, "t4Read ~ em_3"=sigLnWdth#, "t4Read ~ t3L"=sigLnWdth
      )
  ) |>
  change_node_label(
    label_list=ndNames
  ) |> 
  plot()

mtext(text = c("Language", "Emotion", "Conduct", "Reading"), side=2, line=-5
      , at = c(.625, .20833, -.20833, -.625))
mtext(text = c("T2", "T3", "T4"), side=1, line=0, at = c(-.725, 0, .725))
mtext(text = paste("Age ~ ", c(6,8,11)), side=1, line=1, at = c(-.725, 0, .725))
# title("Pre Modification Indices")
## 2.1 Teacher at t1 rather than parent at t2 ####
## Setup
# Ok, so we have a 12 x 5
# semPaths(mrfft1234.sem, intercepts=F, DoNotPlot=T)$graphAttributes$Nodes$names
lytMatrix = layout_matrix(
  r_2 = c(1,1), e_2 = c(1,3), a_2 = c(1,5), s._2 = c(1,7)
  , r_3 = c(1,9), ew_3 = c(1,11), a_3 = c(1,13), s._3 = c(1,15)
  , r_4 = c(1,17), ew_4 = c(1,19), a_4 = c(1,21), s._4 = c(1,23)
  , t2L = c(3,4), t3L = c(3,12), t4L = c(3,20)
  , e_1 = c(5,4), em_3 = c(5,12), em_4 = c(5,20)
  , gnM = c(6,1)
  , c_1 = c(7,4), c_3 = c(7,12), c_4 = c(7,20)
  , t2R= c(9,4), t3R = c(9,12), t4R = c(9,20)
  , cc2n_2 = c(11,2), cc2rg_2 = c(11,4), cc2rr_2 = c(11,6)
  , cc2n_3 = c(11,10), cc2rg_3 = c(11,12), cc2rr_3 = c(11,14)
  , cc2n_4 = c(11,18), cc2rg_4 = c(11,20), cc2rr_4 = c(11,22)
)

ndNames = 
  c(t2L = "Lang2", t3L = "Lang3", t4L = "Lang4"
    , e_1 = "Emo1t", em_3 = "Emo3", em_4 = "Emo4"
    , c_1 = "Cond1t", c_3 = "Cond3", c_4 = "Cond4"
    , t2R= "CC2 2", t3R = "CC2 3", t4R = "CC2 4"
    , cc2n_2 = "nw 2", cc2rg_2 = "reg 2", cc2rr_2 = "irr 2"
    , cc2n_3 = "nw 3", cc2rg_3 = "reg 3", cc2rr_3 = "irr 3"
    , cc2n_4 = "nw 4", cc2rg_4 = "reg 4", cc2rr_4 = "irr 4"
    , gnM = "Male"
  )

crvList = list(
  list(from="s._2", to="s._3", new_curve = 1)
  , list(from="s._3", to="s._4", new_curve = 1)
  , list(from="s._2", to="s._4", new_curve = 1)
  , list(from="t2L", to="c_1", new_curve = -1.5)
  , list(from="t2L", to="t2Read", new_curve = -1.5)
  , list(from="e_1", to="t2Read", new_curve = 1.5)
  , list(from="t3L", to="c_3", new_curve = -1.1)
  , list(from="t3L", to="t3Read", new_curve = -1.1)
  , list(from="em_3", to="t3Read", new_curve = 1.1)
  , list(from="t4L", to="c_4", new_curve = 1.5)
  , list(from="t4L", to="t4Read", new_curve = 1.5)
  , list(from="em_4", to="t4Read", new_curve = -1.5)
)

lblSizeMult = 1
lblPos = .2; lblPos2 = .65
indHeight = .5
indWidth = .5

sigEdgeCol = "darkred"
sigLnWdth = 3

semPaths(mrfft1234.sem, layout=lytMatrix,intercepts=F, whatLabels = "std"
         , sizeLat = 9, sizeLat2=4, sizeMan=9, sizeMan2=4
         , asize=1.5) |> 
  set_curve(crvList) |> 
  mark_sig(mrfft1234.sem) |> 
  set_edge_label_position(position=.4) |> 
  set_edge_label_size(label_size_list=0.01) |> 
  set_edge_label_size(
    label_size_list=
      c(
        "em_3 ~ t2L"=lblSizeMult, "c_3 ~ t2L"=lblSizeMult, "t3Read ~ t2L"=lblSizeMult
        , "t3L ~ e_1"=lblSizeMult, "c_3 ~ e_1"=lblSizeMult, "t3Read ~ e_1"=lblSizeMult
        , "t3L ~ c_2"=lblSizeMult, "em_3 ~ c_2"=lblSizeMult, "t3Read ~ c_2"=lblSizeMult
        , "t3L ~ t2Read"=lblSizeMult, "em_3 ~ t2Read"=lblSizeMult, "c_3 ~ t2Read"=lblSizeMult
        , "em_4 ~ t3L"=lblSizeMult, "c_4 ~ t3L"=lblSizeMult, "t4Read ~ t3L"=lblSizeMult
        , "t4L ~ em_3"=lblSizeMult, "c_4 ~ em_3"=lblSizeMult, "t4Read ~ em_3"=lblSizeMult
        , "t4L ~ c_3"=lblSizeMult, "em_4 ~ c_3"=lblSizeMult, "t4Read ~ c_3"=lblSizeMult
        , "t4L ~ t3Read"=lblSizeMult, "em_4 ~ t3Read"=lblSizeMult, "c_4 ~ t3Read"=lblSizeMult
      )
    , how = "value"
  ) |> 
  set_node_height(
    values=c(
      r_2=indHeight, e_2=indHeight, a_2=indHeight, s._2=indHeight
      , r_3=indHeight, ew_3=indHeight, a_3=indHeight, s._3=indHeight
      , r_4=indHeight, ew_4=indHeight, a_4=indHeight, s._4=indHeight
      , gnM=indHeight
      , cc2n_2=indHeight, cc2rg_2=indHeight, cc2rr_2=indHeight
      , cc2n_3=indHeight, cc2rg_3=indHeight, cc2rr_3=indHeight
      , cc2n_4=indHeight, cc2rg_4=indHeight, cc2rr_4=indHeight
    )
  ) |> 
  set_node_width(
    values=c(
      r_2=indWidth, e_2=indWidth, a_2=indWidth, s._2=indWidth
      , r_3=indWidth, ew_3=indWidth, a_3=indWidth, s._3=indWidth
      , r_4=indWidth, ew_4=indWidth, a_4=indWidth, s._4=indWidth
      , gnM=indWidth
      , cc2n_2=indWidth, cc2rg_2=indWidth, cc2rr_2=indWidth
      , cc2n_3=indWidth, cc2rg_3=indWidth, cc2rr_3=indWidth
      , cc2n_4=indWidth, cc2rg_4=indWidth, cc2rr_4=indWidth
    )
  ) |> 
  set_edge_label_position(
    position_list=
      c(
        "em_3 ~ t2L"=lblPos, "c_3 ~ t2L"=lblPos, "t3Read ~ t2L"=lblPos
        , "t3L ~ e_1"=lblPos2, "c_3 ~ e_1"=lblPos, "t3Read ~ e_1"=lblPos2
        , "t3L ~ c_2"=lblPos2, "em_3 ~ c_2"=lblPos, "t3Read ~ c_2"=lblPos2
        , "t3L ~ t2Read"=.25, "em_3 ~ t2Read"=lblPos, "c_3 ~ t2Read"=lblPos
        , "em_4 ~ t3L"=lblPos, "c_4 ~ t3L"=lblPos, "t4Read ~ t3L"=lblPos
        , "t4L ~ em_3"=lblPos2, "c_4 ~ em_3"=lblPos, "t4Read ~ em_3"=lblPos2
        , "t4L ~ c_3"=lblPos2, "em_4 ~ c_3"=lblPos, "t4Read ~ c_3"=lblPos2
        , "t4L ~ t3Read"=.25, "em_4 ~ t3Read"=lblPos, "c_4 ~ t3Read"=lblPos
      )
  ) |>
  set_edge_color(
    color_list = 
      c(
        # t2 to t3
        "t3L ~ e_2"=sigEdgeCol, "t3L ~ t2Read"=sigEdgeCol
        #, "c_3 ~ t2Read"=sigEdgeCol
        , "t3Read ~ t2L"=sigEdgeCol, "t3Read ~ e_2"=sigEdgeCol
        # t3 to t4
        , "t4Read ~ c_3"=sigEdgeCol, "t4Read ~ em_3"=sigEdgeCol, "t4Read ~ t3L"=sigEdgeCol
      )
  ) |>
  set_edge_line_width(
    line_width_list = 
      c(
        # t2 to t3
        "t3Read ~ t2L"=sigLnWdth#, "t3L ~ t2Read"=sigLnWdth
        # t3 to t4
        , "t4Read ~ c_3"=sigLnWdth, "t4Read ~ em_3"=sigLnWdth#, "t4Read ~ t3L"=sigLnWdth
      )
  ) |>
  change_node_label(
    label_list=ndNames
  ) |> 
  plot()

mtext(text = c("Language", "Emotion", "Conduct", "Reading"), side=2, line=-5
      , at = c(.625, .20833, -.20833, -.625))
mtext(text = c("T1/2", "T3", "T4"), side=1, line=0, at = c(-.725, 0, .725))
mtext(text = paste("Age ~ ", c("5/6","8","11")), side=1, line=1, at = c(-.725, 0, .725))

## 2.2 Measurement Invariance ####

semPaths(mrfft234msinv.sem, layout=lytMatrix,intercepts=F, whatLabels = "std"
         , sizeLat = 9, sizeLat2=4, sizeMan=9, sizeMan2=4
         , asize=1.5) |> 
  set_curve(crvList) |> 
  mark_sig(mrfft234msinv.sem) |> 
  set_edge_label_position(position=.4) |> 
  set_edge_label_size(label_size_list=0.01) |> 
  set_edge_label_size(
    label_size_list=
      c(
        "em_3 ~ t2L"=lblSizeMult, "c_3 ~ t2L"=lblSizeMult, "t3Read ~ t2L"=lblSizeMult
        , "t3L ~ em_2"=lblSizeMult, "c_3 ~ em_2"=lblSizeMult, "t3Read ~ em_2"=lblSizeMult
        , "t3L ~ c_2"=lblSizeMult, "em_3 ~ c_2"=lblSizeMult, "t3Read ~ c_2"=lblSizeMult
        , "t3L ~ t2Read"=lblSizeMult, "em_3 ~ t2Read"=lblSizeMult, "c_3 ~ t2Read"=lblSizeMult
        , "em_4 ~ t3L"=lblSizeMult, "c_4 ~ t3L"=lblSizeMult, "t4Read ~ t3L"=lblSizeMult
        , "t4L ~ em_3"=lblSizeMult, "c_4 ~ em_3"=lblSizeMult, "t4Read ~ em_3"=lblSizeMult
        , "t4L ~ c_3"=lblSizeMult, "em_4 ~ c_3"=lblSizeMult, "t4Read ~ c_3"=lblSizeMult
        , "t4L ~ t3Read"=lblSizeMult, "em_4 ~ t3Read"=lblSizeMult, "c_4 ~ t3Read"=lblSizeMult
      )
    , how = "value"
  ) |> 
  set_node_height(values=c(r_2=indHeight, ew_2=indHeight, a_2=indHeight, s._2=indHeight
                           , r_3=indHeight, ew_3=indHeight, a_3=indHeight, s._3=indHeight
                           , r_4=indHeight, ew_4=indHeight, a_4=indHeight, s._4=indHeight)) |> 
  set_node_width(values=c(r_2=indWidth, ew_2=indWidth, a_2=indWidth, s._2=indWidth
                          , r_3=indWidth, ew_3=indWidth, a_3=indWidth, s._3=indWidth
                          , r_4=indWidth, ew_4=indWidth, a_4=indWidth, s._4=indWidth)) |> 
  set_edge_label_position(
    position_list=
      c(
        "em_3 ~ t2L"=lblPos, "c_3 ~ t2L"=lblPos, "t3Read ~ t2L"=lblPos
        , "t3L ~ em_2"=lblPos2, "c_3 ~ em_2"=lblPos, "t3Read ~ em_2"=lblPos2
        , "t3L ~ c_2"=lblPos2, "em_3 ~ c_2"=lblPos, "t3Read ~ c_2"=lblPos2
        , "t3L ~ t2Read"=.25, "em_3 ~ t2Read"=lblPos, "c_3 ~ t2Read"=lblPos
        , "em_4 ~ t3L"=lblPos, "c_4 ~ t3L"=lblPos, "t4Read ~ t3L"=lblPos
        , "t4L ~ em_3"=lblPos2, "c_4 ~ em_3"=lblPos, "t4Read ~ em_3"=lblPos2
        , "t4L ~ c_3"=lblPos2, "em_4 ~ c_3"=lblPos, "t4Read ~ c_3"=lblPos2
        , "t4L ~ t3Read"=.25, "em_4 ~ t3Read"=lblPos, "c_4 ~ t3Read"=lblPos
      )
  ) |>
  set_edge_color(
    color_list = 
      c(
        # t2 to t3
        "t3L ~ em_2"=sigEdgeCol, "t3L ~ t2Read"=sigEdgeCol
        , "c_3 ~ t2Read"=sigEdgeCol
        , "t3Read ~ t2L"=sigEdgeCol, "t3Read ~ em_2"=sigEdgeCol
        # t3 to t4
        , "t4Read ~ c_3"=sigEdgeCol, "t4Read ~ em_3"=sigEdgeCol, "t4Read ~ t3L"=sigEdgeCol
      )
  ) |>
  set_edge_line_width(
    line_width_list = 
      c(
        # t2 to t3
        "t3Read ~ t2L"=sigLnWdth
        # t3 to t4
        , "t4Read ~ c_3"=sigLnWdth, "t4Read ~ em_3"=sigLnWdth, "t4Read ~ t3L"=sigLnWdth
      )
  ) |>
  plot()
# title("Measurement Invariance")
mtext(text = c("Language", "Emotion", "Conduct", "Reading"), side=2, line=-6
      , at = c(.5, 0, -.5, -1))
mtext(text = c("T2", "T3", "T4"), side=1, line=0, at = c(-.725, 0, .725))
mtext(text = paste("Age ~ ", c(6,8,11)), side=1, line=1, at = c(-.725, 0, .725))

## Statistically the model witout measurement invariance is better, but perhaps
## more importantly there's no good reason to argue for measurement invariance
## and even more importantly, it doesn't change anything.
