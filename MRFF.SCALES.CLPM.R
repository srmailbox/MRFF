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
  rename_with(~gsub("total", "_", .x), starts_with("cc2")) |> 
  rename_with(~tolower(gsub("SDQ(.{3}).*_", "\\1_", .x)), starts_with("SDQ")) |> 
  rename_with(~tolower(gsub("([re])ow.*t", "\\1ow_t", .x)), matches("[er]ow")) |> 
  rename_with(~tolower(gsub("narrativecomp(.{0,2}).*total", "ace.\\1_", .x)), matches("narrativecomp")) |> 
  rename_with(~gsub("\\._", "_", .x)) |> 
  rename_with(~tolower(gsub("sentrep(.{2}).*total", "sisat.\\1_", .x)), matches("sentrep"))


# 1.0 Define and fit the model ####

## 1.1 t2 through t5 ####
mrfft2345.mod = '
  t2Lang =~ row_t2+eow_t2+ace_t2+sisat.se_t2
  t3Lang =~ row_t3+eow_t3+ace_t3+sisat.se_t3
  t4Lang =~ row_t4+eow_t4+ace_t4+sisat.se_t4
  t5Lang =~ row_t5+eow_t5
  
  ## Autoregressive / Stability Coefficients + Cross-Lagged
  t3Lang ~ t2Lang + cc2_t2 + con_pt2 + emo_pt2
  t4Lang ~ t3Lang + cc2_t3 + con_tt3 + emo_tt3
  t5Lang ~ t4Lang + cc2_t4 + con_tt4 + emo_tt4
  
  cc2_t3 ~ cc2_t2 + t2Lang + con_pt2 + emo_pt2
  cc2_t4 ~ cc2_t3 + t3Lang + con_tt3 + emo_tt3

  con_tt3 ~ con_pt2 + cc2_t2 + t2Lang + emo_pt2
  con_tt4 ~ con_tt3 + cc2_t3 + t3Lang + emo_tt3
  con_tt5 ~ con_tt4 + cc2_t4 + t4Lang + emo_tt4

  emo_tt3 ~ con_pt2 + cc2_t2 + t2Lang + emo_pt2
  emo_tt4 ~ con_tt3 + cc2_t3 + t3Lang + emo_tt3
  emo_tt5 ~ con_tt4 + cc2_t4 + t4Lang + emo_tt4
  
  ## Recprocal / Correlational paths
  # Not sure I can do this, but it sabes me energy.
  t2Lang + cc2_t2 + con_pt2 + emo_pt2 ~~ t2Lang + cc2_t2 + con_pt2 + emo_pt2
  t3Lang + cc2_t3 + con_tt3 + emo_tt3 ~~ t3Lang + cc2_t3 + con_tt3 + emo_tt3
  t4Lang + cc2_t4 + con_tt4 + emo_tt4 ~~ t4Lang + cc2_t4 + con_tt4 + emo_tt4
  t5Lang + con_tt5 + emo_tt5 ~~ t5Lang + con_tt5 + emo_tt5
'

mrfft2345.sem = sem(mrfft2345.mod, mrffAnalysis, missing="fiml")

# There is very little SDQ data in t5 (coverages drop to around 9-14%)
# Not a lot of eow or row either, so maybe we just drop t5?
# And since we don't have cc2 then either, I think we just drop t5

## 1.2 t2 through t4 ####
mrfft234.mod = '
# Ensure measurement invariance
  # t2Lang =~ a*row_t2+b*eow_t2+c*ace_t2+d*sisat.se_t2
  # t3Lang =~ a*row_t3+b*eow_t3+c*ace_t3+d*sisat.se_t3
  # t4Lang =~ a*row_t4+b*eow_t4+c*ace_t4+d*sisat.se_t4

  # Allow measurement models to change over time.
  t2Lang =~ row_t2+eow_t2+ace_t2+sisat.se_t2
  t3Lang =~ row_t3+eow_t3+ace_t3+sisat.se_t3
  t4Lang =~ row_t4+eow_t4+ace_t4+sisat.se_t4
  row_t2 ~~ eow_t2
  row_t3 ~~ eow_t3
  row_t4 ~~ eow_t4
  sisat.se_t3~~sisat.se_t2
  sisat.se_t4~~sisat.se_t3
  

  ## Autoregressive / Stability Coefficients + Cross-Lagged
  t3Lang ~ t2Lang + cc2_t2 + con_pt2 + emo_pt2
  t4Lang ~ t3Lang + cc2_t3 + con_tt3 + emo_tt3

  cc2_t3 ~ cc2_t2 + t2Lang + con_pt2 + emo_pt2
  cc2_t4 ~ cc2_t3 + t3Lang + con_tt3 + emo_tt3

  con_tt3 ~ con_pt2 + cc2_t2 + t2Lang + emo_pt2
  con_tt4 ~ con_tt3 + cc2_t3 + t3Lang + emo_tt3

  emo_tt3 ~ con_pt2 + cc2_t2 + t2Lang + emo_pt2
  emo_tt4 ~ con_tt3 + cc2_t3 + t3Lang + emo_tt3
  
  ## Recprocal / Correlational paths
  # Not sure I can do this, but it sabes me energy.
  t2Lang + cc2_t2 + con_pt2 + emo_pt2 ~~ t2Lang + cc2_t2 + con_pt2 + emo_pt2
  t3Lang + cc2_t3 + con_tt3 + emo_tt3 ~~ t3Lang + cc2_t3 + con_tt3 + emo_tt3
  t4Lang + cc2_t4 + con_tt4 + emo_tt4 ~~ t4Lang + cc2_t4 + con_tt4 + emo_tt4
'

mrfft234.sem = sem(mrfft234.mod, mrffAnalysis, missing="fiml")

summary(mrfft234.sem)

fitmeasures(mrfft234.sem, fit_measures = c("rmsea", "srmr", "cfi", "tli"))
# Fits are very good if we allow variance (particularly for sisat) and 
# include some structure within and between the indicators.

modificationIndices(mrfft234.sem) |> arrange(mi) |> tail(20)
### Modification indices seem to suggest that allowing the language measures to
### correlate with each other would be good.
### They also want stability correlations across time for the indicators, but...
### I dunno.
### It might also be good to have some measurement invariance

# 2.0 Figure ####
## Setup
# Ok, so we have a 12 x 5
# semPaths(mrfft234.sem, intercepts=F, DoNotPlot=T)$graphAttributes$Nodes$names
lytMatrix = layout_matrix(
  r_2 = c(1,1), ew_2 = c(1,3), a_2 = c(1,5), s._2 = c(1,7)
  , r_3 = c(1,9), ew_3 = c(1,11), a_3 = c(1,13), s._3 = c(1,15)
  , r_4 = c(1,17), ew_4 = c(1,19), a_4 = c(1,21), s._4 = c(1,23)
  , t2L = c(2,4), t3L = c(2,12), t4L = c(2,20)
  , em_2 = c(3,4), em_3 = c(3,12), em_4 = c(3,20)
  , c_2 = c(4,4), c_3 = c(4,12), c_4 = c(4,20)
  , c2_2 = c(5,4), c2_3 = c(5,12), c2_4 = c(5,20)
)

crvList = list(
  list(from="s._2", to="s._3", new_curve = 1)
  , list(from="s._3", to="s._4", new_curve = 1)
  , list(from="t2L", to="c_2", new_curve = -1.5)
  , list(from="t2L", to="c2_2", new_curve = -1.5)
  , list(from="em_2", to="c2_2", new_curve = 1.5)
  , list(from="t3L", to="c_3", new_curve = -1.1)
  , list(from="t3L", to="c2_3", new_curve = -1.1)
  , list(from="em_3", to="c2_3", new_curve = 1.1)
  , list(from="t4L", to="c_4", new_curve = 1.5)
  , list(from="t4L", to="c2_4", new_curve = 1.5)
  , list(from="em_4", to="c2_4", new_curve = -1.5)
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
        "em_3 ~ t2L"=lblSizeMult, "c_3 ~ t2L"=lblSizeMult, "c2_3 ~ t2L"=lblSizeMult
        , "t3L ~ em_2"=lblSizeMult, "c_3 ~ em_2"=lblSizeMult, "c2_3 ~ em_2"=lblSizeMult
        , "t3L ~ c_2"=lblSizeMult, "em_3 ~ c_2"=lblSizeMult, "c2_3 ~ c_2"=lblSizeMult
        , "t3L ~ c2_2"=lblSizeMult, "em_3 ~ c2_2"=lblSizeMult, "c_3 ~ c2_2"=lblSizeMult
        , "em_4 ~ t3L"=lblSizeMult, "c_4 ~ t3L"=lblSizeMult, "c2_4 ~ t3L"=lblSizeMult
        , "t4L ~ em_3"=lblSizeMult, "c_4 ~ em_3"=lblSizeMult, "c2_4 ~ em_3"=lblSizeMult
        , "t4L ~ c_3"=lblSizeMult, "em_4 ~ c_3"=lblSizeMult, "c2_4 ~ c_3"=lblSizeMult
        , "t4L ~ c2_3"=lblSizeMult, "em_4 ~ c2_3"=lblSizeMult, "c_4 ~ c2_3"=lblSizeMult
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
        "em_3 ~ t2L"=lblPos, "c_3 ~ t2L"=lblPos, "c2_3 ~ t2L"=lblPos
        , "t3L ~ em_2"=lblPos2, "c_3 ~ em_2"=lblPos, "c2_3 ~ em_2"=lblPos2
        , "t3L ~ c_2"=lblPos2, "em_3 ~ c_2"=lblPos, "c2_3 ~ c_2"=lblPos2
        , "t3L ~ c2_2"=.25, "em_3 ~ c2_2"=lblPos, "c_3 ~ c2_2"=lblPos
        , "em_4 ~ t3L"=lblPos, "c_4 ~ t3L"=lblPos, "c2_4 ~ t3L"=lblPos
        , "t4L ~ em_3"=lblPos2, "c_4 ~ em_3"=lblPos, "c2_4 ~ em_3"=lblPos2
        , "t4L ~ c_3"=lblPos2, "em_4 ~ c_3"=lblPos, "c2_4 ~ c_3"=lblPos2
        , "t4L ~ c2_3"=.25, "em_4 ~ c2_3"=lblPos, "c_4 ~ c2_3"=lblPos
      )
  ) |>
  set_edge_color(
    color_list = 
      c(
        # t2 to t3
        "t3L ~ em_2"=sigEdgeCol, "t3L ~ c2_2"=sigEdgeCol
        , "c_3 ~ c2_2"=sigEdgeCol
        , "c2_3 ~ t2L"=sigEdgeCol, "c2_3 ~ em_2"=sigEdgeCol
        # t3 to t4
        , "c2_4 ~ c_3"=sigEdgeCol, "c2_4 ~ em_3"=sigEdgeCol, "c2_4 ~ t3L"=sigEdgeCol
      )
  ) |>
  set_edge_line_width(
    line_width_list = 
      c(
        # t2 to t3
        "c2_3 ~ t2L"=sigLnWdth
        # t3 to t4
        , "c2_4 ~ c_3"=sigLnWdth, "c2_4 ~ em_3"=sigLnWdth, "c2_4 ~ t3L"=sigLnWdth
      )
  ) |>
  plot()
mtext(text = c("Language", "Emotion", "Conduct", "Reading"), side=2, line=-6
      , at = c(.5, 0, -.5, -1))
mtext(text = c("T2", "T3", "T4"), side=1, line=0, at = c(-.725, 0, .725))
mtext(text = paste("Age ~ ", c(6,8,11)), side=1, line=1, at = c(-.725, 0, .725))


