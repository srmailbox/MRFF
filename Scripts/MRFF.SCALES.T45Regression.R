###############################################################################
# MRFF - SCALES
# 
# This is a study funded by Mark Boyes' MRFF project to examine longitudinal
# relationships between language and mental health (including reading) across
# childhood (and into the early adolescence)
# 
# I'm looking at the SCALES data set from the UK (Norbury et al., 2025)
# 
# This script looks at predicting year 13 from year 11, controlling for gender
# done separately from the CLPM in order to deal with the small sample size.
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
  rename_with(~tolower(gsub("sentrep(.{2}).*total", "sisat.\\1_", .x)), matches("sentrep")) |> 
  mutate(gndrM = ifelse(q5gender=="Male", 1, 0))


# 1.0 Define and fit the model ####

mrfft45.mod = '
  t4Lang =~ row_t4+eow_t4+ace_t4+sisat.se_t4

  row_t4~~eow_t4
  # sisat.se_t4+ace_t4
  # eow_t4~~sisat.se_t4+ace_t4
  # sisat.se_t4~~ace_t4
  
  emo_tt5 ~ t4Lang+cc2_t4+gndrM
  con_tt5 ~ t4Lang+cc2_t4+gndrM
  
  emo_tt5~~con_tt5
  t4Lang~~cc2_t4

'

mrfft45.sem = sem(mrfft45.mod, mrffAnalysis)

fitmeasures(mrfft45.sem, fit_measures = c("rmsea", "srmr", "cfi", "tli"))
# Adding the Row to Eow correlation massively improves these.

modificationIndices(mrfft45.sem) |> arrange(mi)

summary(mrfft45.sem)

# 2.0 Figure ####
 
lytMatrix = layout_matrix(
r_4 = c(1,1), e_4 = c(3,1), a_4 = c(5,1), s._ = c(7,1)
, t4L = c(4,3)
, c2_ = c(12,3)
, e_5 = c(4, 6)
, c_5 = c(12, 6)
, gnM = c(8, 8)
)

crvList = list(
  list(from="e_4", to="r_4", new_curve = -1)
  , list(from="t4L", to="c2_", new_curve = -1.5)
  , list(from="e_5", to="c_5", new_curve = 1.5)
)

lblSizeMult = 1
lblPos = .2; lblPos2 = .65
indHeight = .5
indWidth = .5

sigEdgeCol = "darkred"
sigLnWdth = 3

semPaths(mrfft45.sem, layout=lytMatrix,intercepts=F, whatLabels = "std"
         , sizeLat = 12, sizeLat2=4, sizeMan=14, sizeMan2=4
         , asize=1.5) |>
  set_curve(crvList) |>
  mark_sig(mrfft45.sem) |> 
  set_edge_label_position(position=.4) |> 
  set_edge_label_size(label_size_list=1) |> 
  set_edge_label_size(
    label_size_list=
      c(
        "e_5 ~ t4L"=lblSizeMult, "c_5 ~ t4L"=lblSizeMult
        , "c_5 ~ c2_"=lblSizeMult, "e_5 ~ c2_"=lblSizeMult
      )
    , how = "value"
  ) |>
  set_node_height(values=c(r_4=indHeight, e_4=indHeight, a_4=indHeight, s._=indHeight)) |>
  set_node_width(values=c(r_4=indWidth, e_4=indWidth, a_4=indWidth, s._=indWidth)) |>
  set_edge_color(
    color_list =
      c(
        "e_5 ~ t4L" = sigEdgeCol, "c_5 ~ t4L" = sigEdgeCol
        , "e_5 ~ c2_" = sigEdgeCol, "c_5 ~ c2_" = sigEdgeCol
      )
  ) |>
  set_edge_line_width(
    line_width_list =
      c(
        "e_5 ~ t4L" = sigLnWdth, "c_5 ~ t4L" = sigLnWdth
      )
  ) |>
  change_node_label(
    label_list = list(
      list(node="e_5", to ="emotional")
      , list(node="c_5", to ="conduct")
      , list(node="t4L", to ="language")
      , list(node="c2_", to ="cc2")
      , list(node="gnM", to ="Male")
    ), label.norm="language"
  ) |>
  plot()
# mtext(text = c("Language", "Emotion", "Conduct", "Reading"), side=2, line=-6
#       , at = c(.5, 0, -.5, -1))
# mtext(text = c("T4", "T5"), side=1, line=0, at = c(-.725, .725))
mtext(text = paste("Age ~ ", c(11,13)), side=1, line=1, at = c(-.45, .45))
