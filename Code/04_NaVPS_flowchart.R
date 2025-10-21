# -----------------------------------------------------------------------------
# Script Name: NCH_003_flowchart.R
# Purpose: Build and export CONSORT-style flowchart for the main analysis
# Author: Florian Halbeisen
# Date Created: 2025-01-23
# -----------------------------------------------------------------------------
# Description:
#   - Creates a CONSORT flowchart using DiagrammeR (Graphviz DOT language)
#   - Exports the diagram to PNG and SVG
#
# Inputs:
#   - No direct data input (the counts are hard-coded in node labels)
#
# Outputs:
#   - PNG:  paste0(path_study, "/TableFigure/graph.png")
#   - SVG:  paste0(path_study, "/TableFigure/flowchart.svg")
#
# Dependencies (R packages):
#   - DiagrammeR        (grViz, export_svg)
#   - DiagrammeRsvg     (export_svg backend used by DiagrammeR)
#   - rsvg              (rsvg_png)
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Define the flowchart graph in Graphviz DOT language via a string
# -----------------------------------------------------------------------------
graph <- "
digraph flowchart {

  # ----------------------------
  # Global node appearance
  # ----------------------------
  node [
    shape = box,
    fixedsize = true,
    width = 4,
    height = 1,
    fontsize = 15,
    fontname = \"Times New Roman\"
  ]

  # ----------------------------
  # Exclusion boxes (Ultrasound arm)
  # ----------------------------
  exclude_ultrasound[shape=box, label=<
    <FONT POINT-SIZE=\"15\">Excluded (N = 2)</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• 2 - No intervention</FONT><BR ALIGN=\"LEFT\"/>
  >]

  exclude_ultrasound_fu1[shape=box, label=<
    <FONT POINT-SIZE=\"15\">Excluded (N = 9)</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• 5 - Death</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• 2 - VPS explanted</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• 2 - Loss of follow-up </FONT><BR ALIGN=\"LEFT\"/>
  >]

  exclude_ultrasound_fu2[shape=box, label=<
    <FONT POINT-SIZE=\"15\">Excluded (N = 5)</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• 3 - Death</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• 2 - Loss of follow-up </FONT><BR ALIGN=\"LEFT\"/>
  >]

  # ----------------------------
  # Exclusion boxes (Stereotactic arm)
  # ----------------------------
  exclude_stereo[shape=box, label=<
    <FONT POINT-SIZE=\"15\">Excluded (N = 5)</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• 4 - No intervention</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• 1 - Missing data</FONT><BR ALIGN=\"LEFT\"/>
  >]

  exclude_stereo_fu1[shape=box, label=<
    <FONT POINT-SIZE=\"15\">Excluded (N = 5)</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• 3 - Death</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• 2 - Loss of follow-up </FONT><BR ALIGN=\"LEFT\"/>
  >]

  # ----------------------------
  # Core CONSORT nodes
  # ----------------------------
  Start       [shape=box, label=\"Assessed for eligibility (N = 153)\"]

  Excluded [label=<
    <FONT POINT-SIZE=\"15\">Excluded (N = 19)</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• Did not meet criteria</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• Declined to participate</FONT><BR ALIGN=\"LEFT\"/>
    <FONT POINT-SIZE=\"11\">• No intervention as planned</FONT><BR ALIGN=\"LEFT\"/>
  >]

  Randomized  [shape=box, label=\"Randomized (N = 134)\"]

  alloc_ultrasound [shape=box, label=\"Allocated to Ultrasound (N = 66)\", group = ultrasound]
  alloc_stero      [shape=box, label=\"Allocated to Stereotactic navigation (N = 68)\", group = stereo]

  analysis_ultrasound [shape=box, label=\"Primary outcome assessed (N = 64)\", group = ultrasound]
  analysis_stereo     [shape=box, label=\"Primary outcome assessed (N = 63)\", group = stereo]

  # Spacer/blank nodes to control layout
  Excluded_blank             [label = '', width = 0.01, height = 0.01]
  exclude_ultrasound_blank   [label = '', width = 0.01, height = 0.01, group = ultrasound]
  exclude_stereo_blank       [label = '', width = 0.01, height = 0.01, group = stereo]
  ultrasound_blank           [label = '', width = 0.01, height = 0.01, group = ultrasound]
  Split_treatment            [label = '', width = 0.01, height = 0.01]
  stereo_blank               [label = '', width = 0.01, height = 0.01, group = stereo]

  # ----------------------------
  # Follow-up 1 nodes
  # ----------------------------
  exclude_ultrasound_blank_fu1 [label = '', width = 0.01, height = 0.01, group = ultrasound]
  fu1_ultrasound               [shape=box, label=\"1st follow-up (N = 55)\", group = ultrasound]
  exclude_stereo_blank_fu1     [label = '', width = 0.01, height = 0.01, group = stereo]
  fu1_stereo                   [shape=box, label=\"1st follow-up (N = 50)\", group = stereo]

  # ----------------------------
  # Follow-up 2 nodes
  # ----------------------------
  exclude_ultrasound_blank_fu2 [label = '', width = 0.01, height = 0.01, group = ultrasound]
  fu2_ultrasound               [shape=box, label=\"2nd follow-up (N = 50)\", group = ultrasound]
  exclude_stereo_blank_fu2     [label = '', width = 0.01, height = 0.01, group = stereo]
  fu2_stereo                   [shape=box, label=\"2nd follow-up (N = 50)\", group = stereo]

  # ---------------------------------------------------------------------------
  # Edges: define flow between stages (including layout-only edges)
  # ---------------------------------------------------------------------------
  # Eligibility to randomization/exclusion
  Start -> Excluded_blank [dir = none];
  Excluded_blank -> Randomized;
  Excluded_blank -> Excluded [minlen = 3];

  # Randomisation split (center -> two arms)
  Randomized -> Split_treatment [dir = none];
  ultrasound_blank -> Split_treatment [dir = none, minlen = 3];
  Split_treatment -> stereo_blank [dir = none, minlen = 3];

  # (No-op line preserved intentionally)
  exclude_ultrasound_blank

  # Allocation per arm
  ultrasound_blank -> alloc_ultrasound;
  stereo_blank     -> alloc_stero;

  # Post-allocation exclusions and primary outcome
  alloc_ultrasound -> exclude_ultrasound_blank [dir = none, minlen = 1];
  alloc_stero      -> exclude_stereo_blank     [dir = none, minlen = 1];
  exclude_ultrasound          -> exclude_ultrasound_blank [dir = back];
  exclude_stereo_blank        -> exclude_stereo;
  exclude_ultrasound_blank    -> analysis_ultrasound [minlen = 1];
  exclude_stereo_blank        -> analysis_stereo     [minlen = 1];

  # Follow-up 1
  analysis_ultrasound        -> exclude_ultrasound_blank_fu1 [dir = none, minlen = 1];
  analysis_stereo            -> exclude_stereo_blank_fu1     [dir = none, minlen = 1];
  exclude_ultrasound_fu1     -> exclude_ultrasound_blank_fu1 [dir = back];
  exclude_stereo_blank_fu1   -> exclude_stereo_fu1;
  exclude_ultrasound_blank_fu1 -> fu1_ultrasound [minlen = 1];
  exclude_stereo_blank_fu1     -> fu1_stereo     [minlen = 1];

  # Follow-up 2
  fu1_ultrasound            -> exclude_ultrasound_blank_fu2 [dir = none, minlen = 1];
  fu1_stereo                -> exclude_stereo_blank_fu2     [dir = none, minlen = 1];
  exclude_ultrasound_fu2    -> exclude_ultrasound_blank_fu2 [dir = back];
  # exclude_stereo_blank_fu2 -> exclude_stereo_fu2           # No missings FU
  exclude_ultrasound_blank_fu2 -> fu2_ultrasound [minlen = 1];
  exclude_stereo_blank_fu2     -> fu2_stereo     [minlen = 1];

  # ---------------------------------------------------------------------------
  # Rank constraints: keep parallel elements aligned horizontally
  # ---------------------------------------------------------------------------
  { rank = same; exclude_ultrasound; exclude_ultrasound_blank; exclude_stereo_blank; exclude_stereo; }
  { rank = same; exclude_ultrasound_fu1; exclude_ultrasound_blank_fu1; exclude_stereo_blank_fu1; exclude_stereo_fu1; }
  { rank = same; exclude_ultrasound_fu2; exclude_ultrasound_blank_fu2; exclude_stereo_blank_fu2; }
  { rank = same; Excluded Excluded_blank }
  { rank = same; fu1_ultrasound fu1_stereo }
  { rank = same; fu2_ultrasound fu2_stereo }
  { rank = same; ultrasound_blank Split_treatment stereo_blank }

  node [shape = box]
}
"


# PNG export
grViz(graph) %>%
  export_svg %>%                   # SVG text
  charToRaw %>%                    # raw vector
  rsvg_png(paste0(path_study, "/TableFigure/graph.png"))

# SVG export
grViz(graph) %>%
  export_svg() %>%
  charToRaw() %>%
  writeBin(paste0(path_study, "/TableFigure/flowchart.svg"))
