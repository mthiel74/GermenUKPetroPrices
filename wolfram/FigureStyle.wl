(* ::Package:: *)

(* FigureStyle.wl — common figure styling constants for the project.
   Used by every analysis .wls so that title/axis/tick sizes are
   consistent and readable in the assembled notebook. *)

BeginPackage["FigureStyle`"];

TitleStyle::usage = "TitleStyle[s] returns a Style for plot titles.";
SubtitleStyle::usage = "SubtitleStyle[s] for subtitles.";
LabelStyle::usage = "LabelStyle[s] for axis labels.";
TickStyle::usage = "TickStyle[s] for tick labels.";
LegendStyle::usage = "LegendStyle[s] for legend text.";

Teal::usage   = "Project teal RGB.";
Ember::usage  = "Project ember RGB.";
Slate::usage  = "Project slate RGB.";
Crimson::usage = "Project crimson RGB.";
Forest::usage = "Project forest RGB.";

DefaultPlotOptions::usage =
  "DefaultPlotOptions returns a list of plotting options to pass to \
ListPlot / ListLinePlot / DateListPlot for consistent styling.";

CommonEvents::usage =
  "CommonEvents is a list of {DateObject, label, color} triples for \
major oil-price events used by long-time-series figures.";

Begin["`Private`"];

Teal    = RGBColor[0.18, 0.50, 0.62];
Ember   = RGBColor[0.91, 0.45, 0.10];
Slate   = RGBColor[0.30, 0.34, 0.42];
Crimson = RGBColor[0.78, 0.10, 0.18];
Forest  = RGBColor[0.20, 0.55, 0.30];

TitleStyle[s_]    := Style[s, 18, Bold,   GrayLevel[0.1]];
SubtitleStyle[s_] := Style[s, 14, Italic, GrayLevel[0.3]];
LabelStyle[s_]    := Style[s, 14, Bold,   GrayLevel[0.15]];
TickStyle[s_]     := Style[s, 12,         GrayLevel[0.2]];
LegendStyle[s_]   := Style[s, 12,         GrayLevel[0.15]];

DefaultPlotOptions = {
  Frame -> True,
  FrameStyle -> Directive[GrayLevel[0.2], Thickness[0.0015]],
  GridLines -> Automatic,
  GridLinesStyle -> Directive[GrayLevel[0.85], Dashed, Thickness[0.0005]],
  LabelStyle -> Directive[12, GrayLevel[0.15]],
  BaseStyle -> {FontFamily -> "Helvetica", FontSize -> 12},
  ImageSize -> 1100,
  PlotTheme -> "Detailed"
};

CommonEvents = {
  {{1990, 8, 2},  "Iraq invades Kuwait", Crimson},
  {{1998, 12, 1}, "Asia crisis trough",  Slate},
  {{2008, 7, 11}, "GFC peak",            Crimson},
  {{2014, 11, 27}, "OPEC + shale glut",  Slate},
  {{2020, 4, 20}, "COVID demand crash",  Slate},
  {{2022, 3, 1},  "Russia invades Ukraine", Crimson},
  {{2026, 4, 1},  "12-Uhr-Regel takes effect", Forest},
  {{2026, 4, 15}, "Iran war Brent peak", Crimson}
};

End[];
EndPackage[];
