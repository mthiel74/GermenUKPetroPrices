#!/usr/bin/env wolframscript
(* germany_station_network.wls — country-wide station-network map
   using the full ~14,500-station master list from the gustavz
   Tankerkönig archive mirror.

   Output:
     docs/images/germany_snapshot.png    (overwrites the old partial-
     fetch snapshot used in notebook §2.2)
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

stations = Normal @ Import["data/raw/gustavz/stations.csv",
                           "Dataset", "HeaderLines" -> 1];
stations = Select[stations,
  NumberQ[#latitude] && NumberQ[#longitude] &&
  47 < #latitude < 56 && 5 < #longitude < 16 &];
Print["[net] valid stations: ", Length[stations]];

map = GeoGraphics[
  {FaceForm[Lighter[Gray, 0.94]], EdgeForm[GrayLevel[0.4]],
   CountryData["Germany", "Polygon"],
   Black, PointSize[0.002], Opacity[0.6],
   Point @ GeoPosition @ ({#latitude, #longitude} & /@ stations)},
  GeoRange -> Entity["Country", "Germany"],
  GeoProjection -> "Mercator",
  GeoBackground -> White,
  PlotLabel -> Style[
    StringTemplate[
      "German fuel-station network: `1` stations (Tankerkonig MTS-K master list)"][
      Length[stations]],
    18, Bold, GrayLevel[0.1]],
  ImageSize -> 1100];

Export["docs/images/germany_snapshot.png", map, ImageResolution -> 144];
Print["[net] wrote docs/images/germany_snapshot.png (",
      Round[FileByteCount["docs/images/germany_snapshot.png"]/1024], " KB)"];
