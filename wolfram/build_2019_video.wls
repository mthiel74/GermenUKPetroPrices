#!/usr/bin/env wolframscript
(* build_2019_video.wls — assemble the 30 daily-frame PNGs in
   docs/images/germany_2019_frames/ into an MP4 video.

   Uses ffmpeg via RunProcess (must be on $PATH; install via Homebrew
   if missing: `brew install ffmpeg`).

   Outputs:
     docs/images/germany_2019_animation.mp4   — H.264, ~2 fps, looped intent
*)

SetDirectory @ FileNameDrop[$InputFileName, -2];

framesDir = "docs/images/germany_2019_frames";
outFile   = "docs/images/germany_2019_animation.mp4";

frameFiles = Sort @ FileNames["frame_*.png", framesDir];
Print["[video] frame count: ", Length[frameFiles]];
If[Length[frameFiles] < 5,
  Print["[video] not enough frames; aborting"]; Exit[1]];

(* ffmpeg expects a filename pattern like frame_%08d.png or a concat
   list. Our filenames are frame_YYYYMMDD.png — we feed an explicit
   concat list to avoid renaming. *)
concatFile = CreateFile[];
WriteString[concatFile, "ffconcat version 1.0\n"];
fps = 3;     (* 3 frames per second -> 10 seconds total *)
duration = 1.0/fps;
Do[
  WriteString[concatFile, "file '" <> FileNameJoin[{Directory[], f}] <> "'\n"];
  WriteString[concatFile, "duration " <> ToString[duration] <> "\n"],
  {f, frameFiles}];
(* ffmpeg's concat demuxer needs the last file repeated once *)
WriteString[concatFile, "file '" <> FileNameJoin[{Directory[], Last[frameFiles]}] <> "'\n"];
Close[concatFile];

Print["[video] concat list: ", concatFile];

cmd = {"ffmpeg", "-y",
       "-f", "concat", "-safe", "0", "-i", concatFile,
       "-vf", "scale=900:-2:flags=lanczos,format=yuv420p",
       "-c:v", "libx264", "-preset", "slow", "-crf", "20",
       "-movflags", "+faststart",
       outFile};

Print["[video] running ffmpeg ..."];
result = RunProcess[cmd];
Print["[video] exit code: ", result["ExitCode"]];
If[result["ExitCode"] =!= 0,
  Print["[video] stderr:\n", result["StandardError"]];
  Exit[result["ExitCode"]]];

DeleteFile[concatFile];

Print["[video] wrote ", outFile, " (",
      Round[FileByteCount[outFile]/1024], " KB)"];
