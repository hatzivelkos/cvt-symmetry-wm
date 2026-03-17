(* ::Package:: *)

BeginPackage["CVTPulley`ExportTools`"];

Needs["CVTPulley`TypesAndValidation`"];

ResolveOutputRoot::usage =
  "ResolveOutputRoot[params] returns the validated output root directory.";

ResolveOutputSubdirectory::usage =
  "ResolveOutputSubdirectory[kind, params] returns the subdirectory path \
inside the output root, where kind is one of \"figures\", \"tables\", \
\"raw\", or \"logs\".";

EnsureDirectory::usage =
  "EnsureDirectory[dir] creates dir if needed and returns the directory path, \
or Failure if creation is unsuccessful.";

BuildExportBaseName::usage =
  "BuildExportBaseName[params] returns the base export name stored in params.";

BuildExportFileName::usage =
  "BuildExportFileName[stem, ext, params] builds a standard export filename \
using the export base name from params.";

ExportRawWL::usage =
  "ExportRawWL[data, stem, params] exports data to output/raw as a .wl file.";

ExportTableCSV::usage =
  "ExportTableCSV[data, stem, params] exports tabular data to output/tables \
as a .csv file.";

ExportFigure::usage =
  "ExportFigure[g, stem, ext, params] exports a graphic to output/figures, \
where ext is one of \"png\", \"pdf\", or \"svg\".";

ExportLogText::usage =
  "ExportLogText[text, stem, params] exports plain text to output/logs as a \
.txt file.";

ExportResultQ::usage =
  "ExportResultQ[expr] returns True if expr is an Association representing a \
successful export result.";

Begin["`Private`"];

(* ========================================= *)
(* Messages                                  *)
(* ========================================= *)

ResolveOutputRoot::badparams =
  "Input must be a validated parameter Association.";

ResolveOutputSubdirectory::badkind =
  "Output kind must be one of \"figures\", \"tables\", \"raw\", or \"logs\". Got `1`.";

EnsureDirectory::fail =
  "Failed to create or access directory `1`.";

BuildExportFileName::badext =
  "File extension must be a nonempty string. Got `1`.";

ExportRawWL::badparams =
  "Input must be a validated parameter Association.";

ExportTableCSV::badparams =
  "Input must be a validated parameter Association.";

ExportFigure::badparams =
  "Input must be a validated parameter Association.";

ExportFigure::badext =
  "Graphic export format must be one of \"png\", \"pdf\", or \"svg\". Got `1`.";

ExportLogText::badparams =
  "Input must be a validated parameter Association.";

(* ========================================= *)
(* Helpers                                   *)
(* ========================================= *)

failure[tag_, msg_, assoc_: <||>] :=
  Failure[tag, Join[<|"MessageTemplate" -> msg|>, assoc]];

nonEmptyStringQ[x_] := StringQ[x] && StringLength[StringTrim[x]] > 0;

validOutputKindQ[kind_] := MemberQ[{"figures", "tables", "raw", "logs"}, kind];

ExportResultQ[expr_] :=
  AssociationQ[expr] &&
   KeyExistsQ[expr, "Path"] &&
   KeyExistsQ[expr, "Success"] &&
   TrueQ[expr["Success"]];

(* ========================================= *)
(* Directory handling                        *)
(* ========================================= *)

ResolveOutputRoot[params_Association] := Module[
  {validated, dir},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[ResolveOutputRoot::badparams];
    Return[validated];
  ];

  dir = validated["outputDirectory"];
  dir
];

ResolveOutputRoot[_] := (
  Message[ResolveOutputRoot::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

ResolveOutputSubdirectory[kind_, params_Association] := Module[
  {root},
  If[!validOutputKindQ[kind],
    Message[ResolveOutputSubdirectory::badkind, kind];
    Return @ failure[
      "InvalidOutputKind",
      "Output kind must be one of \"figures\", \"tables\", \"raw\", or \"logs\".",
      <|"Kind" -> kind|>
    ];
  ];

  root = ResolveOutputRoot[params];
  If[FailureQ[root], Return[root]];

  FileNameJoin[{root, kind}]
];

ResolveOutputSubdirectory[kind_, _] := (
  Message[ResolveOutputSubdirectory::badkind, kind];
  failure["InvalidArguments", "ResolveOutputSubdirectory expects (kind_String, params_Association)."]
);

EnsureDirectory[dir_?nonEmptyStringQ] := Module[{},
  Quiet@CreateDirectory[dir, CreateIntermediateDirectories -> True];
  If[DirectoryQ[dir],
    dir,
    Message[EnsureDirectory::fail, dir];
    failure[
      "DirectoryCreationFailed",
      "Failed to create or access directory.",
      <|"Directory" -> dir|>
    ]
  ]
];

EnsureDirectory[dir_] := failure[
  "InvalidDirectory",
  "Directory must be a nonempty string.",
  <|"Directory" -> dir|>
];

(* ========================================= *)
(* File naming                               *)
(* ========================================= *)

BuildExportBaseName[params_Association] := Module[
  {validated},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated], Return[validated]];
  validated["exportBaseName"]
];

BuildExportFileName[stem_?nonEmptyStringQ, ext_, params_Association] := Module[
  {validated, baseName, cleanExt},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated], Return[validated]];

  If[!nonEmptyStringQ[ext],
    Message[BuildExportFileName::badext, ext];
    Return @ failure[
      "InvalidExtension",
      "File extension must be a nonempty string.",
      <|"Extension" -> ext|>
    ];
  ];

  baseName = validated["exportBaseName"];
  cleanExt = ToLowerCase @ StringTrim[ext, "."];

  baseName <> "_" <> stem <> "." <> cleanExt
];

(* ========================================= *)
(* Core export wrappers                      *)
(* ========================================= *)

ExportRawWL[data_, stem_?nonEmptyStringQ, params_Association] := Module[
  {dir, ensured, fileName, path, res},
  dir = ResolveOutputSubdirectory["raw", params];
  If[FailureQ[dir],
    Message[ExportRawWL::badparams];
    Return[dir];
  ];

  ensured = EnsureDirectory[dir];
  If[FailureQ[ensured], Return[ensured]];

  fileName = BuildExportFileName[stem, "wl", params];
  If[FailureQ[fileName], Return[fileName]];

  path = FileNameJoin[{ensured, fileName}];
  res = Export[path, data, "WL"];

  <|
    "Success" -> True,
    "Type" -> "WL",
    "Kind" -> "raw",
    "Path" -> path,
    "Returned" -> res
  |>
];

ExportRawWL[_, _, _] := (
  Message[ExportRawWL::badparams];
  failure["InvalidArguments", "ExportRawWL expects (data, stem_String, params_Association)."]
);

ExportTableCSV[data_, stem_?nonEmptyStringQ, params_Association] := Module[
  {dir, ensured, fileName, path, res},
  dir = ResolveOutputSubdirectory["tables", params];
  If[FailureQ[dir],
    Message[ExportTableCSV::badparams];
    Return[dir];
  ];

  ensured = EnsureDirectory[dir];
  If[FailureQ[ensured], Return[ensured]];

  fileName = BuildExportFileName[stem, "csv", params];
  If[FailureQ[fileName], Return[fileName]];

  path = FileNameJoin[{ensured, fileName}];
  res = Export[path, data, "CSV"];

  <|
    "Success" -> True,
    "Type" -> "CSV",
    "Kind" -> "tables",
    "Path" -> path,
    "Returned" -> res
  |>
];

ExportTableCSV[_, _, _] := (
  Message[ExportTableCSV::badparams];
  failure["InvalidArguments", "ExportTableCSV expects (data, stem_String, params_Association)."]
);

ExportFigure[g_, stem_?nonEmptyStringQ, ext_, params_Association] := Module[
  {dir, ensured, cleanExt, fileName, path, res},
  dir = ResolveOutputSubdirectory["figures", params];
  If[FailureQ[dir],
    Message[ExportFigure::badparams];
    Return[dir];
  ];

  cleanExt = ToLowerCase @ StringTrim[ext, "."];
  If[!MemberQ[{"png", "pdf", "svg"}, cleanExt],
    Message[ExportFigure::badext, ext];
    Return @ failure[
      "InvalidFigureExtension",
      "Graphic export format must be one of \"png\", \"pdf\", or \"svg\".",
      <|"Extension" -> ext|>
    ];
  ];

  ensured = EnsureDirectory[dir];
  If[FailureQ[ensured], Return[ensured]];

  fileName = BuildExportFileName[stem, cleanExt, params];
  If[FailureQ[fileName], Return[fileName]];

  path = FileNameJoin[{ensured, fileName}];
  res = Export[path, g];

  <|
    "Success" -> True,
    "Type" -> ToUpperCase[cleanExt],
    "Kind" -> "figures",
    "Path" -> path,
    "Returned" -> res
  |>
];

ExportFigure[_, _, _, _] := (
  Message[ExportFigure::badparams];
  failure["InvalidArguments", "ExportFigure expects (graphic, stem_String, ext_String, params_Association)."]
);

ExportLogText[text_?nonEmptyStringQ, stem_?nonEmptyStringQ, params_Association] := Module[
  {dir, ensured, fileName, path, res},
  dir = ResolveOutputSubdirectory["logs", params];
  If[FailureQ[dir],
    Message[ExportLogText::badparams];
    Return[dir];
  ];

  ensured = EnsureDirectory[dir];
  If[FailureQ[ensured], Return[ensured]];

  fileName = BuildExportFileName[stem, "txt", params];
  If[FailureQ[fileName], Return[fileName]];

  path = FileNameJoin[{ensured, fileName}];
  res = Export[path, text, "Text"];

  <|
    "Success" -> True,
    "Type" -> "TXT",
    "Kind" -> "logs",
    "Path" -> path,
    "Returned" -> res
  |>
];

ExportLogText[_, _, _] := (
  Message[ExportLogText::badparams];
  failure["InvalidArguments", "ExportLogText expects (text_String, stem_String, params_Association)."]
);

End[];
EndPackage[];