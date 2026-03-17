(* ::Package:: *)

BeginPackage["CVTPulley`ErrorAnalysis`"];

Needs["CVTPulley`TypesAndValidation`"];
Needs["CVTPulley`BeltLength`"];
Needs["CVTPulley`ImplicitMap`"];

ResidualOnPair::usage =
  "ResidualOnPair[{r, R}, params] returns the belt-length residual for the pair {r,R}.";

ResidualsOnPairs::usage =
  "ResidualsOnPairs[pairs, params] returns the list of belt-length residuals for pairs {{r1,R1},{r2,R2},...}.";

ResidualSummary::usage =
  "ResidualSummary[residuals] returns an Association with basic error statistics.";

ImplicitMapResiduals::usage =
  "ImplicitMapResiduals[params] computes residuals on the default implicit-map pairs.";

ImplicitMapResidualData::usage =
  "ImplicitMapResidualData[params] returns pairs, residuals, and a summary for the default implicit map.";

ErrorSummaryQ::usage =
  "ErrorSummaryQ[expr] returns True if expr is an Association representing a residual summary.";

Begin["`Private`"];

(* ========================================= *)
(* Messages                                  *)
(* ========================================= *)

ResidualOnPair::badpair =
  "Input must be a numeric pair {r,R}. Got `1`.";

ResidualOnPair::badparams =
  "Input must be a validated parameter Association.";

ResidualsOnPairs::badpairs =
  "Input must be a list of numeric pairs {{r1,R1},{r2,R2},...}.";

ResidualSummary::badlist =
  "Input must be a nonempty numeric list.";

ImplicitMapResiduals::badparams =
  "Input must be a validated parameter Association.";

ImplicitMapResidualData::badparams =
  "Input must be a validated parameter Association.";

(* ========================================= *)
(* Helpers                                   *)
(* ========================================= *)

failure[tag_, msg_, assoc_: <||>] :=
  Failure[tag, Join[<|"MessageTemplate" -> msg|>, assoc]];

numericPairQ[x_] := MatchQ[x, {_?NumericQ, _?NumericQ}];
numericPairListQ[x_] := MatchQ[x, {(_List) ..}] && AllTrue[x, numericPairQ];
numericNonemptyListQ[x_] := ListQ[x] && x =!= {} && VectorQ[x, NumericQ];

ErrorSummaryQ[expr_] :=
  AssociationQ[expr] &&
   KeyExistsQ[expr, "Count"] &&
   KeyExistsQ[expr, "MaxAbsResidual"] &&
   KeyExistsQ[expr, "MeanAbsResidual"];

(* ========================================= *)
(* Pairwise residuals                        *)
(* ========================================= *)

ResidualOnPair[pair_, params_Association] /; !numericPairQ[pair] := (
  Message[ResidualOnPair::badpair, pair];
  failure[
    "InvalidPair",
    "Input must be a numeric pair {r,R}.",
    <|"Pair" -> pair|>
  ]
);

ResidualOnPair[{r_?NumericQ, R_?NumericQ}, params_Association] := Module[
  {validated},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[ResidualOnPair::badparams];
    Return[validated];
  ];

  CVTPulley`BeltLength`BeltResidualFromParameters[r, R, validated]
];

ResidualOnPair[_, _] := (
  Message[ResidualOnPair::badparams];
  failure["InvalidArguments", "ResidualOnPair expects ({r,R}, params_Association)."]
);

ResidualsOnPairs[pairs_, params_Association] := Module[
  {validated},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[ResidualsOnPairs::badpairs];
    Return[validated];
  ];

  If[!numericPairListQ[pairs],
    Message[ResidualsOnPairs::badpairs];
    Return @ failure[
      "InvalidPairList",
      "Input must be a list of numeric pairs.",
      <|"Input" -> pairs|>
    ];
  ];

  ResidualOnPair[#, validated] & /@ pairs
];

ResidualsOnPairs[_, _] := (
  Message[ResidualsOnPairs::badpairs];
  failure["InvalidArguments", "ResidualsOnPairs expects (pairs_List, params_Association)."]
);

(* ========================================= *)
(* Residual summaries                        *)
(* ========================================= *)

ResidualSummary[residuals_] /; !numericNonemptyListQ[residuals] := (
  Message[ResidualSummary::badlist];
  failure[
    "InvalidResidualList",
    "Input must be a nonempty numeric list.",
    <|"Input" -> residuals|>
  ]
);

ResidualSummary[residuals_List] := Module[
  {abs},
  abs = Abs[residuals];

  <|
    "Count" -> Length[residuals],
    "MinResidual" -> Min[residuals],
    "MaxResidual" -> Max[residuals],
    "MaxAbsResidual" -> Max[abs],
    "MeanResidual" -> Mean[residuals],
    "MeanAbsResidual" -> Mean[abs]
  |>
];

(* ========================================= *)
(* Default implicit-map diagnostics          *)
(* ========================================= *)

ImplicitMapResiduals[params_Association] := Module[
  {validated, data},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[ImplicitMapResiduals::badparams];
    Return[validated];
  ];

  data = CVTPulley`ImplicitMap`ImplicitMapData[validated];
  If[FailureQ[data], Return[data]];

  ResidualsOnPairs[data["Pairs"], validated]
];

ImplicitMapResiduals[_] := (
  Message[ImplicitMapResiduals::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

ImplicitMapResidualData[params_Association] := Module[
  {validated, data, residuals, summary},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[ImplicitMapResidualData::badparams];
    Return[validated];
  ];

  data = CVTPulley`ImplicitMap`ImplicitMapData[validated];
  If[FailureQ[data], Return[data]];

  residuals = ResidualsOnPairs[data["Pairs"], validated];
  If[FailureQ[residuals], Return[residuals]];

  summary = ResidualSummary[residuals];
  If[FailureQ[summary], Return[summary]];

  <|
    "Pairs" -> data["Pairs"],
    "Residuals" -> residuals,
    "Summary" -> summary
  |>
];

ImplicitMapResidualData[_] := (
  Message[ImplicitMapResidualData::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

End[];
EndPackage[];