(* ::Package:: *)

BeginPackage["CVTPulley`ImplicitMap`"];

Needs["CVTPulley`TypesAndValidation`"];
Needs["CVTPulley`Geometry`"];
Needs["CVTPulley`BeltLength`"];
Needs["CVTPulley`RootFinding`"];

ImplicitRadius::usage =
  "ImplicitRadius[r, params] computes R = g(r) from the implicit equation \
BeltLength[r,R,d] == L0.";

ImplicitRadiusWithBracket::usage =
  "ImplicitRadiusWithBracket[r, bracket, params] computes R = g(r) using the \
specified bracket for R.";

ImplicitRadiusValue::usage =
  "ImplicitRadiusValue[r, params] returns only the root value R, without the \
full diagnostic Association.";

ImplicitRadiusOnGrid::usage =
  "ImplicitRadiusOnGrid[rList, params] computes the implicit radius map on a \
list of r-values and returns a list of result Associations.";

ImplicitRadiusValuesOnGrid::usage =
  "ImplicitRadiusValuesOnGrid[rList, params] returns only the root values \
corresponding to the input list of r-values.";

UniformGridRadii::usage =
  "UniformGridRadii[params] returns the uniform grid {r_i} on [rMin, rMax] \
with nGrid subintervals.";

ImplicitMapData::usage =
  "ImplicitMapData[params] builds the implicit map data on the default \
uniform grid determined by params.";

ImplicitMapPairs::usage =
  "ImplicitMapPairs[params] returns {{r1,R1},{r2,R2},...} on the default grid.";

InitialLinearRadiusProfile::usage =
  "InitialLinearRadiusProfile[t, params] returns the linear initial radius \
profile r(t) from rMin to rMax.";

InitialLinearRadiusProfileOnGrid::usage =
  "InitialLinearRadiusProfileOnGrid[params] returns the values of the linear \
initial profile r(t_i) on the default grid t_i = i/nGrid.";

Begin["`Private`"];

(* ========================================= *)
(* Messages                                  *)
(* ========================================= *)

ImplicitRadius::badparams =
  "Input must be a validated parameter Association.";

ImplicitRadius::badr =
  "Input radius r = `1` is outside the admissible interval [`2`,`3`].";

ImplicitRadius::nobracket =
  "Failed to construct a valid bracket for the implicit equation at r = `1`.";

ImplicitRadiusWithBracket::badbracket =
  "The supplied bracket must be a numeric pair {RLeft,RRight} with RLeft < RRight.";

ImplicitRadiusOnGrid::badlist =
  "Input must be a numeric list of radii.";

ImplicitMapData::badparams =
  "Input must be a validated parameter Association.";

UniformGridRadii::badparams =
  "Input must be a validated parameter Association.";

InitialLinearRadiusProfile::badparams =
  "Input must be a validated parameter Association.";

InitialLinearRadiusProfile::badt =
  "Parameter t = `1` must lie in the interval [0,1].";

(* ========================================= *)
(* Helpers                                   *)
(* ========================================= *)

numericListQ[x_] := VectorQ[x, NumericQ];

failure[tag_, msg_, assoc_: <||>] :=
  Failure[tag, Join[<|"MessageTemplate" -> msg|>, assoc]];

(* ========================================= *)
(* Initial linear profile                    *)
(* ========================================= *)

InitialLinearRadiusProfile[t_?NumericQ, params_Association] := Module[
  {validated, rMin, rMax},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[InitialLinearRadiusProfile::badparams];
    Return[validated];
  ];

  If[!(0 <= t <= 1),
    Message[InitialLinearRadiusProfile::badt, t];
    Return @ failure[
      "InvalidParameterT",
      "Parameter t must lie in [0,1].",
      <|"t" -> t|>
    ];
  ];

  rMin = validated["rMin"];
  rMax = validated["rMax"];

  (1 - t) rMin + t rMax
];

InitialLinearRadiusProfile[_, _] := (
  Message[InitialLinearRadiusProfile::badparams];
  failure["InvalidArguments", "InitialLinearRadiusProfile expects (t, params_Association)."]
);

InitialLinearRadiusProfileOnGrid[params_Association] := Module[
  {validated, n, tGrid},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated], Return[validated]];

  n = validated["nGrid"];
  tGrid = N[Range[0, n]/n];

  InitialLinearRadiusProfile[#, validated] & /@ tGrid
];

(* ========================================= *)
(* Uniform grid in r                         *)
(* ========================================= *)

UniformGridRadii[params_Association] := Module[
  {validated, rMin, rMax, n},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[UniformGridRadii::badparams];
    Return[validated];
  ];

  rMin = validated["rMin"];
  rMax = validated["rMax"];
  n = validated["nGrid"];

  N[Range[0, n] (rMax - rMin)/n + rMin]
];

UniformGridRadii[_] := (
  Message[UniformGridRadii::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

(* ========================================= *)
(* Pointwise implicit radius                 *)
(* ========================================= *)

Options[ImplicitRadius] = {
  "Tolerance" -> Automatic,
  "MaxIterations" -> Automatic,
  "WorkingPrecision" -> Automatic,
  "StoreHistory" -> False
};

ImplicitRadius[r_?NumericQ, params_Association, OptionsPattern[]] := Module[
  {
    validated, rMin, rMax,
    left, right, tol, maxIter, wp, storeHistory, result
  },

  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[ImplicitRadius::badparams];
    Return[validated];
  ];

  rMin = validated["rMin"];
  rMax = validated["rMax"];

  If[!(rMin <= r <= rMax),
    Message[ImplicitRadius::badr, r, rMin, rMax];
    Return @ failure[
      "RadiusOutOfRange",
      "Input radius lies outside the admissible interval.",
      <|"r" -> r, "rMin" -> rMin, "rMax" -> rMax|>
    ];
  ];

  left = rMin;
  right = rMax;

  If[!(NumericQ[left] && NumericQ[right] && left < right),
    Message[ImplicitRadius::nobracket, r];
    Return @ failure[
      "InvalidImplicitBracket",
      "Failed to construct a valid implicit bracket.",
      <|"r" -> r, "Left" -> left, "Right" -> right|>
    ];
  ];

  tol = Replace[OptionValue["Tolerance"], Automatic -> validated["epsBis"]];
  maxIter = Replace[OptionValue["MaxIterations"], Automatic -> validated["maxBisIter"]];
  wp = Replace[OptionValue["WorkingPrecision"], Automatic -> validated["workingPrecision"]];
  storeHistory = TrueQ[OptionValue["StoreHistory"]];

  result = CVTPulley`RootFinding`FindRadiusRoot[
    r,
    {left, right},
    validated,
    "Tolerance" -> tol,
    "MaxIterations" -> maxIter,
    "WorkingPrecision" -> wp,
    "StoreHistory" -> storeHistory
  ];

  If[FailureQ[result], Return[result]];

  Join[
    result,
    <|
      "InputRadius" -> r,
      "ConstructedBracket" -> {left, right}
    |>
  ]
];

ImplicitRadius[___] := (
  Message[ImplicitRadius::badparams];
  failure["InvalidArguments", "ImplicitRadius expects (r_?NumericQ, params_Association, ...)."]
);

(* ========================================= *)
(* Pointwise implicit radius with user bracket *)
(* ========================================= *)

Options[ImplicitRadiusWithBracket] = Options[ImplicitRadius];

ImplicitRadiusWithBracket[
  r_?NumericQ,
  bracket_,
  params_Association,
  OptionsPattern[]
] := Module[
  {validated, tol, maxIter, wp, storeHistory, result},

  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated], Return[validated]];

  If[!MatchQ[bracket, {_?NumericQ, _?NumericQ}] || !(bracket[[1]] < bracket[[2]]),
    Message[ImplicitRadiusWithBracket::badbracket];
    Return @ failure[
      "InvalidBracket",
      "The supplied bracket must be a numeric pair {RLeft,RRight} with RLeft < RRight.",
      <|"Bracket" -> bracket|>
    ];
  ];

  tol = Replace[OptionValue["Tolerance"], Automatic -> validated["epsBis"]];
  maxIter = Replace[OptionValue["MaxIterations"], Automatic -> validated["maxBisIter"]];
  wp = Replace[OptionValue["WorkingPrecision"], Automatic -> validated["workingPrecision"]];
  storeHistory = TrueQ[OptionValue["StoreHistory"]];

  result = CVTPulley`RootFinding`FindRadiusRoot[
    r,
    bracket,
    validated,
    "Tolerance" -> tol,
    "MaxIterations" -> maxIter,
    "WorkingPrecision" -> wp,
    "StoreHistory" -> storeHistory
  ];

  If[FailureQ[result], Return[result]];

  Join[
    result,
    <|
      "InputRadius" -> r,
      "ConstructedBracket" -> bracket
    |>
  ]
];

(* ========================================= *)
(* Value-only wrappers                       *)
(* ========================================= *)

ImplicitRadiusValue[
  r_?NumericQ,
  params_Association,
  opts : OptionsPattern[ImplicitRadius]
] := Module[{res},
  res = ImplicitRadius[r, params, opts];
  If[FailureQ[res], res, res["Root"]]
];

(* ========================================= *)
(* Grid evaluation                           *)
(* ========================================= *)

Options[ImplicitRadiusOnGrid] = Options[ImplicitRadius];

ImplicitRadiusOnGrid[rList_, params_Association, OptionsPattern[]] := Module[
  {validated},
  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated], Return[validated]];

  If[!numericListQ[rList],
    Message[ImplicitRadiusOnGrid::badlist];
    Return @ failure[
      "InvalidRadiusList",
      "Input must be a numeric list of radii.",
      <|"Input" -> rList|>
    ];
  ];

  ImplicitRadius[
      #,
      validated,
      "Tolerance" -> OptionValue["Tolerance"],
      "MaxIterations" -> OptionValue["MaxIterations"],
      "WorkingPrecision" -> OptionValue["WorkingPrecision"],
      "StoreHistory" -> OptionValue["StoreHistory"]
    ] & /@ rList
];

ImplicitRadiusValuesOnGrid[
  rList_,
  params_Association,
  opts : OptionsPattern[ImplicitRadiusOnGrid]
] := Module[{res},
  res = ImplicitRadiusOnGrid[rList, params, opts];
  If[FailureQ[res],
    res,
    If[AnyTrue[res, FailureQ],
      res,
      res[[All, "Root"]]
    ]
  ]
];

(* ========================================= *)
(* Full default-map data                     *)
(* ========================================= *)

Options[ImplicitMapData] = Options[ImplicitRadiusOnGrid];

ImplicitMapData[params_Association, OptionsPattern[]] := Module[
  {validated, n, tGrid, rGrid, rootResults, anyFailQ},

  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[ImplicitMapData::badparams];
    Return[validated];
  ];

  n = validated["nGrid"];
  tGrid = N[Range[0, n]/n];
  rGrid = InitialLinearRadiusProfileOnGrid[validated];
  If[FailureQ[rGrid], Return[rGrid]];

  rootResults = ImplicitRadiusOnGrid[
    rGrid,
    validated,
    "Tolerance" -> OptionValue["Tolerance"],
    "MaxIterations" -> OptionValue["MaxIterations"],
    "WorkingPrecision" -> OptionValue["WorkingPrecision"],
    "StoreHistory" -> OptionValue["StoreHistory"]
  ];

  If[FailureQ[rootResults], Return[rootResults]];

  anyFailQ = AnyTrue[rootResults, FailureQ];
  If[anyFailQ,
    Return @ failure[
      "ImplicitMapComputationFailed",
      "At least one grid point failed during implicit-map computation.",
      <|"Results" -> rootResults|>
    ]
  ];

  <|
    "tGrid" -> tGrid,
    "rGrid" -> rGrid,
    "RGrid" -> rootResults[[All, "Root"]],
    "RootResults" -> rootResults,
    "Pairs" -> Transpose[{rGrid, rootResults[[All, "Root"]]}]
  |>
];

ImplicitMapData[_] := (
  Message[ImplicitMapData::badparams];
  failure["InvalidParameters", "Input must be a validated parameter Association."]
);

ImplicitMapPairs[
  params_Association,
  opts : OptionsPattern[ImplicitMapData]
] := Module[{data},
  data = ImplicitMapData[params, opts];
  If[FailureQ[data], data, data["Pairs"]]
];

End[];
EndPackage[];