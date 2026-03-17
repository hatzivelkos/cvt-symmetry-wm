(* ::Package:: *)

BeginPackage["CVTPulley`Symmetrization`"];

Needs["CVTPulley`TypesAndValidation`"];
Needs["CVTPulley`ImplicitMap`"];

CrossMidpointLeft::usage =
  "CrossMidpointLeft[rLeft, RRight] returns (rLeft + RRight)/2.";

CrossMidpointRight::usage =
  "CrossMidpointRight[rRight, RLeft] returns (rRight + RLeft)/2.";

SymmetryError::usage =
  "SymmetryError[rLeft, RRight] returns Abs[RRight - rLeft].";

SymmetryErrorPair::usage =
  "SymmetryErrorPair[rLeft, RLeft, rRight, RRight] returns an Association \
with both cross-errors and their maximum.";

InitialPairState::usage =
  "InitialPairState[i, params] constructs the initial state for the pair \
(i, nGrid-i) using the linear profile and the implicit map.";

SymmetrizationStep::usage =
  "SymmetrizationStep[state, params] performs one pairwise symmetrization \
update according to the article.";

IterateSymmetricPair::usage =
  "IterateSymmetricPair[i, params] iterates the symmetric pair (i, nGrid-i) \
until the prescribed tolerance is reached.";

IterateMidpoint::usage =
  "IterateMidpoint[params] iterates the midpoint i = nGrid/2 separately.";

SymmetrizationData::usage =
  "SymmetrizationData[params] runs the pairwise symmetrization procedure \
across the full grid.";

PairIterationTable::usage =
  "PairIterationTable[result] extracts the stored iteration history.";

BuildSymmetricDiscreteProfiles::usage =
  "BuildSymmetricDiscreteProfiles[symData] builds the final discrete profiles \
r(t_i) and R(t_i) from SymmetrizationData output.";

Begin["`Private`"];

(* ========================================= *)
(* Messages                                  *)
(* ========================================= *)

InitialPairState::badparams =
  "Input must be a validated parameter Association.";

InitialPairState::badindex =
  "Index i = `1` must satisfy 0 <= i <= Floor[(nGrid-1)/2].";

SymmetrizationStep::badstate =
  "Input state is not a valid pair-state Association.";

IterateSymmetricPair::badparams =
  "Input must be a validated parameter Association.";

IterateSymmetricPair::badindex =
  "Index i = `1` must satisfy 0 <= i <= Floor[(nGrid-1)/2].";

IterateMidpoint::badparams =
  "Input must be a validated parameter Association.";

IterateMidpoint::badgrid =
  "Midpoint iteration requires even nGrid.";

SymmetrizationData::badparams =
  "Input must be a validated parameter Association.";

PairIterationTable::badinput =
  "Input must be a successful result Association returned by IterateSymmetricPair or IterateMidpoint.";

BuildSymmetricDiscreteProfiles::badinput =
  "Input must be a successful Association returned by SymmetrizationData.";

(* ========================================= *)
(* Helpers                                   *)
(* ========================================= *)

failure[tag_, msg_, assoc_: <||>] :=
  Failure[tag, Join[<|"MessageTemplate" -> msg|>, assoc]];

pairStateQ[state_] :=
  AssociationQ[state] &&
   KeyExistsQ[state, "IndexLeft"] &&
   KeyExistsQ[state, "IndexRight"] &&
   KeyExistsQ[state, "rLeft"] &&
   KeyExistsQ[state, "RLeft"] &&
   KeyExistsQ[state, "rRight"] &&
   KeyExistsQ[state, "RRight"] &&
   KeyExistsQ[state, "Iteration"];

(* ========================================= *)
(* Elementary update quantities              *)
(* ========================================= *)

CrossMidpointLeft[rLeft_?NumericQ, RRight_?NumericQ] := (rLeft + RRight)/2;

CrossMidpointRight[rRight_?NumericQ, RLeft_?NumericQ] := (rRight + RLeft)/2;

SymmetryError[rLeft_?NumericQ, RRight_?NumericQ] := Abs[RRight - rLeft];

SymmetryErrorPair[
  rLeft_?NumericQ, RLeft_?NumericQ,
  rRight_?NumericQ, RRight_?NumericQ
] :=
  <|
    "LeftCrossError" -> Abs[RRight - rLeft],
    "RightCrossError" -> Abs[RLeft - rRight],
    "MaxCrossError" -> Max[Abs[RRight - rLeft], Abs[RLeft - rRight]]
  |>;

(* ========================================= *)
(* Initial state for one pair                *)
(* ========================================= *)

InitialPairState[i_Integer, params_Association] := Module[
  {
    validated, n, j, tLeft, tRight,
    rLeft, rRight, RLeftRes, RRightRes, errs
  },

  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[InitialPairState::badparams];
    Return[validated];
  ];

  n = validated["nGrid"];

  If[!(0 <= i <= Floor[(n - 1)/2]),
    Message[InitialPairState::badindex, i];
    Return @ failure[
      "InvalidPairIndex",
      "Index i is outside the valid pair range.",
      <|"i" -> i, "nGrid" -> n|>
    ];
  ];

  j = n - i;
  tLeft = N[i/n];
  tRight = N[j/n];

  rLeft = CVTPulley`ImplicitMap`InitialLinearRadiusProfile[tLeft, validated];
  rRight = CVTPulley`ImplicitMap`InitialLinearRadiusProfile[tRight, validated];

  RLeftRes = CVTPulley`ImplicitMap`ImplicitRadius[rLeft, validated, "StoreHistory" -> False];
  If[FailureQ[RLeftRes], Return[RLeftRes]];

  RRightRes = CVTPulley`ImplicitMap`ImplicitRadius[rRight, validated, "StoreHistory" -> False];
  If[FailureQ[RRightRes], Return[RRightRes]];

  errs = SymmetryErrorPair[rLeft, RLeftRes["Root"], rRight, RRightRes["Root"]];

  <|
    "Iteration" -> 1,
    "IndexLeft" -> i,
    "IndexRight" -> j,
    "tLeft" -> tLeft,
    "tRight" -> tRight,
    "rLeft" -> rLeft,
    "RLeft" -> RLeftRes["Root"],
    "rRight" -> rRight,
    "RRight" -> RRightRes["Root"],
    "LeftImplicitResult" -> RLeftRes,
    "RightImplicitResult" -> RRightRes,
    "LeftCrossError" -> errs["LeftCrossError"],
    "RightCrossError" -> errs["RightCrossError"],
    "MaxCrossError" -> errs["MaxCrossError"]
  |>
];

(* ========================================= *)
(* One symmetrization step                   *)
(* ========================================= *)

Options[SymmetrizationStep] = {
  "ImplicitStoreHistory" -> False
};

SymmetrizationStep[state_, params_Association, OptionsPattern[]] := Module[
  {
    validated, newrLeft, newrRight,
    newRLeftRes, newRRightRes, errs
  },

  If[!pairStateQ[state],
    Message[SymmetrizationStep::badstate];
    Return @ failure[
      "InvalidPairState",
      "Input state is not a valid pair-state Association."
    ]
  ];

  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated], Return[validated]];

  newrLeft = CrossMidpointLeft[state["rLeft"], state["RRight"]];
  newrRight = CrossMidpointRight[state["rRight"], state["RLeft"]];

  newRLeftRes = CVTPulley`ImplicitMap`ImplicitRadius[
    newrLeft, validated,
    "StoreHistory" -> OptionValue["ImplicitStoreHistory"]
  ];
  If[FailureQ[newRLeftRes], Return[newRLeftRes]];

  newRRightRes = CVTPulley`ImplicitMap`ImplicitRadius[
    newrRight, validated,
    "StoreHistory" -> OptionValue["ImplicitStoreHistory"]
  ];
  If[FailureQ[newRRightRes], Return[newRRightRes]];

  errs = SymmetryErrorPair[
    newrLeft, newRLeftRes["Root"],
    newrRight, newRRightRes["Root"]
  ];

  <|
    "Iteration" -> state["Iteration"] + 1,
    "IndexLeft" -> state["IndexLeft"],
    "IndexRight" -> state["IndexRight"],
    "tLeft" -> state["tLeft"],
    "tRight" -> state["tRight"],
    "rLeft" -> newrLeft,
    "RLeft" -> newRLeftRes["Root"],
    "rRight" -> newrRight,
    "RRight" -> newRRightRes["Root"],
    "LeftImplicitResult" -> newRLeftRes,
    "RightImplicitResult" -> newRRightRes,
    "LeftCrossError" -> errs["LeftCrossError"],
    "RightCrossError" -> errs["RightCrossError"],
    "MaxCrossError" -> errs["MaxCrossError"]
  |>
];

(* ========================================= *)
(* Iterate one pair                          *)
(* ========================================= *)

Options[IterateSymmetricPair] = {
  "Tolerance" -> Automatic,
  "MaxIterations" -> Automatic,
  "StoreHistory" -> True,
  "ImplicitStoreHistory" -> False
};

IterateSymmetricPair[i_Integer, params_Association, OptionsPattern[]] := Module[
  {
    validated, n, tol, maxIter, storeHistory,
    current, history = {}, converged, k = 1
  },

  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[IterateSymmetricPair::badparams];
    Return[validated];
  ];

  n = validated["nGrid"];

  If[!(0 <= i <= Floor[(n - 1)/2]),
    Message[IterateSymmetricPair::badindex, i];
    Return @ failure[
      "InvalidPairIndex",
      "Index i is outside the valid pair range.",
      <|"i" -> i, "nGrid" -> n|>
    ];
  ];

  tol = Replace[OptionValue["Tolerance"], Automatic -> validated["epsSym"]];
  maxIter = Replace[OptionValue["MaxIterations"], Automatic -> validated["maxSymIter"]];
  storeHistory = TrueQ[OptionValue["StoreHistory"]];

  current = InitialPairState[i, validated];
  If[FailureQ[current], Return[current]];

  If[storeHistory, AppendTo[history, current]];

  While[current["MaxCrossError"] > tol && k < maxIter,
    current = SymmetrizationStep[
      current,
      validated,
      "ImplicitStoreHistory" -> OptionValue["ImplicitStoreHistory"]
    ];
    If[FailureQ[current], Return[current]];
    If[storeHistory, AppendTo[history, current]];
    k++;
  ];

  converged = current["MaxCrossError"] <= tol;

  <|
    "Type" -> "PairIteration",
    "PairIndexLeft" -> i,
    "PairIndexRight" -> n - i,
    "InitialIterationNumber" -> 1,
    "FinalIterationNumber" -> current["Iteration"],
    "IterationsPerformed" -> current["Iteration"] - 1,
    "Tolerance" -> tol,
    "Converged" -> converged,
    "FinalState" -> current,
    "History" -> If[storeHistory, history, Missing["NotStored"]]
  |>
];

(* ========================================= *)
(* Midpoint iteration                        *)
(* ========================================= *)

Options[IterateMidpoint] = {
  "Tolerance" -> Automatic,
  "MaxIterations" -> Automatic,
  "StoreHistory" -> True,
  "ImplicitStoreHistory" -> False
};

IterateMidpoint[params_Association, OptionsPattern[]] := Module[
  {
    validated, n, m, tMid, tol, maxIter, storeHistory,
    currentr, currentRRes, currentR, currentErr,
    history = {}, iter = 1, converged = False
  },

  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[IterateMidpoint::badparams];
    Return[validated];
  ];

  n = validated["nGrid"];
  If[!EvenQ[n],
    Message[IterateMidpoint::badgrid];
    Return @ failure[
      "NoMidpointOnOddGrid",
      "Midpoint iteration requires even nGrid.",
      <|"nGrid" -> n|>
    ];
  ];

  m = Quotient[n, 2];
  tMid = N[m/n];

  tol = Replace[OptionValue["Tolerance"], Automatic -> validated["epsSym"]];
  maxIter = Replace[OptionValue["MaxIterations"], Automatic -> validated["maxSymIter"]];
  storeHistory = TrueQ[OptionValue["StoreHistory"]];

  currentr = CVTPulley`ImplicitMap`InitialLinearRadiusProfile[tMid, validated];
  currentRRes = CVTPulley`ImplicitMap`ImplicitRadius[
    currentr, validated,
    "StoreHistory" -> OptionValue["ImplicitStoreHistory"]
  ];
  If[FailureQ[currentRRes], Return[currentRRes]];

  currentR = currentRRes["Root"];
  currentErr = Abs[currentR - currentr];

  If[storeHistory,
    AppendTo[history, <|
      "Iteration" -> iter,
      "Index" -> m,
      "t" -> tMid,
      "r" -> currentr,
      "R" -> currentR,
      "Error" -> currentErr,
      "ImplicitResult" -> currentRRes
    |>]
  ];

  While[currentErr > tol && iter < maxIter,
    currentr = (currentr + currentR)/2;

    currentRRes = CVTPulley`ImplicitMap`ImplicitRadius[
      currentr, validated,
      "StoreHistory" -> OptionValue["ImplicitStoreHistory"]
    ];
    If[FailureQ[currentRRes], Return[currentRRes]];

    currentR = currentRRes["Root"];
    currentErr = Abs[currentR - currentr];
    iter++;

    If[storeHistory,
      AppendTo[history, <|
        "Iteration" -> iter,
        "Index" -> m,
        "t" -> tMid,
        "r" -> currentr,
        "R" -> currentR,
        "Error" -> currentErr,
        "ImplicitResult" -> currentRRes
      |>]
    ];
  ];

  converged = currentErr <= tol;

  <|
    "Type" -> "MidpointIteration",
    "Index" -> m,
    "t" -> tMid,
    "Finalr" -> currentr,
    "FinalR" -> currentR,
    "FinalError" -> currentErr,
    "IterationsPerformed" -> iter - 1,
    "Tolerance" -> tol,
    "Converged" -> converged,
    "History" -> If[storeHistory, history, Missing["NotStored"]]
  |>
];

(* ========================================= *)
(* Full symmetrization over the grid         *)
(* ========================================= *)

Options[SymmetrizationData] = {
  "Tolerance" -> Automatic,
  "MaxIterations" -> Automatic,
  "StoreHistory" -> True,
  "ImplicitStoreHistory" -> False
};

SymmetrizationData[params_Association, OptionsPattern[]] := Module[
  {
    validated, n, pairIndices, pairResults, midpointResult
  },

  validated = ValidateParameters[params, "EmitWarnings" -> False];
  If[FailureQ[validated],
    Message[SymmetrizationData::badparams];
    Return[validated];
  ];

  n = validated["nGrid"];
  pairIndices = Range[0, Floor[(n - 1)/2]];

  pairResults = IterateSymmetricPair[
      #,
      validated,
      "Tolerance" -> OptionValue["Tolerance"],
      "MaxIterations" -> OptionValue["MaxIterations"],
      "StoreHistory" -> OptionValue["StoreHistory"],
      "ImplicitStoreHistory" -> OptionValue["ImplicitStoreHistory"]
    ] & /@ pairIndices;

  If[AnyTrue[pairResults, FailureQ],
    Return @ failure[
      "PairwiseSymmetrizationFailed",
      "At least one pairwise symmetrization failed.",
      <|"PairResults" -> pairResults|>
    ]
  ];

  midpointResult =
    If[EvenQ[n],
      IterateMidpoint[
        validated,
        "Tolerance" -> OptionValue["Tolerance"],
        "MaxIterations" -> OptionValue["MaxIterations"],
        "StoreHistory" -> OptionValue["StoreHistory"],
        "ImplicitStoreHistory" -> OptionValue["ImplicitStoreHistory"]
      ],
      Missing["NoMidpoint"]
    ];

  If[FailureQ[midpointResult], Return[midpointResult]];

  <|
    "Type" -> "FullSymmetrization",
    "nGrid" -> n,
    "PairIndices" -> pairIndices,
    "PairResults" -> pairResults,
    "MidpointResult" -> midpointResult
  |>
];

(* ========================================= *)
(* Extractors                                *)
(* ========================================= *)

PairIterationTable[result_Association] := Module[{},
  If[!KeyExistsQ[result, "History"],
    Message[PairIterationTable::badinput];
    Return @ failure[
      "MissingHistory",
      "Input does not contain iteration history."
    ]
  ];

  result["History"]
];

PairIterationTable[_] := (
  Message[PairIterationTable::badinput];
  failure["InvalidInput", "Input must be a successful result Association returned by IterateSymmetricPair or IterateMidpoint."]
);

BuildSymmetricDiscreteProfiles[symData_Association] := Module[
  {
    n, rFinal, RFinal, pairResults, midpointResult, finalState, i, j
  },

  If[!(AssociationQ[symData] && KeyExistsQ[symData, "PairResults"]),
    Message[BuildSymmetricDiscreteProfiles::badinput];
    Return @ failure[
      "InvalidSymmetrizationData",
      "Input must be a successful Association returned by SymmetrizationData."
    ];
  ];

  n = symData["nGrid"];
  rFinal = ConstantArray[Indeterminate, n + 1];
  RFinal = ConstantArray[Indeterminate, n + 1];

  pairResults = symData["PairResults"];

  Do[
    finalState = pairResults[[k, "FinalState"]];
    i = finalState["IndexLeft"];
    j = finalState["IndexRight"];

    rFinal[[i + 1]] = finalState["rLeft"];
    RFinal[[i + 1]] = finalState["RLeft"];

    rFinal[[j + 1]] = finalState["rRight"];
    RFinal[[j + 1]] = finalState["RRight"];
    ,
    {k, Length[pairResults]}
  ];

  midpointResult = symData["MidpointResult"];
  If[AssociationQ[midpointResult] && midpointResult["Type"] === "MidpointIteration",
    i = midpointResult["Index"];
    rFinal[[i + 1]] = midpointResult["Finalr"];
    RFinal[[i + 1]] = midpointResult["FinalR"];
  ];

  <|
    "tGrid" -> N[Range[0, n]/n],
    "rGrid" -> rFinal,
    "RGrid" -> RFinal,
    "Pairs" -> Transpose[{rFinal, RFinal}]
  |>
];

BuildSymmetricDiscreteProfiles[_] := (
  Message[BuildSymmetricDiscreteProfiles::badinput];
  failure["InvalidInput", "Input must be a successful Association returned by SymmetrizationData."]
);

End[];
EndPackage[];