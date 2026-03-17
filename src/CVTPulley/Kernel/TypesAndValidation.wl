(* ::Package:: *)

BeginPackage["CVTPulley`TypesAndValidation`"];

MakeParameters::usage =
  "MakeParameters[assoc] creates a validated Association of model and \
numerical parameters for the continuous transmission pulley problem.";

ValidateParameters::usage =
  "ValidateParameters[params] validates the parameter Association and \
returns the same Association if valid; otherwise it returns Failure. \
Use the option \"EmitWarnings\" -> False to suppress non-fatal warnings.";

ParameterQ::usage =
  "ParameterQ[expr] returns True if expr is a parameter Association with \
the required keys.";

GetParameter::usage =
  "GetParameter[params, key] returns params[key] with a message if the key \
is missing.";

DefaultParameters::usage =
  "DefaultParameters[] returns the default parameter Association.";

SymmetryPairIndexQ::usage =
  "SymmetryPairIndexQ[i, n] returns True if i is a valid grid index for \
pairwise symmetrization on a grid of size n.";

MidpointIndexQ::usage =
  "MidpointIndexQ[i, n] returns True if i is the midpoint index on an even \
grid with n subintervals.";

GridIndexQ::usage =
  "GridIndexQ[i, n] returns True if i is a valid grid index between 0 and n.";

GridValue::usage =
  "GridValue[i, n] returns the grid point i/n.";

SymmetricGridIndex::usage =
  "SymmetricGridIndex[i, n] returns the index symmetric to i, namely n-i.";

RequiredParameterKeys::usage =
  "RequiredParameterKeys[] returns the list of required parameter keys.";

Begin["`Private`"];

(* ========================================= *)
(* Required keys and defaults                *)
(* ========================================= *)

RequiredParameterKeys[] := {
   "rMin",
   "rMax",
   "d",
   "nGrid",
   "epsSym",
   "epsBis",
   "maxSymIter",
   "maxBisIter",
   "workingPrecision",
   "beltLength"
};

DefaultParameters[] := <|
   "rMin" -> Missing["NotSpecified"],
   "rMax" -> Missing["NotSpecified"],
   "d" -> Missing["NotSpecified"],
   "nGrid" -> 100,
   "epsSym" -> 10^-6,
   "epsBis" -> 10^-12,
   "maxSymIter" -> 200,
   "maxBisIter" -> 200,
   "workingPrecision" -> MachinePrecision,
   "beltLength" -> Missing["ToBeComputed"],
   "storeIterationHistory" -> True,
   "verbose" -> False,
   "exportBaseName" -> "example",
   "outputDirectory" -> "output"
|>;

Options[ValidateParameters] = {"EmitWarnings" -> True};

(* ========================================= *)
(* Basic predicates                          *)
(* ========================================= *)

ParameterQ[expr_] :=
  AssociationQ[expr] &&
   AllTrue[RequiredParameterKeys[], KeyExistsQ[expr, #] &];

GridIndexQ[i_, n_Integer?Positive] :=
  IntegerQ[i] && 0 <= i <= n;

SymmetryPairIndexQ[i_, n_Integer?Positive] :=
  IntegerQ[i] && 0 <= i <= n && i <= n - i;

MidpointIndexQ[i_, n_Integer?Positive] :=
  EvenQ[n] && IntegerQ[i] && i == Quotient[n, 2];

GridValue[i_Integer, n_Integer?Positive] /; GridIndexQ[i, n] := N[i/n];

SymmetricGridIndex[i_Integer, n_Integer?Positive] /; GridIndexQ[i, n] := n - i;

GetParameter::missing = "Required parameter key `1` is missing.";

GetParameter[params_Association, key_] :=
  If[KeyExistsQ[params, key],
    params[key],
    Message[GetParameter::missing, key];
    Missing["KeyAbsent", key]
  ];

(* ========================================= *)
(* Validation messages                       *)
(* ========================================= *)

ValidateParameters::notassoc =
  "Input must be an Association.";

ValidateParameters::missingkeys =
  "The following required parameter keys are missing: `1`.";

ValidateParameters::badnum =
  "Parameter `1` must be a positive real number. Got `2`.";

ValidateParameters::badint =
  "Parameter `1` must be a positive integer. Got `2`.";

ValidateParameters::ord =
  "Expected rMin < rMax, but got rMin = `1`, rMax = `2`.";

ValidateParameters::geometry =
  "Open-belt geometry requires Abs[rMax - rMin] < d. Got Abs[rMax-rMin] = `1`, d = `2`.";

ValidateParameters::belt =
  "Parameter beltLength must be either Missing[\"ToBeComputed\"] or a positive real number. Got `1`.";

ValidateParameters::tol =
  "Numerical tolerance hierarchy should satisfy epsBis < epsSym. Got epsBis = `1`, epsSym = `2`.";

ValidateParameters::precision =
  "workingPrecision must be MachinePrecision or a positive integer. Got `1`.";

ValidateParameters::ngrid =
  "For midpoint-based tables/figures, nGrid should be even. Got nGrid = `1`.";

ValidateParameters::warnprec =
  "Warning: workingPrecision is MachinePrecision while epsBis = `1`; \
high-accuracy bisection may later require a higher WorkingPrecision.";

(* ========================================= *)
(* Internal helper checks                    *)
(* ========================================= *)

positiveRealQ[x_] := NumericQ[x] && TrueQ[x > 0];

positiveIntegerQ[x_] := IntegerQ[x] && x > 0;

validPrecisionQ[p_] := (p === MachinePrecision) || (IntegerQ[p] && p > 0);

failure[tag_, msg_, assoc_: <||>] :=
  Failure[tag, Join[<|"MessageTemplate" -> msg|>, assoc]];

(* ========================================= *)
(* Main validator                            *)
(* ========================================= *)

ValidateParameters[params_, OptionsPattern[]] /; ! AssociationQ[params] :=
  (Message[ValidateParameters::notassoc];
   failure["NotAssociation", "Input must be an Association."]);

ValidateParameters[params_Association, OptionsPattern[]] := Module[
  {
    missing, rMin, rMax, d, nGrid, epsSym, epsBis,
    maxSymIter, maxBisIter, wp, beltLength, emitWarnings
  },

  emitWarnings = TrueQ[OptionValue["EmitWarnings"]];

  missing = Select[RequiredParameterKeys[], ! KeyExistsQ[params, #] &];
  If[missing =!= {},
    Message[ValidateParameters::missingkeys, missing];
    Return @ failure[
      "MissingKeys",
      "Missing required parameter keys.",
      <|"MissingKeys" -> missing|>
    ];
  ];

  rMin = params["rMin"];
  rMax = params["rMax"];
  d = params["d"];
  nGrid = params["nGrid"];
  epsSym = params["epsSym"];
  epsBis = params["epsBis"];
  maxSymIter = params["maxSymIter"];
  maxBisIter = params["maxBisIter"];
  wp = params["workingPrecision"];
  beltLength = params["beltLength"];

  If[! positiveRealQ[rMin],
    Message[ValidateParameters::badnum, "rMin", rMin];
    Return @ failure["InvalidrMin", "rMin must be a positive real number.", <|"Value" -> rMin|>];
  ];

  If[! positiveRealQ[rMax],
    Message[ValidateParameters::badnum, "rMax", rMax];
    Return @ failure["InvalidrMax", "rMax must be a positive real number.", <|"Value" -> rMax|>];
  ];

  If[! positiveRealQ[d],
    Message[ValidateParameters::badnum, "d", d];
    Return @ failure["Invalidd", "d must be a positive real number.", <|"Value" -> d|>];
  ];

  If[! positiveIntegerQ[nGrid],
    Message[ValidateParameters::badint, "nGrid", nGrid];
    Return @ failure["InvalidnGrid", "nGrid must be a positive integer.", <|"Value" -> nGrid|>];
  ];

  If[! positiveRealQ[epsSym],
    Message[ValidateParameters::badnum, "epsSym", epsSym];
    Return @ failure["InvalidEpsSym", "epsSym must be a positive real number.", <|"Value" -> epsSym|>];
  ];

  If[! positiveRealQ[epsBis],
    Message[ValidateParameters::badnum, "epsBis", epsBis];
    Return @ failure["InvalidEpsBis", "epsBis must be a positive real number.", <|"Value" -> epsBis|>];
  ];

  If[! positiveIntegerQ[maxSymIter],
    Message[ValidateParameters::badint, "maxSymIter", maxSymIter];
    Return @ failure["InvalidMaxSymIter", "maxSymIter must be a positive integer.", <|"Value" -> maxSymIter|>];
  ];

  If[! positiveIntegerQ[maxBisIter],
    Message[ValidateParameters::badint, "maxBisIter", maxBisIter];
    Return @ failure["InvalidMaxBisIter", "maxBisIter must be a positive integer.", <|"Value" -> maxBisIter|>];
  ];

  If[! validPrecisionQ[wp],
    Message[ValidateParameters::precision, wp];
    Return @ failure["InvalidWorkingPrecision", "Invalid workingPrecision.", <|"Value" -> wp|>];
  ];

  If[!(beltLength === Missing["ToBeComputed"] || positiveRealQ[beltLength]),
    Message[ValidateParameters::belt, beltLength];
    Return @ failure["InvalidBeltLength", "beltLength must be positive or Missing[\"ToBeComputed\"].",
      <|"Value" -> beltLength|>];
  ];

  If[!(rMin < rMax),
    Message[ValidateParameters::ord, rMin, rMax];
    Return @ failure["InvalidRadiusOrder", "Expected rMin < rMax.", <|"rMin" -> rMin, "rMax" -> rMax|>];
  ];

  If[!(Abs[rMax - rMin] < d),
    Message[ValidateParameters::geometry, Abs[rMax - rMin], d];
    Return @ failure[
      "InvalidGeometry",
      "Open-belt geometry requires Abs[rMax-rMin] < d.",
      <|"AbsDifference" -> Abs[rMax - rMin], "d" -> d|>
    ];
  ];

  If[!(epsBis < epsSym),
    Message[ValidateParameters::tol, epsBis, epsSym];
    Return @ failure[
      "InvalidToleranceHierarchy",
      "Expected epsBis < epsSym.",
      <|"epsBis" -> epsBis, "epsSym" -> epsSym|>
    ];
  ];

  If[emitWarnings && EvenQ[nGrid] === False,
    Message[ValidateParameters::ngrid, nGrid];
  ];

  If[emitWarnings && wp === MachinePrecision && epsBis <= 10^-12,
    Message[ValidateParameters::warnprec, InputForm[epsBis]];
  ];

  params
];

(* ========================================= *)
(* Constructor                               *)
(* ========================================= *)

MakeParameters[input_Association] := Module[
  {params},
  params = Join[DefaultParameters[], input];
  ValidateParameters[params]
];

End[];
EndPackage[];