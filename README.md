# CVTPulley / cvt-symmetry-wm

Wolfram Mathematica package for the numerical study of the **continuous transmission pulley problem** and the construction of a **symmetric discrete radius solution** under a fixed belt-length constraint.

This project accompanies the article:

**“Symmetrical solution to Continuous transmission pulley problem”**

## Purpose

The package implements a modular numerical pipeline for:

* geometric quantities of the open-belt two-pulley configuration
* belt-length evaluation under fixed center distance
* implicit computation of the radius mapping (R=g(r))
* discrete pairwise symmetrization of radius samples
* midpoint computation for the symmetric solution
* quadratic approximation of the symmetric profile
* residual/error analysis
* generation of tables, figures, and exported outputs

The current goal of the project is a **local, functional, and well-structured package**, ready for later migration to GitHub and public release.

## Project structure

```text
cvt-symmetry-wm/
  notebooks/
  src/
    CVTPulley/
      Kernel/
        Package.wl
        TypesAndValidation.wl
        Geometry.wl
        BeltLength.wl
        RootFinding.wl
        ImplicitMap.wl
        Symmetrization.wl
        ExportTools.wl
        ErrorAnalysis.wl
        QuadraticApproximation.wl
        TablesAndFigures.wl
  output/
    figures/
    logs/
    raw/
    tables/
```

## Main modules

### `TypesAndValidation.wl`

Defines parameter construction and validation, default values, numerical tolerances, and grid/index helper functions.

### `Geometry.wl`

Implements elementary geometric quantities for the open-belt configuration:
contact-angle offset, wrap angles, straight span, arc lengths, and geometry summaries.

### `BeltLength.wl`

Implements the belt-length formula with **two straight segments**, residual functions, and reference belt-length construction.

### `RootFinding.wl`

Implements bisection-based root finding and specialized radius solving for the implicit belt-length equation.

### `ImplicitMap.wl`

Builds the implicit relation (R=g(r)) on individual points and on the default grid.

### `Symmetrization.wl`

Implements the discrete pairwise symmetrization procedure, midpoint iteration, and reconstruction of symmetric discrete profiles.

### `ExportTools.wl`

Provides output helpers for exporting raw data, tables, logs, and figures to the project `output/` subfolders.

### `ErrorAnalysis.wl`

Computes residuals and basic summary diagnostics for implicit-map and related outputs.

### `QuadraticApproximation.wl`

Builds the quadratic approximation of the symmetric radius profile using:

* left endpoint
* midpoint value
* right endpoint

and defines the symmetric companion approximation by reflection.

### `TablesAndFigures.wl`

Builds package-level tabular outputs and figure objects from already computed pipeline stages.

### `Package.wl`

Loader file for the local package modules.

## Loading the package

The package is currently loaded through `Package.wl` located in:

```text
src/CVTPulley/Kernel/Package.wl
```

A typical notebook setup uses:

1. a root cell that defines the project directories
2. a loader cell that executes `Get[packageFile]`

Example loader logic:

```wl
packageRoot = FileNameJoin[{projectRoot, "src", "CVTPulley", "Kernel"}];
packageFile = FileNameJoin[{packageRoot, "Package.wl"}];

Get[packageFile];
```

## Typical workflow

A basic workflow is:

1. construct validated parameters with `MakeParameters[...]`
2. set the reference belt length with `SetReferenceBeltLength[...]`
3. compute the implicit map
4. run symmetrization
5. compute the midpoint value and quadratic approximation
6. run residual/error analysis
7. build tables and figures
8. export selected outputs

Example:

```wl
params0 = CVTPulley`TypesAndValidation`MakeParameters[<|
  "rMin" -> 10,
  "rMax" -> 30,
  "d" -> 70,
  "nGrid" -> 100
|>];

params = CVTPulley`BeltLength`SetReferenceBeltLength[params0];

symData = CVTPulley`Symmetrization`SymmetrizationData[params];
quadData = CVTPulley`QuadraticApproximation`QuadraticApproximationData[params];
devTab = CVTPulley`TablesAndFigures`DeviationTable[params];
```

## Output folders

The package uses the following output structure:

* `output/figures` — exported figures
* `output/logs` — log files
* `output/raw` — raw Mathematica data
* `output/tables` — exported tables

`ExportTools.wl` routes outputs automatically to the appropriate subfolder.

## Current status

The package currently has a working local pipeline with:

* modular kernel files
* package loader
* smoke-tested core workflow
* tested export layer
* tested table/figure layer

A full smoke test has been executed successfully for:

* parameters
* geometry
* root finding
* implicit map
* symmetrization
* error analysis
* quadratic approximation
* tables
* figures
* export

## Notes

* The project currently prioritizes **local functionality and clean structure** over distribution tooling.
* GitHub/DOI/release workflow is intentionally postponed to a later stage.
* The package has been built from a numerically verified pipeline used to regenerate the article’s corrected tables and figures after fixing the belt-length implementation.

## Next possible steps

Possible future improvements include:

* additional regression tests
* a cleaner testing folder structure
* a more polished public-facing README
* example notebooks for article reproduction
* GitHub migration and archival workflow
* DOI-oriented release preparation
