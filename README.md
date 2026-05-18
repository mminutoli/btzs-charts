# btzs-charts

[![GitHub CI](https://github.com/mminutoli/btzs-charts/workflows/CI/badge.svg)](https://github.com/mminutoli/btzs-charts/actions)
[![Hackage](https://img.shields.io/hackage/v/btzs-charts.svg?logo=haskell)](https://hackage.haskell.org/package/btzs-charts)
[![Stackage Lts](http://stackage.org/package/btzs-charts/badge/lts)](http://stackage.org/lts/package/btzs-charts)
[![Stackage Nightly](http://stackage.org/package/btzs-charts/badge/nightly)](http://stackage.org/nightly/package/btzs-charts)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

`btzs-charts` is a tool for generating characteristic curves and analysis charts for the BTZS (Beyond The Zone System) film photography methodology. It processes film test data to fit HD curves and generates visualizations and exposure guides.

## Features

- **HD Curve Fitting:** Fits characteristic curves from step tablet exposure data using GSL (GNU Scientific Library).
- **Visualization:** Generates high-quality SVG/PostScript plots of film performance.
- **Nix Integration:** Uses Nix to manage system dependencies (GSL, BLAS, LAPACK) for reproducible builds.
- **JSON Data Input:** Easily process film test data stored in JSON format.

## Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [Nix](https://nixos.org/download.html) (optional but recommended for dependency management)

## Installation & Build

This project uses Haskell Stack with Nix integration to manage system libraries.

```bash
# Clone the repository
git clone https://github.com/mminutoli/btzs-charts.git
cd btzs-charts

# Build the project
stack build
```

## Usage

You can run the program using `stack run`. The program expects path to a JSON data file representing a film test.

```bash
# Run with sample data
stack run -- data/AristaUltraEdu100.json
```

### Running Tests

```bash
stack test
```

## Data Format

The input JSON files represent material tests. See the `data/` directory for examples. A typical file includes:

- **Step Tablet Definition:** Exposure values for each step.
- **Material Test Data:** Density measurements for various development times.

## Project Structure

- `src/`: Core logic for curve fitting and plotting.
- `app/`: Main executable entry point.
- `data/`: Sample film test data.
- `test/`: Unit tests and property-based tests.

## License

This project is licensed under the [BSD-3-Clause License](LICENSE).
