

# PPSE

This repository contains the code used in the PPSE paper.

It implements the PPSE algorithm and compares it with a PSE baseline and a random baseline on the `PatternSquare` benchmark used to assess how well the search recovers structured target patterns.

## Requirements

- JDK 21 or newer
- SBT

## Building the project

From the repository root:

```bash
sbt compile
```

## Running the benchmarks

The benchmark entry points are defined as `@main` methods in `ppse-paper/src/main/scala/ppse/paper/benchmark/PatternSquare.scala`.

### PPSE benchmark

```bash
sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.patternSquareBenchmarkPPSE /tmp/square-ppse.csv 20 10000"
```

### PSE benchmark

```bash
sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.patternSquareBenchmarkPSE /tmp/square-pse.csv 20 10000"
```

### Random baseline

```bash
sbt --mem 4096 "project ppse-paper" "runMain ppse.paper.benchmark.patternSquareBenchmarkRandom /tmp/square-random.csv 20 10000"
```

The repository includes convenience scripts:

```bash
./square-ppse.sh
./square-pse.sh
./square-random.sh
```

## Output format

The benchmark scripts write CSV files with rows of the form:

- replication index
- evaluated sample count or generation index
- approximation error (e.g., Jeffreys divergence)
- number of missed target patterns

This makes it easy to plot convergence and compare algorithms across multiple replications.

## License

This project is distributed under the GNU General Public License v3.0 (AGPL-3.0). See the source files for the license header and licensing notices.
