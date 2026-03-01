# WIG Simulation - Wing in Ground Effect

A comprehensive Fortran-based simulation framework for analyzing aerodynamic and hydrodynamic interactions of vehicles operating in ground effect. This project provides a scalable platform for computing vehicle dynamics, including panel-based aerodynamic methods, hydrodynamic forces, and state integration over time.

## Overview

This simulation framework is designed to model the complex physics of Wings in Ground (WIG) effect, accounting for:

- **Aerodynamic Forces**: Pressure distributions and forces on vehicle panels near ground boundaries
- **Hydrodynamic Forces**: Water surface interactions and buoyancy effects
- **Vehicle Dynamics**: State integration including position, velocity, and orientation
- **Configurable Scenarios**: TOML-based configuration system for flexible simulation parameters

## Project Structure

```
.
├── app/                          # Application entry point
│   └── main.f90                 # Main simulation program with time-stepping loop
├── src/                          # Core source code
│   ├── base/                    # Foundational modules
│   │   ├── base_kinds.f90      # Precision definitions (real, integer kinds)
│   │   └── helper_mod.f90      # Utility functions
│   ├── infra/                   # Infrastructure & utilities
│   │   ├── io_mod.f90          # Configuration file I/O (TOML parsing)
│   │   ├── logger_mod.f90      # Logging system
│   │   ├── plotting_mod.f90    # Visualization interface
│   │   ├── system_utils_mod.f90# System utilities
│   │   └── timer_mod.f90       # Performance timing
│   ├── maths/                   # Mathematical operations
│   │   ├── math_constants_mod.f90
│   │   ├── math_utils_mod.f90
│   │   ├── geometry/           # Geometric computations
│   │   └── linalg/             # Linear algebra operations
│   ├── physics/                 # Physics constants & models
│   │   └── physics_constants_mod.f90
│   ├── state/                   # State management
│   │   └── state_mod.f90
│   └── vehicle/                 # Vehicle representation
│       ├── mesh_mod.f90        # Mesh data structures
│       ├── panel_mod.f90       # Panel-based aerodynamic elements
│       └── vehicle_mod.f90     # Vehicle aggregation
├── test/                         # Unit tests
│   ├── test_euler.f90
│   ├── test_infra.f90
│   ├── test_panel.f90
│   └── test_vector.f90
├── docs/                         # Documentation
│   ├── Logger.md
│   ├── Plotter.md
│   └── Timer.md
├── fpm.toml                      # Fortran Package Manager configuration
├── input.toml                    # Simulation configuration (runtime)
└── Makefile                      # Build automation

```

## Key Features

- **Modern Fortran Building**: Built with FPM (Fortran Package Manager) for clean dependency management
- **Comprehensive Logging**: Structured logging system with console and file output
- **Performance Monitoring**: Built-in timer module for profiling simulation performance
- **Visualization Support**: Integration with pyplot-fortran for post-processing plots
- **Modular Architecture**: Clear separation of concerns across infrastructure, mathematics, physics, and vehicle representation
- **TOML Configuration**: Flexible input configuration via standard TOML format

## Implementation Roadmap

The simulation framework requires the following components to be fully operational:

1. **Aerodynamic Module** (`get_aerodynamic()`)
   - Vortex Lattice Method
   - Wake constists of wake panels and wake particles
   - Integration with vehicle geometry

2. **Hydrodynamic Module** (`get_hydronamic()`)
   - Still not thought

3. **State Update Logic** (`update_state()`)
   - Integrate aerodynamic and hydrodynamic forces
   - Update vehicle position and velocity
   - Handle orientation/attitude dynamics

4. **Output Management** (`write_output()`)
   - Generate time history files
   - Create visualization plots
   - Export results for post-processing

## Requirements

- **Fortran Compiler**: gfortran 10.3+
- **Fortran Package Manager**: FPM 0.7.0+
- **Dependencies**:
  - stdlib (Modern Fortran Standard Library)
  - toml-f (TOML file parsing)
  - pyplot-fortran (Python-based plotting)
  - OpenBLAS (Linear algebra acceleration)

## Installation & Build

### Using FPM (Recommended)

```bash
# Clone/download the project
cd PhD-WIG

# Build the project
fpm build

# Run the simulation
fpm run

# Run tests
fpm test
```

### Manual Build with Makefile

```bash
make build
make run
make clean
```

## Configuration (not implemented yet)

Create or modify `input.toml` to configure simulation parameters:

```toml
title = "WIG Simulation Case 1"
max_time = 10.0
time_step = 0.01

[vehicle]
# Vehicle parameters

[environment]
# Environmental conditions (wind speed, water depth, etc.)
```

See `input.toml` for a complete example with all configurable parameters.

## Running the Simulation

```bash
fpm run
```

The simulation will:

1. Load configuration from `input.toml`
2. Initialize all simulation modules
3. Execute a time-stepping loop from t=0 to max_time
4. Log progress to both console and `simulation.log`
5. Generate output files in the `output/` directory

Example output:

```
=== WIG Simulation Starting ===
Title: 'WIG Simulation Case 1'
Entries: 5
Spectrum: [0.5, 1.2, 2.1, 0.8, 1.5]
=== Simulation Complete ===
Simulation ran in    5.234 seconds.
```

## Testing

Run the comprehensive test suite:

```bash
fpm test
```

Individual test modules:

- `test_euler.f90` - Numerical integration methods
- `test_infra.f90` - Infrastructure modules (logging, I/O)
- `test_panel.f90` - Panel geometry and aerodynamics
- `test_vector.f90` - Vector operations and linear algebra

## Documentation

Detailed module documentation available in `docs/`:

- [Logger Module](docs/Logger.md) - Logging system configuration
- [Plotter Module](docs/Plotter.md) - Visualization capabilities
- [Timer Module](docs/Timer.md) - Performance measurement

## Performance

The simulation framework is optimized for:

- Parallel computation (OpenBLAS for linear algebra)
- Efficient memory management
- Real-time performance monitoring

Timing information is automatically logged for each simulation run.

## Output

Simulation results are written to the `output/` directory:

- Time history files (`.txt`, `.dat`)
- Python plotting scripts (`.py`)
- Configuration snapshots (`.toml`)

## Development Status

**Current Version**: 0.1.0  
**Status**: Under Development  
**Planned Features**:

- Multi-threaded aerodynamic calculations
- Coupled fluid-structure interaction
- Advanced mesh refinement strategies
- Real-time visualization

## License

MIT License - Copyright 2026, Georgios Loukas

## Author

Georgios Loukas  
Email: georgios.a.loukas@gmail.com

## Contributing

Contributions are welcome! Please ensure:

- Code follows the existing module structure
- Comprehensive tests are included
- Documentation is updated accordingly

---

For questions or issues, please contact the maintainer or open an issue in the project repository.
