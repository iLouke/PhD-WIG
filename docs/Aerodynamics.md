# Aerodynamics Module — UVLM Solver

## Overview

Modern, object-oriented implementation of the **Unsteady Vortex Lattice Method (UVLM)** for 3D aerodynamic analysis. Rewritten from legacy Fortran 77 code (`PROGRAM ROS`) into modular Fortran 2008+ with:

- **Quaternion-based rotations** (singularity-free, replacing Euler angle trig)
- **OpenMP multithreading** for AIC matrix assembly, wake relaxation, and force computation
- **LAPACK/FlexiBLAS** for linear algebra (LU factorization replaces hand-coded SVD)
- **Maximum modularity** — 12 independent modules with clean interfaces

---

## Module Architecture

```
src/aero/
├── vector3d_mod.f90          # 3D vector type with operator overloading
├── quaternion_mod.f90        # Quaternion math for rotations
├── panel_mod.f90             # Panel geometry (centroid, normal, area)
├── grid_mod.f90              # Mesh management (nodes + panels)
├── vortex_mod.f90            # Biot-Savart vortex induction law
├── aero_solver_mod.f90       # LAPACK LU solver for AIC system
├── symmetry_mod.f90          # Y-plane mirror + ground effect
├── wake_mod.f90              # Free-wake shedding and convection
├── influence_mod.f90         # AIC matrix assembly (OpenMP)
├── penetration_mod.f90       # Wake-body penetration correction
├── aerodynamic_loads_mod.f90 # Lift, drag, side force computation
└── uvlm_solver_mod.f90       # Top-level UVLM orchestrator
```

### Dependency Graph

```
base_kinds_mod ─┬─> vector3d_mod ─┬─> quaternion_mod
                │                  ├─> panel_mod
constants_mod ──┘                  ├─> vortex_mod
                                   ├─> symmetry_mod
                                   └─> grid_mod (uses quaternion_mod, panel_mod)
                                         │
              aero_solver_mod ────────────┤
                                          ├─> wake_mod (uses grid_mod, vortex_mod, symmetry_mod)
                                          ├─> influence_mod (uses grid_mod, vortex_mod, wake_mod, aero_solver_mod)
                                          ├─> penetration_mod (uses grid_mod, wake_mod)
                                          ├─> aerodynamic_loads_mod (uses grid_mod, wake_mod, vortex_mod)
                                          └─> uvlm_solver_mod (orchestrates all)
```

---

## Legacy-to-Modern Mapping

| Legacy Code | Modern Module | Key Changes |
|-------------|--------------|-------------|
| `PROGRAM ROS` | `uvlm_solver_mod` | OOP solver with `init/step/run` methods |
| `SUBROUTINE GEOM` | `grid_mod::mesh_t%apply_rotation` | Quaternion rotation replaces Euler trig matrix |
| `SUBROUTINE ANALGEO` | `panel_mod::panel_t%compute_geometry` | Per-panel OOP method, OpenMP parallel |
| `SUBROUTINE VORTEX` | `vortex_mod::vortex_ring_velocity` | Pure function, core-radius regularization |
| `SUBROUTINE VORCALC` | `influence_mod::build_aic_matrix/build_rhs` | Split AIC build + RHS; OpenMP parallel |
| `SUBROUTINE SVDCMP/SVDBK` | `aero_solver_mod::aero_linsys_t` | LAPACK DGETRF/DGETRS replaces hand-coded SVD |
| `FUNCTION PYTHAG` | *(removed)* | LAPACK handles internally |
| `SUBROUTINE WAKE` | `wake_mod::wake_t%shed` | Allocatable arrays, OOP wake type |
| `SUBROUTINE WAKREL` | `wake_mod::wake_t%relax` | OpenMP parallel over grid points |
| `SUBROUTINE WAKCOR` | `penetration_mod::correct_wake_penetrations` | Clean iterative correction |
| `SUBROUTINE PENETR` | `penetration_mod::correct_single_point` | Point-in-polygon test preserved |
| `SUBROUTINE CROSS` | `penetration_mod::detect_cross_intersection` | Encapsulated quad intersection |
| `SUBROUTINE WAKINT` | `penetration_mod::handle_wake_intersections` | Prop/wing wake interaction |
| `SUBROUTINE NEIBORG` | *(integrated)* | Neighbor logic folded into wake_mod |
| `SUBROUTINE VELWAK` | `wake_mod::wake_t%compute_induced_velocity` | Method on wake type |
| `SUBROUTINE VELPAN` | `wake_mod::compute_body_induced_vel` (private) | Internal to wake relaxation |
| `SUBROUTINE AIRLOAD1` | `aerodynamic_loads_mod::compute_forces` | Returns `aero_forces_t` struct |
| `SUBROUTINE CPAIP` | `aerodynamic_loads_mod::compute_pressure_coefficients` | OpenMP parallel |
| COMMON blocks | Module types + allocatable arrays | No global state; encapsulated data |

---

## Key Design Decisions

### 1. Quaternions (`quaternion_mod`)

The legacy code applied Euler angle rotation directly via a 3×3 rotation matrix with trigonometric expressions. This approach has:

- Gimbal lock at β = ±90°
- Accumulated floating-point drift in multi-step rotations

The quaternion module provides:

- `from_euler_aero(alpha, beta, gamma)` — constructs the **exact same rotation** as the legacy code by building the rotation matrix first, then converting to a quaternion via Shepperd's method
- `rotate(vector)` — uses the optimized formula `v' = v + w*t + q_vec × t` where `t = 2*(q_vec × v)`, avoiding full quaternion multiplication
- `slerp(q1, q2, t)` — smooth interpolation for dynamic simulations

The rotation reproduces the legacy matrix:

```
R[1,:] = [cosα·cosβ,           sinβ,           sinα·cosβ          ]
R[2,:] = [sinα·sinγ-cosα·sinβ·cosγ, cosβ·cosγ, cosα·sinγ-sinα·sinβ·cosγ]
R[3,:] = [cosα·sinβ·sinγ-sinα·cosγ, -cosβ·sinγ, sinα·sinβ·sinγ+cosα·cosγ]
```

### 2. OpenMP Parallelization

Parallel regions are placed at the **most impactful** computation points:

| Location | Operation | Complexity | Parallelism |
|----------|-----------|-----------|-------------|
| `influence_mod::build_aic_matrix` | AIC matrix assembly | O(N²) | `!$OMP PARALLEL DO` over panel rows |
| `influence_mod::build_rhs` | RHS vector computation | O(N × N_wake) | `!$OMP PARALLEL DO` over panels |
| `grid_mod::compute_panel_geometry` | Panel normals/areas | O(N) | `!$OMP PARALLEL DO` over panels |
| `grid_mod::apply_rotation` | Quaternion rotation | O(N_nodes) | `!$OMP PARALLEL DO` over nodes |
| `wake_mod::relax` | Wake velocity field | O(N_iter × N_wake × N_total) | `!$OMP PARALLEL DO COLLAPSE(2)` |
| `aerodynamic_loads_mod::compute_cp` | Pressure coefficients | O(N × N_total) | `!$OMP PARALLEL DO` |

Each OpenMP region uses `PRIVATE` clauses for thread-local variables and `SCHEDULE(DYNAMIC)` for load balancing (panel work varies due to tri/quad differences).

### 3. LAPACK/FlexiBLAS (`aero_solver_mod`)

The legacy hand-coded SVD (`SVDCMP` + `SVDBK` + `PYTHAG`) is replaced by:

| Workflow Step | LAPACK Routine | Purpose |
|-------------|---------------|---------|
| First iteration | `DGETRF` | LU factorization of AIC matrix (O(2/3 N³)) |
| Each iteration | `DGETRS` | Back-substitution with new RHS (O(N²)) |
| Ill-conditioned fallback | `DGELSD` | SVD least-squares solve |

**Performance gain**: The LU factorization is done once and reused for all subsequent iterations. The legacy SVD had complexity ~4N³ vs LU's ~2/3 N³, a ~6× speedup for the factorization step alone. FlexiBLAS automatically selects the best BLAS backend (OpenBLAS, MKL, BLIS, etc.) at runtime.

### 4. Modularity

Each module is:

- **Self-contained**: All data and procedures for one concept
- **Independently testable**: Can be unit-tested without the full solver
- **Loosely coupled**: Dependencies flow one way (see graph above)
- **Hot-swappable**: e.g., replace `aero_solver_mod` with a GPU solver without touching other modules

---

## Algorithm Flow

```
┌─────────────────────────────────────────┐
│         solver%init(mesh, config)       │
│  1. Apply quaternion rotation to mesh   │
│  2. Apply ground effect offset          │
│  3. Compute panel geometry (ANALGEO)    │
│  4. Initialize wake storage             │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│          solver%step() [iter=0]         │
│  1. Build AIC matrix (VORCALC)          │
│  2. LU-factorize AIC (DGETRF)          │
│  3. Build RHS (no wake)                 │
│  4. Solve: gamma = A^-1 * b (DGETRS)   │
└──────────────┬──────────────────────────┘
               │
        ┌──────▼──────┐
        │  iter loop  │◄──────────────┐
        └──────┬──────┘               │
               ▼                      │
┌─────────────────────────────────────┤
│     solver%step() [iter > 0]       │
│  1. Shed wake (WAKE)               │
│  2. Convect wake freestream        │
│  3. Handle intersections (WAKINT)  │
│  4. Correct penetrations (WAKCOR)  │
│  5. Relax wake (WAKREL)            │
│  6. Build RHS with wake velocity   │
│  7. Solve (reuse LU factors)       │
│  8. Compute forces (AIRLOAD1)      │
│  9. Check convergence              │
└──────────────┬─────────────────────┘
               │
        ┌──────▼──────┐
        │ Converged?  │──No──────────┘
        └──────┬──────┘
               │ Yes
               ▼
┌─────────────────────────────────────┐
│  Post-processing                   │
│  - Pressure coefficients (CPAIP)   │
│  - Output forces, wake, solution   │
└─────────────────────────────────────┘
```

---

## Usage Example

```fortran
use uvlm_solver_mod
use grid_mod

type(mesh_t) :: mesh
type(uvlm_config_t) :: cfg
type(uvlm_solver_t) :: solver
type(aero_forces_t) :: forces
integer :: info

! Setup mesh (would normally load from file)
call mesh%set_geometry(nodes, panel_ids, marks, props)

! Configure solver
cfg%vinit     = 30.0_wp    ! m/s
cfg%alpha_deg = 5.0_wp     ! degrees
cfg%rho       = 1.225_wp   ! kg/m³
cfg%nsym      = 1          ! Y-symmetry
cfg%ngrnd     = 1          ! Ground effect
cfg%hfl       = 2.0_wp     ! 2m above ground

! Initialize and run
call solver%init(mesh, cfg)
call solver%run(info)

! Get results
forces = solver%get_forces()
print *, "Lift =", forces%lift, "N"
print *, "Drag =", forces%drag, "N"

call solver%destroy()
```

---

## File Sizes & Complexity

| Module | Lines | Key Responsibility |
|--------|-------|--------------------|
| `vector3d_mod.f90` | ~170 | 3D vector type + operators |
| `quaternion_mod.f90` | ~270 | Quaternion algebra + rotation |
| `panel_mod.f90` | ~150 | Panel geometry computation |
| `grid_mod.f90` | ~160 | Mesh management + rotation |
| `vortex_mod.f90` | ~130 | Biot-Savart vortex induction |
| `aero_solver_mod.f90` | ~170 | LAPACK LU/SVD solver |
| `symmetry_mod.f90` | ~60 | Mirror operations |
| `wake_mod.f90` | ~370 | Wake shedding + convection |
| `influence_mod.f90` | ~180 | AIC matrix + system solve |
| `penetration_mod.f90` | ~240 | Wake penetration correction |
| `aerodynamic_loads_mod.f90` | ~230 | Force + Cp computation |
| `uvlm_solver_mod.f90` | ~290 | Top-level solver orchestrator |
| **Total** | **~2420** | Full UVLM implementation |
