# Plotting Module Documentation (`plotting_mod`)

## Overview

The `plotting_mod` module provides plotting for Fortran via two mechanisms:

1. **2D plots (`plotter_t`)** — A stateful wrapper around [`pyplot-fortran`](https://github.com/jacobwilliams/pyplot-fortran). It lazily initializes the underlying `pyplot` object, allowing you to cleanly set properties (titles, labels) before adding data series and exporting the script.
2. **3D panel wireframes (`panel_plot_3d_t`)** — For rendering unstructured quad-panel meshes (aerodynamic surfaces, wakes) via custom matplotlib script generation (`Line3DCollection` / `Poly3DCollection`).

A `check_matplotlib()` utility function is provided for runtime availability checking.

---

## Prerequisites

Python 3 with **matplotlib** must be installed and accessible in your system's PATH:

```bash
pip install matplotlib
```

---

## 2D Plotting (`plotter_t` wrapper)

Use the `plotter_t` derived type to generate 2D plots. This wrapper manages the underlying `pyplot-fortran` instance and handles Python script generation automatically.

```fortran
program demo_2d
   use base_kinds_mod, only: wp
   use plotting_mod, only: plotter_t, check_matplotlib
   implicit none

   type(plotter_t) :: plt
   real(wp) :: x(100), y(100)
   integer :: i

   ! Generate sample data
   do i = 1, 100
      x(i) = real(i, wp) * 0.1_wp
      y(i) = sin(x(i))
   end do

   ! Initialize a new figure instance
   call plt%figure()

   ! Configure plot properties (cached until data is added)
   call plt%grid(.true.)
   call plt%xlabel('Time (s)')
   call plt%ylabel('Amplitude')
   call plt%title('System Response')

   ! Add data series (Matplotlib standard linestyles apply)
   call plt%add(x, y, label='Sine Wave', style='-')

   ! Save to file (automatically executes the underlying Python script)
   call plt%save('output/response.png')

end program demo_2d

```

### Key `plotter_t` methods

| Method | Arguments | Description |
| --- | --- | --- |
| `figure` | `()` | Reset state and initialize a new plotting instance. |
| `title` | `(text)` | Set the plot title. |
| `xlabel` | `(text)` | Set the X-axis label. |
| `ylabel` | `(text)` | Set the Y-axis label. |
| `grid` | `(enable)` | Enable (`.true.`) or disable (`.false.`) the background grid. |
| `add` | `(x, y, label, style)` | Add a 2D data series. Uses Matplotlib styles (e.g., `"-"`, `"--"`, `"o"`). |
| `save` | `(filename)` | Save to an image file and execute the generated Python script. |

---

## 3D Panel Wireframes (`panel_plot_3d_t`)

For rendering unstructured quad panels (4 corners × N panels) — e.g., aerodynamic meshes and wakes — use `panel_plot_3d_t`. This generates custom Python/matplotlib scripts since `pyplot-fortran`'s 3D methods require structured grids.

```fortran
use plotting_mod, only: panel_plot_3d_t
type(panel_plot_3d_t) :: p3d
real(wp) :: mesh_px(4, n_panels), mesh_py(4, n_panels), mesh_pz(4, n_panels)

call p3d%set_title("Wing Mesh + Wake")
call p3d%set_labels(xlabel="X [m]", ylabel="Y [m]", zlabel="Z [m]")
call p3d%set_view(55.0_wp, 315.0_wp)

call p3d%add_panels(mesh_px, mesh_py, mesh_pz, "Wing", color="blue")
call p3d%add_panels(wake_px, wake_py, wake_pz, "Wake", color="#CC3333")

call p3d%save("output/mesh.png")
! call p3d%show()  ! interactive window
call p3d%reset()

```

### `panel_plot_3d_t` methods

| Method | Arguments | Description |
| --- | --- | --- |
| `set_title` | `(text)` | Set the plot title |
| `set_labels` | `(xlabel, ylabel, zlabel)` | Set axis labels (all optional) |
| `set_view` | `(elev, azim)` | Set 3D viewing angle (degrees) |
| `set_grid` | `(logical)` | Enable/disable grid |
| `add_panels` | `(px, py, pz, label, color, scalar)` | Add quad panels `(4, n_panels)`. Optional `scalar` for colormap. |
| `save` | `(filename)` | Save to PNG |
| `show` | `()` | Open interactive matplotlib window |
| `reset` | `()` | Free data and reset state |

---

## Utility

### `check_matplotlib()`

```fortran
logical :: ok
ok = check_matplotlib()

```

Returns `.true.` if Python + matplotlib are usable. Results are cached after the first call to prevent redundant system checks.

---

## Output Files

| File Pattern | Description |
| --- | --- |
| `output/plot_*.py` | Generated Python scripts for 2D plots |
| `output/panel3d_*.py` | Generated Python scripts for 3D wireframe rendering |
| `output/panel3d_data_*.dat` | Panel vertex data for 3D wireframe rendering |
