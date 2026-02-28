# Plotting Module Documentation (`plotting_mod`)

## Overview

The `plotting_mod` module provides a clean, stateful Fortran wrapper (`plotter_t`) around [`pyplot-fortran`](https://www.google.com/search?q=%5Bhttps://github.com/jacobwilliams/pyplot-fortran%5D(https://github.com/jacobwilliams/pyplot-fortran)). It uses lazy initialization, allowing you to easily configure figure properties (titles, labels, grids, dimensions, and axis limits) before adding data series and exporting the Python script.

A `check_matplotlib()` utility function is also provided for runtime availability checking, ensuring that scripts are only executed if the host system has the proper Python environment.

---

## Prerequisites

Python 3 with **matplotlib** must be installed and accessible in your system's `PATH`:

```bash
pip install matplotlib

```

---

## 2D Plotting (`plotter_t`)

Use the `plotter_t` derived type to generate 2D plots. This wrapper manages the underlying `pyplot-fortran` instance and handles Python script generation automatically, including advanced styling and layout control.

```fortran
program demo_2d
   use base_kinds_mod, only: wp
   use plotting_mod, only: plotter_t
   implicit none

   type(plotter_t) :: plt
   real(wp) :: x(100), y(100), y2(100)
   integer :: i

   ! Generate sample data
   do i = 1, 100
      x(i) = real(i, wp) * 0.1_wp
      y(i) = sin(x(i))
      y2(i) = cos(x(i))
   end do

   ! Initialize a new figure instance with custom size
   call plt%figure(figsize=[12, 8])

   ! Configure plot properties (cached until data is added)
   call plt%grid(.true.)
   call plt%xlabel('Time (s)')
   call plt%ylabel('Amplitude')
   call plt%title('System Response')
   call plt%xrange(0.0_wp, 10.0_wp)

   ! Add data series with advanced styling
   call plt%add(x, y, label='Sine', style='-', linewidth=2, color=[1.0_wp, 0.0_wp, 0.0_wp])
   call plt%add(x, y2, label='Cosine', style='--', linewidth=3)

   ! Save to file (Checks for matplotlib and executes the generated Python script)
   call plt%save('output/response.png')

end program demo_2d

```

### Key `plotter_t` Methods

| Method | Arguments | Description |
| --- | --- | --- |
| `figure` | `([figsize(2)], [tight_layout])` | Resets state and initializes a new plotting instance. Optional `figsize` controls dimensions. |
| `title` | `(text)` | Sets the plot title. |
| `xlabel` | `(text)` | Sets the X-axis label. |
| `ylabel` | `(text)` | Sets the Y-axis label. |
| `grid` | `(enable)` | Enables (`.true.`) or disables (`.false.`) the background grid. |
| `xrange` / `yrange` | `(min, max)` | Explicitly sets the bounding limits for the X or Y axis. |
| `xscale` / `yscale` | `(scale_type)` | Sets the axis scale (e.g., `"linear"`, `"log"`). |
| `add` | `(x, y, label, style, [linewidth], [markersize], [color(3)])` | Adds a 2D data series. Uses standard Matplotlib styles (e.g., `"-"`). Optional arguments allow precise control over line thickness, marker size, and RGB color. |
| `save` | `(filename)` | Saves to an image file. It automatically calls `check_matplotlib()` and executes the script if available. |

---

## Utility Functions

### `check_matplotlib()`

```fortran
use plotting_mod, only: check_matplotlib
logical :: ok

ok = check_matplotlib()

```

Returns `.true.` if Python and matplotlib are usable on the system. Results are cached after the first call to prevent redundant system checks during program execution.

---

## Output Files

| File Pattern | Description |
| --- | --- |
| `output/plot_*.py` | Generated Python scripts for 2D plots. Executed automatically by `plotter_t%save()` if Matplotlib is installed. |
