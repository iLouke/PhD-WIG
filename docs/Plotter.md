# Plotter Module Documentation (`plotting_mod`)

## Overview

The `plotting_mod` is a robust, Object-Oriented wrapper for **Gnuplot**. It enables Fortran applications to generate 2D plots programmatically.

Unlike simple wrappers, this module is designed for **production environments**:

* **Auto-Detection:** Automatically checks if Gnuplot is installed.
* **Graceful Degradation:** If Gnuplot is missing, it skips rendering but still saves numeric data for later analysis.
* **Multi-Instance Safe:** Supports creating multiple plot objects simultaneously without file collisions (using unique instance IDs).
* **Dual Output:** Can render to an interactive window (`qt`) or save directly to an image file (`.png`).

## Prerequisites

To view plots interactively or generate PNGs, **Gnuplot** must be installed on your system.

### Installation

* **Ubuntu/Debian:** `sudo apt-get install gnuplot`
* **Fedora/RPM:** `sudo dnf install gnuplot`
* **macOS:** `brew install gnuplot`
* **Windows:** Install Gnuplot and ensure `gnuplot.exe` is in your system PATH.

> **Note:** If Gnuplot is not found, the module will print a warning and strictly operate in "Data Logging" mode, saving `.dat` files without attempting to render graphics.

---

## Usage Workflow

The standard workflow involves four steps: **Initialize**, **Configure**, **Add Data**, and **Output**.

### 1. Basic Example

```fortran
program demo_plot
   use base_kinds_mod, only: wp
   use plotting_mod
   implicit none

   type(plotter_t) :: plt
   real(wp) :: x(100), y(100)
   integer :: i

   ! 1. Generate Data
   do i = 1, 100
      x(i) = real(i, wp) * 0.1_wp
      y(i) = sin(x(i))
   end do

   ! 2. Initialize
   call plt%figure()

   ! 3. Configure
   call plt%title("System Response")
   call plt%xlabel("Time (s)")
   call plt%ylabel("Amplitude")
   call plt%grid(.true.)

   ! 4. Add Data
   call plt%add(x, y, "Sine Wave", "lines lc rgb 'blue' lw 2")

   ! 5. Output
   ! Option A: Show interactive window (requires GUI/X11)
   call plt%render()
   
   ! Option B: Save to file (works on headless servers)
   call plt%save("output/response_curve.png")

end program demo_plot

```

---

## API Reference

### Type `plotter_t`

The main opaque type handling the plotting session.

### Core Methods

#### `figure()`

**Signature:** `subroutine figure(this)`
**Description:**

* Initializes the plotting session.
* Assigns a **unique instance ID** (e.g., `1`, `2`, `3`) to avoid file conflicts if multiple plots are used.
* Checks for Gnuplot availability (only on the first call).
* Creates the `output/` directory if it doesn't exist.

#### `render()`

**Signature:** `subroutine render(this)`
**Description:**

* Generates the Gnuplot script (`output/plot_script_N.gp`).
* Executes Gnuplot to open an interactive **QT window**.
* *Behavior if Gnuplot missing:* Prints a warning and returns immediately.

#### `save(filename)`

**Signature:** `subroutine save(this, filename)`
**Description:**

* Exports the current plot configuration to a **PNG image**.
* Uses the `pngcairo` terminal for high-quality antialiased output.
* **Arguments:**
* `filename` (character): The path to save the image (e.g., `"output/results.png"`).

### Configuration Methods

All configuration methods store their settings internally so they can be applied to both `render()` (screen) and `save()` (file).

| Method | Arguments | Description |
| --- | --- | --- |
| **`title`** | `(text)` | Sets the chart main title. |
| **`xlabel`** | `(text)` | Sets the X-axis label. |
| **`ylabel`** | `(text)` | Sets the Y-axis label. |
| **`grid`** | `(logical)` | `.true.` enables the background grid. |
| **`xrange`** | `(min, max)` | Manually fixes the X-axis view window. |
| **`yrange`** | `(min, max)` | Manually fixes the Y-axis view window. |

### Data Methods

#### `add(x, y, label, style)`

**Signature:** `subroutine add(this, x, y, label, style)`
**Description:** Adds a data series to the plot.

* **`x`, `y**`: `real(wp)` arrays of equal size.
* **`label`**: Legend text for this series.
* **`style`**: Raw Gnuplot style string.
* *Examples:* `"lines"`, `"points"`, `"linespoints"`, `"lines dt 2"` (dashed).

---

## Output Files

The module generates organized files in the `output/` directory to keep your project root clean.

| File Name | Description |
| --- | --- |
| `data_plotN_M.dat` | Raw ASCII data. `N` is the plotter Instance ID, `M` is the Series ID. |
| `plot_script_N.gp` | The Gnuplot script used for the interactive window (`render`). |
| `plot_png_N.gp` | Temporary script generated during PNG export (`save`). |

**Example of Multi-Instance Output:**
If you create two `plotter_t` objects in your code, you will see:

```text
output/
├── data_plot1_1.dat   <-- Plot 1, Series 1
├── data_plot2_1.dat   <-- Plot 2, Series 1
├── plot_script_1.gp   <-- Script for Plot 1
└── plot_script_2.gp   <-- Script for Plot 2

```

---

## Troubleshooting

### "Gnuplot not found - plots will be skipped"

This message means `command -v gnuplot` failed.

* **Fix:** Install Gnuplot.
* **Impact:** Your simulation will still run, and `.dat` files will still be created in `output/`. You can plot them manually later.

### No window appears when calling `render()`

* **Cause:** You might be running on a headless server (SSH) or WSL without a window manager.
* **Fix:** Use `call plt%save("my_plot.png")` instead. This generates an image file without needing a display server.
