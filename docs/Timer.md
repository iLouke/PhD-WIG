# Timer Module Documentation (`timer_mod`)

## Overview

The `timer_mod` provides a high-precision stopwatch for benchmarking Fortran code. It utilizes the intrinsic `system_clock` with 64-bit integers to prevent overflow errors during long simulations.

**Key Features:**

* **Precision:** Accumulates raw clock ticks to minimize floating-point drift.
* **Live Reporting:** Can report elapsed time while the timer is still running.
* **Resumable:** Supports `start()` -> `stop()` -> `start()` sequences to measure cumulative time (e.g., inside loops).

## Usage Guide

### 1. Basic Timing

```fortran
program bench
   use timer_mod
   implicit none
   
   type(timer_t) :: stopwatch
   
   call stopwatch%start()
   
   ! ... run expensive code ...
   
   call stopwatch%stop()
   print *, "Time elapsed: ", stopwatch%report(), " seconds"
end program bench

```

### 2. Checkpointing (Live Reporting)

You do not need to stop the timer to check the time. This is useful for progress bars or logging intermediate steps.

```fortran
call stopwatch%start()

do i = 1, 1000000
   call heavy_computation()
   
   ! Log every 1000 steps
   if (mod(i, 1000) == 0) then
      print *, "Step", i, " - Elapsed:", stopwatch%report(), "s"
   end if
end do

call stopwatch%stop()

```

### 3. Accumulating Time

The timer is additive. You can measure specific parts of a loop by starting and stopping repeatedly.

```fortran
do i = 1, 100
   ! ... setup (not measured) ...
   
   call stopwatch%start()
   call matrix_multiply()  ! Only measuring this part
   call stopwatch%stop()
   
   ! ... cleanup (not measured) ...
end do

print *, "Total time spent in matrix math:", stopwatch%report()

```

---

## API Reference

### Type: `timer_t`

The main object used for timing.

### Methods

#### `start()`

**Description:** Starts the timer. If already running, this does nothing.

* **Note:** Captures the current system tick count and the clock rate.

#### `stop()`

**Description:** Pauses the timer.

* **Behavior:** Calculates the difference between now and the start tick, adds it to the `total_ticks`, and sets status to stopped.

#### `reset()`

**Description:** zeros out the accumulated time and stops the timer.

#### `report()`

**Result:** `real(wp)` (seconds)
**Description:** Returns the total elapsed time in seconds.

* **If Stopped:** Returns the accumulated time from previous runs.
* **If Running:** Returns (accumulated time + time since last start).
