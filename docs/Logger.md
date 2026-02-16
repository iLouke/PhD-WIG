# Logger Module Documentation (`logger_mod`)

## Overview

The `logger_mod` provides a flexible, tiered logging system for Fortran applications. It is designed to replace simple `print *` statements with a structured approach that supports:

* **Log Levels:** Filter messages by importance (DEBUG, INFO, WARN, ERROR).
* **Dual Output:** Simultaneously write to the console and a log file.
* **Global Access:** A shared singleton instance (`global_logger`) allows logging from any module without passing objects around.
* **Timestamps:** Automatically prefixes every message with the current date and time.
* **History:** Opens files in append mode to preserve logs across multiple program runs.

## Features

| Feature | Description |
| --- | --- |
| **Global Access** | Use `global_logger` anywhere to log events across different modules. |
| **Simplicity** | Easy-to-use API: `log%msg(LEVEL, "Text")`. |
| **Filtering** | Ignore verbose debug messages in production runs without changing code. |
| **Context** | All messages include `YYYY-MM-DD HH:MM:SS` timestamps. |
| **Resilience** | Automatically creates the `output/` directory if it is missing. |

---

## Usage Guide

### 1. Global Initialization (Recommended)

The module provides a shared instance called `global_logger`. Initialize this once in your main program, and it becomes available to all other modules.

```fortran
program main
   use logger_mod ! Imports global_logger automatically
   implicit none

   ! Initialize the global logger:
   ! 1. Filename: "app.log" (saved in output/app.log)
   ! 2. Level: LOG_INFO (Ignore DEBUG messages)
   call global_logger%init("app.log", level=LOG_INFO)

   call global_logger%msg(LOG_INFO, "Application started.")

   ! ... call other modules ...

   call global_logger%close()
end program main

```

### 2. Cross-Module Logging

Because `global_logger` is public, any module can import it and log messages without needing a logger argument.

```fortran
module my_physics_mod
   use logger_mod, only: global_logger, LOG_ERROR
   implicit none

   contains
   
   subroutine calculate()
      ! ... calculation ...
      if (failed) then
         call global_logger%msg(LOG_ERROR, "Calculation diverged!")
      end if
   end subroutine calculate

end module my_physics_mod

```

### 3. Dynamic Configuration

You can change the logger's behavior at runtime (e.g., after reading a config file).

```fortran
! Enable verbose logging
call global_logger%set_level(LOG_DEBUG)

! Mute console output (run in background/daemon mode)
call global_logger%set_console(.false.)

```

---

## API Reference

### Constants (Log Levels)

| Constant | Value | Description |
| --- | --- | --- |
| `LOG_DEBUG` | `0` | Detailed information for diagnosis. |
| `LOG_INFO` | `1` | General operational events (Startup, Progress, Shutdown). |
| `LOG_WARN` | `2` | Unexpected events that might cause issues later. |
| `LOG_ERROR` | `3` | Critical failures that may stop execution. |

### Type: `logger_t`

The main opaque type.

* **`global_logger`**: A pre-declared, `save`d instance of `logger_t` available for immediate use.

### Methods

#### `init(filename, [level], [console])`

**Description:** Opens the log file and sets initial preferences.

* `filename` (string): The name of the log file (created inside `output/`).
* `level` (integer, optional): The minimum severity to record. Default: `LOG_INFO`.
* `console` (logical, optional): Whether to print to stdout. Default: `.true.`

#### `msg(level, text)`

**Description:** Writes a message if `level` >= current threshold.

* `level` (integer): One of the `LOG_*` constants.
* `text` (string): The message content.

#### `set_level(level)`

**Description:** Updates the logging threshold.

* `level` (integer): New minimum severity.

#### `set_console(enable)`

**Description:** Toggles console output.

* `enable` (logical): `.true.` to show logs on screen, `.false.` to hide them.

#### `close()`

**Description:** Flushes buffers and closes the log file handle. **Always call this at the end of your program.**

---

## Output Format

The logger generates structured, pipe-delimited output for easy parsing.

**Console & File Format:**
`YYYY-MM-DD HH:MM:SS | [LEVEL] | Message Text`

**Example Output:**

```text
2023-10-27 14:30:01 | [INFO]  | System initialized.
2023-10-27 14:30:02 | [DEBUG] | Loaded 500 particles.
2023-10-27 14:30:05 | [WARN]  | Convergence slow at step 42.
2023-10-27 14:30:10 | [ERROR] | Division by zero detected.

```

## Best Practices

1. **Prefer `global_logger`:** Using the singleton instance avoids cluttering your subroutine arguments with logger objects.
2. **Use `LOG_DEBUG` liberally:** Put detailed math checks inside `LOG_DEBUG` calls. In production, simply init with `LOG_INFO` and these checks will have zero performance cost (they return immediately).
3. **Check `output/`:** Ensure your `.gitignore` includes the `output/` directory so you don't commit log files to version control.
