# DAMPE Extension

Dampe (Debugging and Memory Protection Extension) is an extension to the System
device of Varvara, to add various features to aid debugging (especially memory
corruption, buffer overflows, and the like).

It adds a few commands to the .System/extension port:

| Command     | `.System/extension` id | `.System/extension` args |
|:-----------:|:----------------------:|:------------------------:|
| backtrace   | 0x40                   |  n/a                     |
| protect     | 0x41                   |  `ptr* len*`             |
| unprotect   | 0x42                   |  `ptr*`                  |
| priv-enter  | 0x43                   |  n/a                     |
| priv-exit   | 0x44                   |  n/a                     |
| assert-prot | 0x45                   |  `ptr*`                  |

Description of commands:

| Command     | description |
|:-----------:|:-----------:|
| backtrace   | Prints a backtrace to stdout, iterating over the return stack and converting addresses to their respective functions (and adding line/column/file info where possible). Does not empty rst. |
| protect     | Protect memory region. If region is already protected or is contained in an already-protected region, emulator prints a backtrace and exits.  |
| unprotect   | Unprotect memory region. Pointer MUST be the beginning of a previously-protected region. If the region isn't protected or it's not the beginning of the region, the emulator prints a backtrace and exits. |
| priv-enter  | Enter privileged mode, where writing to protected memory is permitted. |
| priv-exit   | Exit privileged mode. |
| assert-prot | Assert that a region of memory has been protected, and crash otherwise. |

Violation of protection results in the execution being terminated and a
backtrace printed. Additional info may be added, such as a backtrace of the
initial protection.

## Status

The extension details, like Finwë itself, is beta and subject to change at any time. No other emulators are known to implement it.
