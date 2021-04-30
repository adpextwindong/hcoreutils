# Hcoreutils

coreutils RIIHaskell

## Text/shell utilities

| Name | Notes |
| --- | --- |
| [basename](src/basename.hs) | Skips implementing the -a flag |
| [echo](src/echo.hs) | Character locale handling needs testing with octal and hex values |
| [true](src/true.hs) | |
| [uniq](src/uniq.hs | WIP Following FreeBSD options |
| [wc](src/wc.hs) | Practically done except for Character locale handling and its line count is based on ByteString's idea of lines. |
| [yes](src/yes.hs) | |

I'll be going through [this list](https://wiki.debian.org/coreutils) and do whatever feels tackleable.

Inspired by [uutils/coreutils](https://github.com/uutils/coreutils).

See benchmarks in the benchmarks directory.
