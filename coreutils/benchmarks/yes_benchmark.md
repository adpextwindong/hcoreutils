In the spirit of [this post](https://www.reddit.com/r/unix/comments/6gxduc/how_is_gnu_yes_so_fast/) lets benchmark our first version.

GNU coreutils yes version 8.28 on my system gets this speed.

```
λ yes | pv -a > /dev/null
[4.90GiB/s]
```

While my Haskell version (commit 7d61da2) gets

```
λ ./dist/build/yes/yes | pv -a > /dev/null
[6.91MiB/s]
```

Obviously theres some performance still on the table.

After switching from String's to Text its actually slower... Around 4.46MiB/s oddly enough.
But adding cli args the rate jumps up so at this point I'm thinking we better look into buffering the output.

--------------------------------------------------------------------------------

Benchmarking the OpenBSD version of yes.c w/ the pledge stuff commented out we get this on my cygwin env.

```
λ ./yes_openbsd.exe | pv -a > /dev/null
[7.95MiB/s]
```

TODO we should look into the allocs and write syscalls being done

--------------------------------------------------------------------------------

After switching to Data.Text packed and buffering the stdout using System.IO hSetBuffering w/ 4096 as the block size we get this on the windows machine:

```
3.13GiB 0:01:10 [45.7MiB/s]
```

--------------------------------------------------------------------------------

Switched the args version to pack the message into nearly a block size.
Oddly enough I've found some local optima for the args version.

```
λghc --make -O3 src/yes.hs && src/yes.exe heres the messagedwa | pv > /dev/null
[1 of 1] Compiling Main             ( src\yes.hs, src\yes.o )
Linking src\yes.exe ...
1.23GiB 0:00:16 [  75MiB/s] [                           <=>
```
