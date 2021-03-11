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
