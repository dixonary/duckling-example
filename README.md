# duckling-example

A small example of how to use the `duckling` library to parse times and numbers. 
Of course it can do *much* more than that, but this should get you started!

Note that this will only work on linux at the moment, and only on distributions
which store timezone data in Olson files under the prefix `/usr/share/zoneinfo/`.

If your distribution or operating system stores time zone data in an alternative
format, you will need to adapt the `main` function to the system in question.