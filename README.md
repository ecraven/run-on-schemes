# run-on-schemes

Run a file on multiple Scheme implementations.

## Basic Usage

Next, figure out what Scheme standard you wish to target. For example, R7RS.

Write a Scheme script intended to target multiple Schemes. For example:

```scheme
(cond-expand
  (chibi
    (import
      (scheme base)
      (scheme small)))
  (chicken #f))

(display "Hello, world!")
(newline)
```

Run `run-on-schemes.sh run name-of-script.scm`. The script will attempt to find all
schemes on your system that it knows about, and run the script on each.

## Advanced Usage

If you would like to choose only certain schemes to run, use the `-i` argument.

```
run-on-schemes run -i chicken name-of-script.scm
run-on-schemes run -i 'chibi,chicken' name-of-script.scm
```
