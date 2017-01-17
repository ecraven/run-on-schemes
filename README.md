# run-on-schemes

Run a file on multiple Scheme implementations.

## Basic Usage

First, add `$SOME_PATH/run-on-schemes/bin` to your $PATH.

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

Run `run-on-schemes run name-of-script.scm`. The script will attempt to find all
schemes on your system that it knows about, and run the script on each.

## Advanced Usage

If you would like to pass your own TSV file containing a list of schemes, use
the `-f` argument.

```
run-on-schemes run -f list-of-schemes.scm name-of-script.scm
```

If you would like to choose only certain schemes to run, use the `-i` argument.

```
run-on-schemes run -i chicken name-of-script.scm
run-on-schemes run -i 'chibi,chicken' name-of-script.scm
```

## Customizing

### Adding a new Scheme to the list

Open up `$SOME_PATH/run-on-schemes/share/default-schemes` and add a new line
containing a definition for the scheme.
