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

Run `run-on-schemes -i name-of-script.scm`. The script will attempt to find all
schemes on your system that it knows about, and run the script on each.

## Advanced Usage

If you would like to pass your own TSV file containing a list of schemes, use
the `-f` argument.

```
run-on-schemes -f list-of-schemes.tsv
```

If you would like to choose only certain schemes to run, use the `-l` argument.

```
run-on-schemes -l chicken
run-on-schemes -l 'chibi chicken'
```

## Customizing

### Adding a new Scheme to the list

Open up `$SOME_PATH/run-on-schemes/share/default-schemes` and add a new line
containing a tab-separated list of a name, a command name, and a full command
for running.

For example, suppose you want to add a hypothetical Scheme called `super-scheme`
with a command called `supers` and in order to run a script with it, you would
need to call it with an argument, `-s`. Then the entry would look like:

```
super-scheme	supers	supers -s
```
