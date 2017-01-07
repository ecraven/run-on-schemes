# run-on-schemes

Run a file on multiple Scheme implementations.

## Usage

First, add this directory to your $PATH.

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

Run `run-on-schemes name-of-script.scm`. The script will attempt to find all
schemes on your system that it knows about, and run the script on each.

## Customizing

### Adding a new Scheme to the list

Open up `run-on-schemes` and find the line `# NOTE: Add new entries here.`.

Add a new line under the list of entries, such as: `schemes[2]='some-scheme
--some-arg'`.
