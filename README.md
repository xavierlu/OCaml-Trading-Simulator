# A6

run `opam update` and `opam upgraed` first

## .trade File Format

### Initializtion

```
INIT: {

}
```

Example:

```
INIT: {

}
```

### Routine

Each line has to follow this format: `[verb] [ticker] [amt] [frequency] [measure] [> or <] [amt]`
`[verb]` is either `buy` or `sell`. Type: string
`[ticker]` is the ticket of a stock. Type: string
`[amt]` is the amount of share. Type: int
`[frequency]` is either `whenever`, `once`, or `twice`. This is the frequency you want your routine to be executed. Type: string
`[measure]` is any of the measure provided in `analysis.mli`. eg. sma, skew, etc. Type: string
`[> or <]` is either > or <. Type: string
`[amt]` is an integer. Type: int
