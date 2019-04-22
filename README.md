# A6

run `opam update` and `opam upgraed` first

## .trade File Format

### Initializtion

Example:

```
INIT: {
  balance: 10000.
  portfolio: JPM 10;AAPL 10
  date: 20090318
  path: dowjones
}
```

### Routine

Each line has to follow this format: `[verb] [ticker] [amt] [frequency] [measure] [> or <] [number]`

`[verb]` is either `buy` or `sell`

`[ticker]` is the ticket of a stock

`[amt]` is the amount of share. If `[verb]` is sell, then you can put `all` to sell all shares of that `[ticker]`

`[frequency]` is either `whenever`, `once`, or `twice`. This is the frequency you want your routine to be executed

`[measure]` is any of the measure provided in `analysis.mli`. eg. sma, skew, etc

`[> or <]` is either > or <

`[number]` is a number

Example:

```
buy aapl 50 whenever vol > 10
sell aapl all once momentum < -10
buy jpm 5 whenever sma > 10
```
