# Benchmarking `Data.Vector.Storable` with `weigh`

See the accompanying blog post [Reasoning about Haskell memory usage: What is the overhead of a `Storable` Vector?](/posts/Haskell-Storable-Vector-overhead.md)


## Usage

```sh
stack bench
```


## Example output

```
Data.Vector of singleton Storable Vectors

  Case                                      Allocated  GCs         Live  Check          Max          MaxOS
  singleton Storable Vectors  1                   144    0          528  OK             528              0
  singleton Storable Vectors  10                  864    0        1,032  OK           1,032              0
  singleton Storable Vectors  100              12,152    0       10,160  OK          10,160              0
  singleton Storable Vectors  1000            104,560    0       80,968  OK          80,968              0
  singleton Storable Vectors  10000         1,040,872    0      801,280  OK         801,280              0
  singleton Storable Vectors  100000       10,399,880    9    8,000,288  OK       8,000,288     12,582,912
  singleton Storable Vectors  1000000     104,006,432   92   80,006,840  OK      80,006,840    135,266,304
  singleton Storable Vectors  10000000  1,040,080,112  921  800,080,520  OK     800,080,520  1,428,160,512
```
