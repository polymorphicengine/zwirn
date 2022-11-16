# minilambda

An experimental investigation into extending the Tidal Mini-Notation to a Lambda-Calculus style programming language

TODO:

- better represent alternations by a list of terms and pattern match via x;xs for example
- make everything a sequence by also allowing TEmpty
- pattern match with nil for TEmpty
- pattern match more complex patterns like the sequence (x nil) to match against single item sequences
- stick with lazy evaluation even when pattern matching (therefor discurage the usage of multiple patterns that can resolve against each other)
- introduce bangs to force evaluation of certain terms on demand
