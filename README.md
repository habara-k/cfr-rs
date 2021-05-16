# CFR-rs

CFR-rs is a rust implementation of Counterfactual Regret Minimization[1]. 

The library calculates ε-Nash equilibrium of **all** Imperfect-Information Extensive-Form Game satisfying 
- finite
- two-person
- zero-sum
- perfect recall
- **relatively small**

## Quick Start

You can calculate an ε-Nash equilibrium strategy for [Kuhn poker](https://en.wikipedia.org/wiki/Kuhn_poker) in `1,000,000` steps.
```
$ cargo run --release --bin main kuhn 1000000
```

You can also calculate an ε-Nash equilibrium strategy for [Glico(Weighted Rock Paper Scissors)](https://ja.wikipedia.org/wiki/%E3%82%B0%E3%83%AA%E3%82%B3_(%E9%81%8A%E3%81%B3)) :)
```
$ cargo run --release --bin main glico 1000000
```

## Bench

- Rule: [Kuhn poker](https://en.wikipedia.org/wiki/Kuhn_poker), an extremely simplified form of poker.
- step: `1,000,000`
- Environments:
  - 3.2 GHz CPU
  - 8 GB RAM

The library calculates a strategy in `17.3 ± 0.2` sec.

The game value (the score that both players can achieve no matter what strategy the other player uses) of Kuhn poker is `-1/18 = -0.055555...`.

The strategy calculated above guarantees that

- player1 can achieve a score of `-0.055644...` at worst.
- player2 can achieve a score of `-0.055206...` at worst.

## Licence

[MIT](https://github.com/habara-k/cfr-rs/blob/main/LICENSE)

## Author

[habara-k](https://github.com/habara-k)

## References

[1] Zinkevich, Martin, et al. "Regret minimization in games with incomplete information." Advances in neural information processing systems 20 (2007): 1729-1736.
