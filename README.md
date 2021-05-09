# CFR-rs

CFR-rs is a rust implementation of Counterfacutual Regret Minimization[1]. 

The library calculates ε-Nash equilibrium of **all** Imperfect-Information Extensive-Form Game satisfying 
- two person
- zero sum
- perfect recall
- **relatively small**

## Quick Start

You can calculate a optimal strategy of [Kuhn poker](https://en.wikipedia.org/wiki/Kuhn_poker) in `1,000,000` steps.
```
$ cargo run --release kuhn 1000000
```

## Bench

- Rule: [Kuhn poker](https://en.wikipedia.org/wiki/Kuhn_poker), an extremely simplified form of poker.
- step: `1,000,000`
- Environments:
  - 3.2 GHz CPU
  - 8 GB RAM

The library calculates a strategy in `84.76 ±　0.35` sec.

The game value (the score that both players can achieve no matter what strategy the other player uses) of Kuhn poker is `-1/18 = -0.055555...`.

The strategy calculated above guarantees that

- player1 can achieve score `-0.055643...` at worst.
- player2 can achieve score `-0.055206...` at worst.

## Licence

[MIT](https://github.com/habara-k/cfr-rs/blob/main/LICENSE)

## Author

[habara-k](https://github.com/habara-k)

## References

[1] Zinkevich, Martin, et al. "Regret minimization in games with incomplete information." Advances in neural information processing systems 20 (2007): 1729-1736.
