# CFR-rs

![Build Status](https://github.com/habara-k/cfr-rs/actions/workflows/rust.yml/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


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
$ cargo run --release --bin main src/rule/kuhn.json 1000000
```

You can also calculate an ε-Nash equilibrium strategy for [Glico](https://ja.wikipedia.org/wiki/%E3%82%B0%E3%83%AA%E3%82%B3_(%E9%81%8A%E3%81%B3))(Weighted Rock Paper Scissors)
```
$ cargo run --release --bin main src/rule/glico.json 1000000
```

## Bench

- Rule: [Kuhn poker](https://en.wikipedia.org/wiki/Kuhn_poker), an extremely simplified form of poker.
- step: `1,000,000`
- Environments:
  - 3.2 GHz CPU
  - 8 GB RAM

The library calculates a strategy in `17.3 ± 0.1` sec.

The ε value of ε-Nash equilibrium decreases in `O(1/√step)`.
<p align="center">
<img src="https://user-images.githubusercontent.com/34413567/121806896-b9d27f80-cc8c-11eb-876d-7b71f99aef62.png" width="600">
</p>

## Doc

```
$ cargo doc --no-deps --open
```

## Licence

[MIT](https://github.com/habara-k/cfr-rs/blob/main/LICENSE)

## Author

[habara-k](https://github.com/habara-k)

## References

[1] Zinkevich, Martin, et al. "Regret minimization in games with incomplete information." Advances in neural information processing systems 20 (2007): 1729-1736.
