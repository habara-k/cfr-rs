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

This library reads all the rules of a game from a JSON file. 
The implementation is not specific to any particular game, so you can analyze any game that meets the above conditions.

## Quick Start

You can calculate an ε-Nash equilibrium strategy for [Kuhn poker](https://en.wikipedia.org/wiki/Kuhn_poker) in `1,000,000` steps.
```
$ cargo build --release --bin main
$ RUST_LOG=debug target/release/main --rule src/rule/kuhn.json --step 1000000
```


## Benchmark

- Rule: [Kuhn poker](https://en.wikipedia.org/wiki/Kuhn_poker), an extremely simplified form of poker.
- Step: `1,000,000`
- Environments:
  - 3.2 GHz CPU
  - 8 GB RAM

The library calculates a strategy in `16.8 ± 0.7` sec.

The value of NashConv, A common metric for determining the rates of convergence, decreases in `O(1/√step)`.
<p align="center">
<img src="https://user-images.githubusercontent.com/34413567/142921379-98354320-5793-4457-bf18-d900c046475a.png" width="600">
</p>

## Documents

[doc](https://habara-k.github.io/cfr-rs/cfr_rs/)

## Licence

[MIT](https://github.com/habara-k/cfr-rs/blob/main/LICENSE)

## Author

[habara-k](https://github.com/habara-k)

## References

[1] Zinkevich, Martin, et al. "Regret minimization in games with incomplete information." Advances in neural information processing systems 20 (2007): 1729-1736.
