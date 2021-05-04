# CFR-rs

CFR-rs is a rust implementation of Counterfacutual Regret Minimization[1]. 

The library calculates ε-Nash equilibrium of all Imperfect-Information Extensive-Form Game satisfying 
- two person
- zero sum
- perfect recall
- **relatively small**

## Bench

[Kuhn poker](https://en.wikipedia.org/wiki/Kuhn_poker) is an extremely simplified form of poker.

Its game value (the score that both players can achieve no matter what strategy the other player uses.) is `-1/18 = -0.055555...`.

The library calculates a strategy with which 

- player1 can achieve score `-0.055643...` 
- player2 can achieve score `-0.055206...`

no matter what strategy the other player uses, in 75.711[sec], with 3.2 GHz CPU and 8GB RAM.


## Licence

[MIT](https://github.com/habara-k/cfr-rs/blob/main/LICENSE)

## Author

[habara-k](https://github.com/habara-k)

## References

[1] Zinkevich, Martin, et al. "Regret minimization in games with incomplete information." Advances in neural information processing systems 20 (2007): 1729-1736.
