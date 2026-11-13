# Measured benchmark results

Times are microseconds/op; allocation is bytes/op. ± is JMH's reported
99.9% confidence interval half-width, not standard deviation.

Current Size is a coarse baseline and does not produce exact polynomials.

| Run | Operation | Backend | n | µs/op | ± | B/op |
| --- | --- | --- | ---: | ---: | ---: | ---: |
| jmh.json | eitherBelowPair | symja | - | 2.4798 | 0.3020 | 1488.1 |
| jmh.json | pairBelowSumOfPowers | symja | - | 10.6866 | 0.6736 | 6480.3 |
| jmh.json | sumBelowCube | symja | - | 5.2809 | 0.5052 | 3072.1 |
| jmh.json | eitherPairBelowOptionPair | symja | - | 12.8194 | 0.7802 | 7664.9 |
| jmh.json | optionBelowBooleanOption | symja | - | 10.9100 | 1.2984 | 2952.3 |
| jmh.json | mixedAboveOptionPair | symja | - | 23.7032 | 5.8903 | 7021.3 |
| jmh.json | squaredEqualsExpansion | symja | - | 31.4861 | 5.7798 | 11570.7 |
| jmh.json | eitherBelowPair | rings | - | 0.0012 | 0.0002 | 0.0 |
| jmh.json | pairBelowSumOfPowers | rings | - | 0.2057 | 0.0711 | 1060.0 |
| jmh.json | sumBelowCube | rings | - | 0.0013 | 0.0003 | 0.0 |
| jmh.json | eitherPairBelowOptionPair | rings | - | 0.1504 | 0.0407 | 992.0 |
| jmh.json | optionBelowBooleanOption | rings | - | 0.1456 | 0.0259 | 864.0 |
| jmh.json | mixedAboveOptionPair | rings | - | 0.2922 | 0.0570 | 1728.0 |
| jmh.json | squaredEqualsExpansion | rings | - | 0.3104 | 0.0241 | 2592.0 |
| jmh.json | eitherBelowPair | model | - | 0.0013 | 0.0000 | 0.0 |
| jmh.json | pairBelowSumOfPowers | model | - | 0.0019 | 0.0001 | 0.0 |
| jmh.json | sumBelowCube | model | - | 0.0014 | 0.0002 | 0.0 |
| jmh.json | eitherPairBelowOptionPair | model | - | 0.0016 | 0.0000 | 0.0 |
| jmh.json | optionBelowBooleanOption | model | - | 0.0017 | 0.0002 | 0.0 |
| jmh.json | mixedAboveOptionPair | model | - | 0.0017 | 0.0000 | 0.0 |
| jmh.json | squaredEqualsExpansion | model | - | 0.0018 | 0.0001 | 0.0 |
| jmh.json | eitherBelowPair | current | - | 0.0021 | 0.0000 | 0.0 |
| jmh.json | pairBelowSumOfPowers | current | - | 0.0026 | 0.0003 | 0.0 |
| jmh.json | sumBelowCube | current | - | 0.0025 | 0.0001 | 0.0 |
| jmh.json | eitherPairBelowOptionPair | current | - | 0.0023 | 0.0002 | 0.0 |
| jmh.json | optionBelowBooleanOption | current | - | 0.0043 | 0.0011 | 0.0 |
| jmh.json | mixedAboveOptionPair | current | - | 0.0024 | 0.0001 | 0.0 |
| jmh.json | squaredEqualsExpansion | current | - | 0.0016 | 0.0002 | 0.0 |
| jmh.json | binomial | symja | 8 | 1.6221 | 0.0281 | 1160.0 |
| jmh.json | product | symja | 8 | 66.0390 | 0.9280 | 34889.6 |
| jmh.json | sum | symja | 8 | 76.6669 | 2.5835 | 44881.8 |
| jmh.json | render | symja | 8 | 0.2265 | 0.0075 | 232.0 |
| jmh.json | binomial | symja | 64 | 1.6926 | 0.2247 | 1160.0 |
| jmh.json | product | symja | 64 | 771.3013 | 55.0883 | 443802.1 |
| jmh.json | sum | symja | 64 | 3593.9610 | 597.5722 | 1759459.9 |
| jmh.json | render | symja | 64 | 0.2187 | 0.0250 | 232.0 |
| jmh.json | binomial | rings | 8 | 0.4054 | 0.0179 | 4368.0 |
| jmh.json | product | rings | 8 | 0.9212 | 0.0195 | 9184.0 |
| jmh.json | sum | rings | 8 | 0.8455 | 0.0766 | 7224.0 |
| jmh.json | render | rings | 8 | 0.2555 | 0.0069 | 1744.0 |
| jmh.json | binomial | rings | 64 | 20.4819 | 1.2508 | 194960.5 |
| jmh.json | product | rings | 64 | 50.2162 | 1.1627 | 478961.2 |
| jmh.json | sum | rings | 64 | 7.8396 | 0.4475 | 67296.0 |
| jmh.json | render | rings | 64 | 3.9489 | 0.3398 | 31280.1 |
| jmh.json | binomial | model | 8 | 0.1293 | 0.0125 | 224.0 |
| jmh.json | product | model | 8 | 0.3900 | 0.1610 | 836.0 |
| jmh.json | sum | model | 8 | 0.5197 | 0.0782 | 2108.0 |
| jmh.json | render | model | 8 | 0.0212 | 0.0065 | 160.0 |
| jmh.json | binomial | model | 64 | 0.4007 | 0.1240 | 1408.0 |
| jmh.json | product | model | 64 | 6.4103 | 1.3264 | 32944.2 |
| jmh.json | sum | model | 64 | 3.9897 | 0.8007 | 13096.1 |
| jmh.json | render | model | 64 | 0.1194 | 0.0095 | 832.0 |
| jmh.json | binomial | current | 8 | 0.0416 | 0.0049 | 96.0 |
| jmh.json | product | current | 8 | 0.2964 | 0.0432 | 488.0 |
| jmh.json | sum | current | 8 | 0.4906 | 0.0884 | 1056.0 |
| jmh.json | render | current | 8 | 0.0336 | 0.0050 | 224.0 |
| jmh.json | binomial | current | 64 | 0.0377 | 0.0037 | 96.0 |
| jmh.json | product | current | 64 | 1.8962 | 0.4511 | 2636.0 |
| jmh.json | sum | current | 64 | 5.9926 | 3.3152 | 14404.1 |
| jmh.json | render | current | 64 | 0.0282 | 0.0094 | 224.0 |
| jmh.json | finitePower | symja | 64 | 0.1778 | 0.0323 | 280.0 |
| jmh.json | finitePower | symja | 4096 | 1.4431 | 0.3215 | 2968.0 |
| jmh.json | finitePower | rings | 64 | 0.0784 | 0.0082 | 592.0 |
| jmh.json | finitePower | rings | 4096 | 2.0179 | 0.1716 | 3640.0 |
| jmh.json | finitePower | model | 64 | 0.4396 | 0.0542 | 592.0 |
| jmh.json | finitePower | model | 4096 | 1.7216 | 0.6106 | 4000.0 |
| jmh.json | finitePower | current | 64 | 0.0474 | 0.0038 | 208.0 |
| jmh.json | finitePower | current | 4096 | 0.0307 | 0.0017 | 104.0 |
| jmh.json | finitePower | natural | 64 | 0.0199 | 0.0021 | 136.0 |
| jmh.json | finitePower | natural | 4096 | 0.0397 | 0.0006 | 640.0 |
| jmh.json | eitherStrings | symja | - | 2.0312 | 0.2301 | 1332.0 |
| jmh.json | optionString | symja | - | 1.8035 | 0.1654 | 1328.0 |
| jmh.json | pairStrings | symja | - | 4.3501 | 0.3764 | 2600.1 |
| jmh.json | pairOptions | symja | - | 6.2874 | 0.9915 | 3736.1 |
| jmh.json | tripleOptions | symja | - | 7.2252 | 0.7595 | 3764.2 |
| jmh.json | booleanOption | symja | - | 4.4089 | 0.7536 | 2416.1 |
| jmh.json | eitherStringPair | symja | - | 7.5497 | 1.3266 | 3808.2 |
| jmh.json | optionEither | symja | - | 5.9490 | 0.4995 | 3216.1 |
| jmh.json | eitherStrings | rings | - | 0.0478 | 0.0078 | 344.0 |
| jmh.json | optionString | rings | - | 0.0885 | 0.0200 | 552.0 |
| jmh.json | pairStrings | rings | - | 0.1019 | 0.0070 | 448.0 |
| jmh.json | pairOptions | rings | - | 0.1759 | 0.0277 | 864.0 |
| jmh.json | tripleOptions | rings | - | 0.2535 | 0.0341 | 1112.0 |
| jmh.json | booleanOption | rings | - | 0.1247 | 0.0351 | 552.0 |
| jmh.json | eitherStringPair | rings | - | 0.1350 | 0.0246 | 624.0 |
| jmh.json | optionEither | rings | - | 0.1931 | 0.0234 | 832.0 |
| jmh.json | eitherStrings | model | - | 0.0090 | 0.0011 | 56.0 |
| jmh.json | optionString | model | - | 0.0104 | 0.0037 | 56.0 |
| jmh.json | pairStrings | model | - | 0.0133 | 0.0067 | 56.0 |
| jmh.json | pairOptions | model | - | 0.0134 | 0.0076 | 56.0 |
| jmh.json | tripleOptions | model | - | 0.0175 | 0.0100 | 56.0 |
| jmh.json | booleanOption | model | - | 0.0186 | 0.0096 | 56.0 |
| jmh.json | eitherStringPair | model | - | 0.0124 | 0.0066 | 56.0 |
| jmh.json | optionEither | model | - | 0.0295 | 0.0362 | 56.0 |
| jmh.json | eitherStrings | current | - | 0.1181 | 0.1026 | 296.0 |
| jmh.json | optionString | current | - | 0.0958 | 0.0883 | 288.0 |
| jmh.json | pairStrings | current | - | 0.0656 | 0.0374 | 224.0 |
| jmh.json | pairOptions | current | - | 0.0562 | 0.0410 | 224.0 |
| jmh.json | tripleOptions | current | - | 0.0402 | 0.0084 | 224.0 |
| jmh.json | booleanOption | current | - | 0.0435 | 0.0094 | 224.0 |
| jmh.json | eitherStringPair | current | - | 0.0582 | 0.0307 | 296.0 |
| jmh.json | optionEither | current | - | 0.0523 | 0.0176 | 224.0 |

## Physical source size

Code lines exclude blank and comment-only lines, but include imports and braces.
No attempt is made to treat a whole upstream library as equivalent adapter code.

| Source | Physical lines | Code lines |
| --- | ---: | ---: |
| bench-algebra/src/main/scala/cardinality/bench/algebra/Algebra.scala | 131 | 87 |
| bench-algebra/src/main/scala/cardinality/bench/algebra/CompareBench.scala | 46 | 35 |
| bench-algebra/src/main/scala/cardinality/bench/algebra/ConstructBench.scala | 44 | 32 |
| bench-algebra/src/main/scala/cardinality/bench/algebra/CurrentAlgebra.scala | 20 | 13 |
| bench-algebra/src/main/scala/cardinality/bench/algebra/DependencyMeasure.scala | 60 | 54 |
| bench-algebra/src/main/scala/cardinality/bench/algebra/FiniteBench.scala | 43 | 32 |
| bench-algebra/src/main/scala/cardinality/bench/algebra/ModelAlgebra.scala | 92 | 66 |
| bench-algebra/src/main/scala/cardinality/bench/algebra/ProjectBench.scala | 47 | 35 |
| bench-algebra/src/main/scala/cardinality/bench/algebra/RingsAlgebra.scala | 48 | 36 |
| bench-algebra/src/main/scala/cardinality/bench/algebra/SymjaAlgebra.scala | 49 | 37 |
| bench-algebra/src/main/scala/cardinality/bench/algebra/Verify.scala | 213 | 176 |
| core/src/main/scala/cardinality/analysis/inhabitation/Natural.scala | 334 | 246 |
| core/src/main/scala/cardinality/capacity/Size.scala | 341 | 179 |
