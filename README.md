Primality Tests in Scala
========================
The lack of primality tests in Scala led us to make publicly available our implementation of the following:

* The Solovay-Strassen test.
* The Miller-Rabin test.
* The AKS algorithm, which is the first algorithm in P for Primality.

From these algorithms the Miller-Rabin test is by far faster than AKS
and has a higher probability of correctly predicting Primality than the Solovay-Strassen test.

*Note*: The AKS algorithm we implemented does not yet show performance in P for prime numbers from our empirical results.
Still, we think that the tests and our implementation of polynomials may be used to perform some experiments
and perhaps serve as basis to implement AKS in P.
