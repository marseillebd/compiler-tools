This proposal suggests dropping supports for signed zero in integer literals.
The notations `+0` and `-0`, currently distinct, would now be equivalent, now also equivalent to `0`, whose meaning remains unchanged.

> Rationale
>
> Signed zero in modern computing is limited to dedicated numerical analysis using floating point numbers.
> (Sign-magnitude and ones-complement representations are obsolete and have, as far as I am aware, left no trace in the processors manufactored today.)
> I beleive it is also fairly obscure in mathematics, mostly being used in counterexamples to intuitive propositions.
>
> Thus, we expect most users of CSS to immediately discard the distinction.
> That means boring downstream work.
> Meanwhile, signed zero continues to be availablle through floating-pint literals.
>
> The benefits of the availability of a signed integer zero do not outweigh the maintenance costs,
>   both in implementing CCS, and in using CCS downstream.

> Alternative
>
> A combinator in the recognizer library that unifies the two.
> Downstream simply selects the appropriate int-recognizer, either `int` or `intSignedZero`.
