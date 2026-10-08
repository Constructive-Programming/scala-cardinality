# Polymorphic functions and type lambdas: the supported fragment

This is implementation-cardinality support, not a stored-value estimate. The pure,
total, parametric assumptions and observational equivalence in the
[v1 counting contract](../plans/v1.md#_2-agreed-counting-contract) apply.

## First-order beta application

The method resolver supports applying an unbounded type lambda to first-order,
resolvable arguments, for example `([x] =>> (x, x))[A]`. Transparent named aliases
whose body is a lambda and which have no outer alias parameters can also be applied:
`type F = [x] =>> Option[x]`, followed by `F[A]`.

The argument is resolved in its caller's environment before extending the lambda's
lexical environment. This is substitution of resolved shapes, not textual identifier
replacement. Consequently a caller's `A` cannot be captured by a lambda binder spelled
`A`; nested binders shadow only their own lexical name. Alias bodies are resolved in
the alias declaration's scope, not the application's scope.

This does not implement arbitrary higher-kinded substitution. Unapplied lambdas,
bounded/higher-kinded lambda parameters, and aliases with both outer parameters and a
lambda body remain unresolved. Unknown external constructors such as `ZIO[R,E,x]`
are not assumed to be structural containers. A reducible lambda cannot make unknown
constructor semantics known.

## Closed polymorphic identity

The only supported first-class polymorphic function type is the syntactically closed,
unbounded `[a] => a => a`, modulo binder renaming. Under the stated assumptions its
unique observational inhabitant is identity, as defended by D3 in the
[evidence register](code-cardinality-foundations.md#d3-parametricity-constrains-generic-implementations).
The resolver represents this particular type by the terminal shape.

That representation is a proven canonicalization, not quantifier erasure:
instantiating and applying identity returns its argument, so it adds neither another
choice nor a seeded iteration family. For example, a method receiving this identity
and two `A` inputs still has exactly two ways to return `A`; receiving identity alone
does not supply any arbitrary `A`. A result of the identity type has one choice.

All other first-class polymorphic function types are explicitly unresolved.
`[a] => (a, a) => a` is **not** treated as a monomorphic two-selector producer.
General rank-n counting requires quantified introduction, instantiation, and
observational normalization rules beyond the current inhabitation solver.

The polymorphic body is still resolved before rejecting unsupported shapes. A nested
context function retains its evidence-analysis diagnostic, and an unknown external
dependency retains its resolution diagnostic. In particular,
`[b] => b => Type[b] ?=> Expr[Any]` is not counted and no macro/evidence code is run.
Incomplete accessible environments continue to block numeric answers even when the
result itself is the closed identity type.

## Executable evidence

`core/src/test/scala/cardinality/analysis/resolution/PolyLambdaCardinalitySpec.scala` pins beta substitution, capture
avoidance, nested binder shadowing, method and constructor use, identity introduction
and observationally inert application, and unresolved evidence/dependency boundaries.
These tests establish this fragment, not general rank-n or higher-kinded support.
