---
title: Haskell PVP FAQ
---

# Haskell Package Versioning Policy FAQ

This FAQ is intended answer questions related to aspects commonly
being brought up in discussions involving the Haskell
[PVP](http://pvp.haskell.org).

If you have a question not addressed by the FAQ entries, feel free to
post your question in the
[PVP Issue Tracker](https://github.com/haskell/pvp/issues).

## SemVer

### How does the PVP relate to [Semantic Versioning (SemVer)](http://semver.org)?

Historically, the [SemVer specification](http://semver.org) saw the light of day in late
2009, whereas the first incarnation of the PVP was already conceived 
[3 years earlier in late 2006](https://mail.haskell.org/pipermail/haskell/2006-November/018762.html).

The basic goals of SemVer and PVP are very similar, namely, to
**provide a formal specification for version numbers to make it
possible to specify version bounds for dependency management**
(c.f. [Why Use Semantic Versioning?](http://semver.org/#why-use-semantic-versioning)).

The most apparent somewhat superficial difference between SemVer and the
PVP is which components of the version number are interpreted as
major, minor, and patch-level numerals:

 - PVP: *MAJOR.MAJOR.MINOR.PATCH*
 - SemVer: *MAJOR.MINOR.PATCH*

While having a 2-part major component may confuse people used to
SemVer, it provides an additional dimension for communicating API
changes:

 - One popular way is to use the first component of the major version
   to denote "epochs" of APIs (c.f. Edward Kmett's packages) and
   allowing development (with major version increments) to continue
   independently in different epochs. This allows to avoid suffixing
   packages names with numbers to denote different epochs
   (e.g. `base2-1.0`, `base3-1.0`, `base4-1.0`).

 - A different way to see the 2-part major version component is to
   allow for nuanced signaling of large vs. not-so-large backward
   incompatible changes.

 - Yet another way is to use the first component for marketing
   purposes, e.g. to increment to "1.0" when a package is considered
   mature.

Another important difference between SemVer and PVP is that the PVP
doesn't distinguish between `0.x.y` and `1.x.y`. SemVer considers the
major version zero for initial development and allows the API to
change without requiring major version increment. The PVP, however,
*does not* provide for such an exception. This is for historical
reasons because Cabal provides a `Stability`-field in its package
meta-data for providing this kind of signalling; Unfortunately, this
facility isn't used consistently and so can't be relied upon.

Under the PVP, packages with a zero major version provide the same
contractual guarantees as versions released with a non-zero major
version.

Also, while SemVer allows for appending version tags and/or build
metadata (e.g. `1.0.0-alpha+git.5114f85`), the PVP does not regulate
nor support such additional information in version numbers.

Last but not least, SemVer is mostly language agnostic, while the PVP
is designed with Haskell and the needs of the Hackage/Cabal ecosystem
in mind, and describes the rules in terms of Haskell's specific
language facilities (module system, name resolution, typeclass
instances).

### Can we switch to using the more widely established SemVer standard instead of the less known PVP on Hackage?

While this would avoid the initial confusion for new users coming from
ecosystems where SemVer has become the standard, it would come at a
significant cost for the existing Hackage ecosystem.

Most notably, users and tooling would need to know which dependencies
follow SemVer and which are still using the PVP scheme. 

If it's to be allowed for a package to switch from PVP to SemVer, one
would also need to keep track of the version at which the transition
occurred.  Also, packages which are still using PVP-style upper bounds
would miss out on backward compatible updates during the transition,
since a PVP major version upper bound is just a minor version upper
bound in the SemVer representation.

## Upper bounds

### What is the intended meaning of upper bounds; is it "*not known* to be compatible" or rather "*known not* to be compatible"?

In the case of *not known to be compabitle*, newer Cabal versions (since 2.0)
support the caret operator (`^>=`), which is not part of the PVP spec yet.

It is described in the [cabal documentation](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-build-depends).

It allows tools to relax upper bounds more easily, where defensive bounds were put in place.

The spec so far does not make a clear decision on the meaning of upper bounds. Maintainers
may choose to not use the caret operator if they are still using an older Cabal version.
Likewise, maintainers may choose to not use any upper bounds if they have confidence in
their CI automation to detect breakages early.

However, in light of the caret operator, regular upper bounds shall mean *known not to be compabitle*.

## Hackage & Stackage

### My package is in Stackage and people can use it just fine; do I need to follow the PVP and put in version bounds?

Yes, packages uploaded to Hackage (which is a pre-requisite for being
included in Stackage) are expected to honor the PVP.

Of course there are also 3rd party distribution/curation systems, such as
Stackage or Linux distributions, which create distributions from
subsets of Hackage's repository. But the existence of such external
systems does not relieve package authors from the responsibility to
follow the PVP when publishing on Hackage.

Hackage is designed to be a self-contained package repository for
consumption by `cabal` which relies on constraint-solving for
computing install plans. Without accurate version bounds, your package
will result in poor user experience with `cabal` which won't have
accurate enough information to operate properly, as well as run into
problems such as Haddock failing to regenerate documentation due to
degrading install-plans.

## Applying the PVP

### My Package doesn't provide any API; does the PVP apply to executable-only packages?

Even if a package doesn't expose any public API, the guidelines for
API consumers which describe how to declare version constraints for
tracking dependencies still apply.

In some cases, the rules for API providers may apply as well, like for
executables such as `alex` or `happy` which generate code, and
especially when such packages are meant to be tracked as dependencies
in `build-tools`.

### My Package provides multiple APIs; how does the PVP apply to multi-library packages?

Since Cabal 3.0 and GHC 8.8, Cabal packages can have multiple public
libraries, as well as the default library with the same name as the package.

Packages that expose multiple public libraries must consider their API
to be the union of the APIs of the exposed libraries. This is the only
way that the package can obey the PVP for all consumers, since the
libraries share a version.

For example, if package P exposes library A and B, and A changes to
remove an exported entry, then the PVP says that if A was a standalone
package, then its major version of would have to increase. Since 
there is only one package and only one version for both A and B, that 
means that the major version of P itself must increase, even though 
B has not changed.

That is, the rules are the same as if A and B were both exposed via 
a single library.

### What implications does the PVP have when re-exporting API elements?

The PVP is a contract between the API provider which declares a
package version, and any API consumer depending on that package.

It's the responsibility of the provider to guarantee that the exposed
API is a function of the declared version, and in particular does not
depend on the versions of its transitive dependencies, as this would defy
the purpose of the PVP.

To avoid such issues, it's best to avoid re-exporting API elements
from other packages when it's avoidable. In cases where it's essential
to re-export API elements, try to reduce the risk by being very
defensive and control exactly what is re-exported (e.g. avoid
unconstrained module-level re-exports), and make sure to use tight
version bounds.

A related issue is conditional APIs (see next item).

### What implications does the PVP have for conditional APIs?

As already stated in the previous item, it's important for a package
version to uniquely identify the exposed API in order to fulfill its
purpose as a semantic version. So ideally, the exposed API shall be a
function of *only* its package version.

Conditionality weakens the relationship between an API and its version.
Moreover, it forces consumers of such packages to handle the resultant
ambiguity.

This obviously increases the risk for errors, due to the combinatorial
increase of configurations one needs to consider, and should therefore
be avoided.

However, it is reasonable to deviate from this recommendation when
the API conditionality is practically impossible to observe by API
consumers. An example is when API features are conditional on compiler
features which consumers are only able to use of if they depend on
that same compiler feature as well. A common example are API features
conditional on `Generic` support:

The `NFData` class is basically defined as

```hs
class NFData a where
    rnf :: a -> ()
#if HAVE_GENERICS
    default rnf :: (Generic a, GNFData (Rep a)) => a -> ()
    rnf = grnf . from
#endif
```

Consequently, `rnf`'s default implementation is conditional on the
currently available compiler features.

However, it's practically impossible to write consumer code which is
able to observe (i.e. break due to) this specific kind of API
conditionally, as you'd need to have access to the `Generic` class and
be able to define instances for it in the first place, before being
able to make use of (and thus observe) the default implementation of
`rnf`.

## Internal modules

There are various common practices of exempting some modules from PVP.
E.g.:

* Mentioning in the documentation that the module is not under PVP
* Naming module with prefix/infix/suffix component `Internal`
* Combination of the two above

PVP does not endorse or recognise such practices.
All exposed modules are subject to the uniform versioning policy.

The `Internal` modules are useful to export internals of a library, like
in `Data.Text.Internal` in [text](https://hackage.haskell.org/package/text),
however these are expected to follow versioning policy as any other modules
(and `Data.Text.Internal` does).

If author desires to keep library version churn to a minimum, yet be able
to change internals freely, then they are advised to create a separate
package for the internal API. That approach is outlined in
[Nikita Volvov's blog post "Internal convention is a mistake"](http://nikita-volkov.github.io/internal-convention-is-a-mistake/).

