










# _

SBT



## API(s)

all methods must have well-defined design -- avoid ill-defined (including the ambiguous ones) terms



## `@main`-annoted methods

use `@main` to enable CLI calls to the intended method

```

@main
def coreCompilerTestAll(): Unit = {
   ...
   ...
}

/* private ; not runnable */
def coreCompiler() : ... = ...

@main
def checkPlatformRequirements(): Unit = {
   ...
   ...
}

/* private ; not runnable */
def analysePlatformTooling() : ... = ...

@main
@targetName("help")
def printUsageDocs(): Unit = {
   ...
   ...
}

/* private ; not runnable */
def supportsLocale(locale: Locale) : ... = ...

@main
def main(args: ... ): Unit = {
   ...
   ...
}

```


## `-Yexplicit-nulls`

`-Yexplicit-nulls` will generally prevent use of `null`s


## use `a.b.c.type`-types where possible

this ensures :

```
val a : ...
val b : a.type
/* `a` and `b` guaranteed equivalent */
```


## when using `type Bar ...`, avoid `= T` where possible

`Bar` *happens to* be interchangeable with `T`, but
that's more-of due-to technical limitations than by-design --
"we don't have such specialisation yet,
it's quite hard to implement"

```

// type Args = String  // avoid this
type Args >: String <: String
// if possible :
// opaque type Args <: String = String

// type Rational = ff.TimeBase  // avoid this
type Rational >: ff.TimeBase <: ff.TimeBase

```

## avoid `trait Engine` ; use `type Engine >: EngineImpl <: EngineImpl` instead

```

// trait Engine {} // no-no

type Engine >: EngineImpl <: EngineImpl
trait EngineImpl { ... }

```

it's because (a) `trait Bar { ... }`s effectively behaves as type-alias `type Bar = ...` and hence can't be overridden and (b) `Engine` and `EngineImpl` equivalence is not by-design












