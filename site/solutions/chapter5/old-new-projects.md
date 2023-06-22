---
chapter: 5
exercise-id: 2
name: Old New Projects
summary: "
Apply what you've learned about creating new Haskell projects with this exercise
by turning all of your earlier example code into proper stand-aloen projects.
"
---

## Old New Projects {.problem}

Use cabal and create projects for all of the examples that you've already worked
on as you've been working through this book. Consider how you might organize the
modules to maximize re-use in cases where we worked through several variations
of a single example.

### Hints

<div class="hints">
<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
There aren't any tricks to this question. This is a chance for you to get some
practice working with cabal files, and to get in the habit of creating proper
projects before you move on to the larger examples in the rest of the book. Feel
free to look at the solution for a large example of a cabal file, or refer to
any of the examples in the chapter.
</div>
</div>
</details>
</div>

### Solution

<div class="solution">

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

Here's a complete example of the set cabal file that was used to build all of
the examples and tests for Effective Haskell. You'll notice that in cases where
we have many variations of an example, we create modules with names like `V1`,
`V2` and so on. These modules can often import shared code that doesn't need to
change between versions, allowing us to reduce the amount of rework that happens
between multiple versions of the code. In the real world you'd likely use
version control for these sorts of incremental updates, but having multiple
versions of an API can be a useful way of preserving backwards compatibility.

```cabal
cabal-version: 3.0

name:           effective-haskell-examples
version:        0
license:        BSD-2-Clause
license-file:   LICENSE
build-type:     Simple

library
  exposed-modules:
      EffectiveHaskell
      EffectiveHaskell.Chapter1
      EffectiveHaskell.Chapter1.Branches
      EffectiveHaskell.Chapter1.ComposingFunctions
      EffectiveHaskell.Chapter1.LetBindings
      EffectiveHaskell.Chapter1.Looping
      EffectiveHaskell.Chapter1.OperatorExample
      EffectiveHaskell.Chapter1.OperatorExample.InfixL
      EffectiveHaskell.Chapter1.OperatorExample.InfixL6
      EffectiveHaskell.Chapter1.OperatorExample.InfixL7
      EffectiveHaskell.Chapter1.OperatorExample.InfixL8
      EffectiveHaskell.Chapter1.OperatorExample.InfixR0
      EffectiveHaskell.Chapter1.OperatorExample.InfixR6
      EffectiveHaskell.Chapter1.OperatorExample.InfixR7
      EffectiveHaskell.Chapter1.OperatorExample.InfixR8
      EffectiveHaskell.Chapter1.OperatorExample.InfixR9
      EffectiveHaskell.Chapter1.Precedence
      EffectiveHaskell.Chapter1.Salutation.Version1
      EffectiveHaskell.Chapter1.Salutation.Version2
      EffectiveHaskell.Chapter10
      EffectiveHaskell.Chapter10.MetricsV1
      EffectiveHaskell.Chapter10.MetricsV2
      EffectiveHaskell.Chapter10.V1
      EffectiveHaskell.Chapter10.V2
      EffectiveHaskell.Chapter11
      EffectiveHaskell.Chapter11.ExistentialDemo
      EffectiveHaskell.Chapter11.V1
      EffectiveHaskell.Chapter11.V2
      EffectiveHaskell.Chapter11.V3
      EffectiveHaskell.Chapter11.V4
      EffectiveHaskell.Chapter12
      EffectiveHaskell.Chapter12.V1
      EffectiveHaskell.Chapter12.V2
      EffectiveHaskell.Chapter12.V3
      EffectiveHaskell.Chapter12.V4
      EffectiveHaskell.Chapter12.V5
      EffectiveHaskell.Chapter13
      EffectiveHaskell.Chapter13.Archiver
      EffectiveHaskell.Chapter13.ClassyArchiver
      EffectiveHaskell.Chapter13.EitherIO
      EffectiveHaskell.Chapter13.ExceptState
      EffectiveHaskell.Chapter13.ExceptT
      EffectiveHaskell.Chapter13.ExceptTParser
      EffectiveHaskell.Chapter13.FailingStatefulParser
      EffectiveHaskell.Chapter13.Identity
      EffectiveHaskell.Chapter13.MonadError
      EffectiveHaskell.Chapter13.MonadIO
      EffectiveHaskell.Chapter13.MonadIODemo
      EffectiveHaskell.Chapter13.MonadState
      EffectiveHaskell.Chapter13.MonadState.V1
      EffectiveHaskell.Chapter13.MonadStateDemo
      EffectiveHaskell.Chapter13.MonadStateDemo.V1
      EffectiveHaskell.Chapter13.MonadStateDemo.V2
      EffectiveHaskell.Chapter13.MonadStateDemo.V3
      EffectiveHaskell.Chapter13.MonadStateDemo.V4
      EffectiveHaskell.Chapter13.MonadTrans
      EffectiveHaskell.Chapter13.MonadTrans.V1
      EffectiveHaskell.Chapter13.MonadTrans.V2
      EffectiveHaskell.Chapter13.State
      EffectiveHaskell.Chapter13.StateExcept
      EffectiveHaskell.Chapter13.StateParser
      EffectiveHaskell.Chapter13.StateT
      EffectiveHaskell.Chapter14
      EffectiveHaskell.Chapter14.SpellCheck
      EffectiveHaskell.Chapter14.SpellCheck.ListMemo
      EffectiveHaskell.Chapter14.SpellCheck.LowLevelUnboxed
      EffectiveHaskell.Chapter14.SpellCheck.Naive
      EffectiveHaskell.Chapter14.SpellCheck.STMemo
      EffectiveHaskell.Chapter14.SpellCheck.STVec
      EffectiveHaskell.Chapter14.SpellCheck.Types
      EffectiveHaskell.Chapter14.SpellCheck.Types.V1
      EffectiveHaskell.Chapter14.SpellCheck.VectorDemo
      EffectiveHaskell.Chapter15
      EffectiveHaskell.Chapter15.ClosedTypeFamilyDemo
      EffectiveHaskell.Chapter15.ColorDemo
      EffectiveHaskell.Chapter15.CommandRunner
      EffectiveHaskell.Chapter15.CommandRunner.V1
      EffectiveHaskell.Chapter15.CommandRunner.V2
      EffectiveHaskell.Chapter15.GADTs.V1
      EffectiveHaskell.Chapter15.GADTShellCmd
      EffectiveHaskell.Chapter15.OpenDataFamiliesDemo
      EffectiveHaskell.Chapter15.OpenDataFamiliesDemo.V1
      EffectiveHaskell.Chapter15.ShellCommand.V1
      EffectiveHaskell.Chapter15.ShellCommand.V2
      EffectiveHaskell.Chapter15.ShellCommand.V3
      EffectiveHaskell.Chapter15.TypeFamilyListFuncs
      EffectiveHaskell.Chapter15.TypeFamilyListFuncs.V1
      EffectiveHaskell.Chapter15.TypeFamilyListFuncs.V2
      EffectiveHaskell.Chapter15.TypeLevelList
      EffectiveHaskell.Chapter2
      EffectiveHaskell.Chapter2.Fibs
      EffectiveHaskell.Chapter2.Fibs.V1
      EffectiveHaskell.Chapter2.Fibs.V2
      EffectiveHaskell.Chapter3
      EffectiveHaskell.Chapter3.PolymorphicFunctions
      EffectiveHaskell.Chapter3.TypeErrors
      EffectiveHaskell.Chapter3.WritingTypeAnnotationsForFunctions
      EffectiveHaskell.Chapter4
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecords
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecords.Aliases
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecords.Calculator
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecords.DuplicateFields
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecords.Lists
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecords.Parser
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecords.Peano
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecords.PolymorphicTypes
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecords.Records
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecords.SumTypes
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecords.Wildcards
      EffectiveHaskell.Chapter5
      EffectiveHaskell.Chapter6
      EffectiveHaskell.Chapter6.AdHocPolymorphism
      EffectiveHaskell.Chapter6.AdHocPolymorphism.ClassNatural
      EffectiveHaskell.Chapter6.AdHocPolymorphism.Deriving
      EffectiveHaskell.Chapter6.AdHocPolymorphism.Deriving.Derived
      EffectiveHaskell.Chapter6.AdHocPolymorphism.Deriving.ManualInstances
      EffectiveHaskell.Chapter6.AdHocPolymorphism.DerivingVia.Manual
      EffectiveHaskell.Chapter6.AdHocPolymorphism.DerivingVia.Via
      EffectiveHaskell.Chapter6.AdHocPolymorphism.DerivingViaDemo
      EffectiveHaskell.Chapter6.AdHocPolymorphism.Eq
      EffectiveHaskell.Chapter6.AdHocPolymorphism.HKTDemo
      EffectiveHaskell.Chapter6.AdHocPolymorphism.Naive
      EffectiveHaskell.Chapter6.AdHocPolymorphism.NewtypeDemo
      EffectiveHaskell.Chapter6.AdHocPolymorphism.OrdExample
      EffectiveHaskell.Chapter6.AdHocPolymorphism.OrdExampleAlternateDefaults
      EffectiveHaskell.Chapter6.AdHocPolymorphism.OrdExampleDefaults
      EffectiveHaskell.Chapter6.AdHocPolymorphism.OrdExampleDefaultsWithMinMax
      EffectiveHaskell.Chapter6.AdHocPolymorphism.RecordNatural
      EffectiveHaskell.Chapter6.AdHocPolymorphism.Redacted
      EffectiveHaskell.Chapter6.AdHocPolymorphism.TypeApplications
      EffectiveHaskell.Chapter6.AdHocPolymorphism.USD
      EffectiveHaskell.Chapter6.AdHocPolymorphism.USD.Auto
      EffectiveHaskell.Chapter6.AdHocPolymorphism.USD.Manual
      EffectiveHaskell.Chapter7
      EffectiveHaskell.Chapter7.Examples
      EffectiveHaskell.Chapter7.SumArguments
      EffectiveHaskell.Chapter8
      EffectiveHaskell.Chapter8.Examples
      EffectiveHaskell.Chapter8.Examples.V1
      EffectiveHaskell.Chapter8.Examples.V2
      EffectiveHaskell.Chapter8.Examples.V3
      EffectiveHaskell.Chapter8.Examples.V4
      EffectiveHaskell.Chapter8.Examples.V5
      EffectiveHaskell.Chapter9
      EffectiveHaskell.Chapter9.Functors
      EffectiveHaskell.Chapter9.Functors.V1
      EffectiveHaskell.Fifo
      MyLib
  other-modules:
      Paths_effective_haskell_examples
  autogen-modules:
      Paths_effective_haskell_examples
  hs-source-dirs:
      src
  ghc-options: -Wall -O2
  build-depends:
      async
    , base
    , base64-bytestring
    , bytestring
    , containers
    , directory
    , filepath
    , mtl
    , process
    , text
    , time
    , transformers
    , unix
    , vector
  default-language: Haskell2010

executable effective-haskell-demos
  main-is: Main.hs
  other-modules:
      Paths_effective_haskell_examples
  autogen-modules:
      Paths_effective_haskell_examples
  hs-source-dirs:
      app
  ghc-options: -Wall -O2
  build-depends:
      async
    , base
    , base64-bytestring
    , bytestring
    , containers
    , directory
    , effective-haskell-examples
    , filepath
    , mtl
    , process
    , text
    , time
    , transformers
    , unix
    , vector
  default-language: Haskell2010

test-suite test
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  other-modules:
      EffectiveHaskell.Chapter1.BranchesSpec
      EffectiveHaskell.Chapter1.ComposingFunctionsSpec
      EffectiveHaskell.Chapter1.LetBindingsSpec
      EffectiveHaskell.Chapter1.LoopingSpec
      EffectiveHaskell.Chapter1.OperatorExampleSpec
      EffectiveHaskell.Chapter1.PrecedenceSpec
      EffectiveHaskell.Chapter10.MetricsV1Spec
      EffectiveHaskell.Chapter10.MetricsV2Spec
      EffectiveHaskell.Chapter10.V1Spec
      EffectiveHaskell.Chapter10.V2Spec
      EffectiveHaskell.Chapter10Spec
      EffectiveHaskell.Chapter11.ExistentialDemoSpec
      EffectiveHaskell.Chapter11.V1Spec
      EffectiveHaskell.Chapter11.V2Spec
      EffectiveHaskell.Chapter11.V3Spec
      EffectiveHaskell.Chapter11.V4Spec
      EffectiveHaskell.Chapter11Spec
      EffectiveHaskell.Chapter12.V1Spec
      EffectiveHaskell.Chapter12.V2Spec
      EffectiveHaskell.Chapter12.V3Spec
      EffectiveHaskell.Chapter12.V4Spec
      EffectiveHaskell.Chapter12.V5Spec
      EffectiveHaskell.Chapter12Spec
      EffectiveHaskell.Chapter13.ClassyArchiverSpec
      EffectiveHaskell.Chapter13.EitherIOSpec
      EffectiveHaskell.Chapter13.ExceptStateSpec
      EffectiveHaskell.Chapter13.ExceptTParserSpec
      EffectiveHaskell.Chapter13.ExceptTSpec
      EffectiveHaskell.Chapter13.MonadIODemoSpec
      EffectiveHaskell.Chapter13.MonadStateDemoSpec
      EffectiveHaskell.Chapter13.StateExceptSpec
      EffectiveHaskell.Chapter13.StateSpec
      EffectiveHaskell.Chapter13Spec
      EffectiveHaskell.Chapter14.SpellCheck.ListMemoSpec
      EffectiveHaskell.Chapter14.SpellCheck.NaiveSpec
      EffectiveHaskell.Chapter14.SpellCheck.STMemoSpec
      EffectiveHaskell.Chapter14.SpellCheck.TypesSpec
      EffectiveHaskell.Chapter14.SpellCheckSpec
      EffectiveHaskell.Chapter15.ClosedTypeFamilyDemoSpec
      EffectiveHaskell.Chapter15.CommandRunner.V2Spec
      EffectiveHaskell.Chapter15.CommandRunnerSpec
      EffectiveHaskell.Chapter15.GADTs.V1Spec
      EffectiveHaskell.Chapter15.OpenDataFamiliesDemo.V1Spec
      EffectiveHaskell.Chapter15.OpenDataFamiliesDemoSpec
      EffectiveHaskell.Chapter15.ShellCommand.V1Spec
      EffectiveHaskell.Chapter15.ShellCommand.V2Spec
      EffectiveHaskell.Chapter15.ShellCommand.V3Spec
      EffectiveHaskell.Chapter15.TypeFamilyListFuncs.V1Spec
      EffectiveHaskell.Chapter15.TypeFamilyListFuncs.V2Spec
      EffectiveHaskell.Chapter15.TypeFamilyListFuncsSpec
      EffectiveHaskell.Chapter15.TypeLevelListSpec
      EffectiveHaskell.Chapter15Spec
      EffectiveHaskell.Chapter2.FibsSpec
      EffectiveHaskell.Chapter3.PolymorphicFunctionsSpec
      EffectiveHaskell.Chapter4.CreatingDataTypesAndRecordsSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.ClassNaturalSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.DerivingSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.DerivingViaDemoSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.HKTDemoSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.NaiveSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.NewtypeDemoSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.OrdExampleAlternateDefaultsSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.OrdExampleDefaultsSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.OrdExampleDefaultsWithMinMaxSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.OrdExampleSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.RecordNaturalSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.RedactedSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.TypeApplicationsSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphism.USDSpec
      EffectiveHaskell.Chapter6.AdHocPolymorphismSpec
      EffectiveHaskell.Chapter7.ExamplesSpec
      EffectiveHaskell.Chapter7.SumArgumentsSpec
      EffectiveHaskell.Chapter8.Examples.V1Spec
      EffectiveHaskell.Chapter8.Examples.V2Spec
      EffectiveHaskell.Chapter8.Examples.V3Spec
      EffectiveHaskell.Chapter8.Examples.V4Spec
      EffectiveHaskell.Chapter8.Examples.V5Spec
      EffectiveHaskell.Chapter8.ExamplesSpec
      EffectiveHaskell.Chapter9.Functors.V1Spec
      EffectiveHaskell.Chapter9.FunctorsSpec
      EffectiveHaskell.Chapter9Spec
      EffectiveHaskell.TestUtils
      Paths_effective_haskell_examples
  autogen-modules:
      Paths_effective_haskell_examples
  hs-source-dirs:
      test
  ghc-options: -Wall
  build-depends:
      async
    , base
    , base64-bytestring
    , bytestring
    , containers
    , directory
    , effective-haskell-examples
    , filepath
    , hspec
    , mtl
    , process
    , text
    , time
    , transformers
    , unix
    , vector
  default-language: Haskell2010
```
</div>
</div>
</details>
</div>
