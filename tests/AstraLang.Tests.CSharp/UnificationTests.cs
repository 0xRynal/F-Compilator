using Xunit;
using AstraLang.Core;
using AstraLang.TypeInference;
using Microsoft.FSharp.Core;

namespace AstraLang.Tests.CSharp;

public class UnificationTests
{
    [Fact] public void TInt_TInt() => Assert.True(ResultModule.IsOk(Unification.unify(Type.TInt, Type.TInt, SourceSpan.Zero)));
    [Fact] public void TBool_TBool() => Assert.True(ResultModule.IsOk(Unification.unify(Type.TBool, Type.TBool, SourceSpan.Zero)));
    [Fact] public void TUnit_TUnit() => Assert.True(ResultModule.IsOk(Unification.unify(Type.TUnit, Type.TUnit, SourceSpan.Zero)));
    [Fact] public void TInt_TBool_fails() => Assert.True(ResultModule.IsError(Unification.unify(Type.TInt, Type.TBool, SourceSpan.Zero)));
    [Fact] public void TUnit_TInt_fails() => Assert.True(ResultModule.IsError(Unification.unify(Type.TUnit, Type.TInt, SourceSpan.Zero)));
}
