using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Operations;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace pure
{
    public class Analyzer
    {
        private readonly Dictionary<IMethodSymbol, MethodAnalysis> _analyzed;
        private readonly SemanticModel _model;
        private readonly CompilationUnitSyntax _root;

        public Analyzer(string program, string path = "")
        {
            var tree = CSharpSyntaxTree.ParseText(program, path: path);
            _root = tree.GetCompilationUnitRoot();
            var compilation = CSharpCompilation.Create("AnalyzedAssembly")
                .AddReferences(MetadataReference.CreateFromFile(
                    typeof(object).Assembly.Location))
                .AddSyntaxTrees(tree);
            _model = compilation.GetSemanticModel(tree);
            _analyzed = new Dictionary<IMethodSymbol, MethodAnalysis>();
        }

        public IEnumerable<IMethodSymbol> Methods()
        {
            var methodsDeclarations = _root.DescendantNodes().OfType<MethodDeclarationSyntax>();
            var propertyDeclarations = _root.DescendantNodes().OfType<BasePropertyDeclarationSyntax>();
            var constructorDeclarations = _root.DescendantNodes().OfType<ConstructorDeclarationSyntax>();
            var destructorDeclarations = _root.DescendantNodes().OfType<DestructorDeclarationSyntax>();
            var operatorDeclarations = _root.DescendantNodes().OfType<OperatorDeclarationSyntax>();
            var conversionOperatorDeclarations = _root.DescendantNodes().OfType<ConversionOperatorDeclarationSyntax>();
            var indexDeclarations = _root.DescendantNodes().OfType<IndexerDeclarationSyntax>();

            var methodSymbols = methodsDeclarations.Select(m => _model.GetDeclaredSymbol(m));
            var propertySymbols = propertyDeclarations
                .Select(p => _model.GetDeclaredSymbol(p) as IPropertySymbol)
                .Where(s => s != null)
                .SelectMany(s => new List<IMethodSymbol> {s.GetMethod, s.SetMethod})
                .Where(s => s != null);
            var constructorSymbols = constructorDeclarations.Select(c => _model.GetDeclaredSymbol(c));
            var destructorSymbols = destructorDeclarations.Select(c => _model.GetDeclaredSymbol(c));
            var operatorSymbols = operatorDeclarations.Select(o => _model.GetDeclaredSymbol(o));
            var conversionOperatorSymbols = conversionOperatorDeclarations.Select(o => _model.GetDeclaredSymbol(o));
            var indexSymbols = indexDeclarations
                .Select(i => _model.GetDeclaredSymbol(i))
                .Where(i => i != null)
                .SelectMany(i => new List<IMethodSymbol> {i.GetMethod, i.SetMethod})
                .Where(i => i != null);

            return methodSymbols
                .Concat(propertySymbols).Concat(constructorSymbols).Concat(destructorSymbols)
                .Concat(operatorSymbols).Concat(conversionOperatorSymbols).Concat(indexSymbols);
        }

        private static bool MethodHasPureAttribute(IMethodSymbol method)
        {
            return method.GetAttributes().Any(attribute =>
                attribute.AttributeClass.ToString() == "System.Diagnostics.Contracts.PureAttribute");
        }

        public MethodAnalysis AnalyzeMethod(IMethodSymbol method)
        {
            if (_analyzed.ContainsKey(method)) return _analyzed[method];


            var methodAnalysis = new MethodAnalysis
            {
                Name = method.ToString(),
                HasPureAttribute = MethodHasPureAttribute(method),
                HasVoidReturn = method.ReturnsVoid,
                HasOutParams =
                    method.Parameters.Any(param => param.RefKind == RefKind.Out || param.RefKind == RefKind.Ref)
            };

            if (method.DeclaringSyntaxReferences.Length == 0)
            {
                methodAnalysis.NoDeclaration = true;
                return methodAnalysis;
            }

            var declaration = method.DeclaringSyntaxReferences.Single().GetSyntax();

            if (declaration is ArrowExpressionClauseSyntax) declaration = declaration.Parent;
            BlockSyntax statementBody;
            ExpressionSyntax expressionBody;
            // TODO: local functions
            switch (declaration)
            {
                case MethodDeclarationSyntax mds:
                    statementBody = mds.Body;
                    expressionBody = mds.ExpressionBody?.Expression;
                    methodAnalysis.Kind = MethodKind.Method;
                    break;
                case AccessorDeclarationSyntax ads:
                    statementBody = ads.Body;
                    expressionBody = ads.ExpressionBody?.Expression;
                    if (ads.Parent.Parent is IndexerDeclarationSyntax)
                        methodAnalysis.Kind = ads.Kind() == SyntaxKind.GetAccessorDeclaration
                            ? MethodKind.IndexGetter
                            : MethodKind.IndexSetter;
                    else
                        methodAnalysis.Kind = ads.Kind() == SyntaxKind.GetAccessorDeclaration
                            ? MethodKind.PropertyGetter
                            : MethodKind.PropertySetter;

                    break;
                case ConstructorDeclarationSyntax cds:
                    statementBody = cds.Body;
                    expressionBody = cds.ExpressionBody?.Expression;
                    methodAnalysis.Kind = MethodKind.Constructor;
                    break;
                case DestructorDeclarationSyntax dds:
                    statementBody = dds.Body;
                    expressionBody = dds.ExpressionBody?.Expression;
                    methodAnalysis.Kind = MethodKind.Destructor;
                    break;
                case OperatorDeclarationSyntax ods:
                    statementBody = ods.Body;
                    expressionBody = ods.ExpressionBody?.Expression;
                    methodAnalysis.Kind = MethodKind.Operator;
                    break;
                case ConversionOperatorDeclarationSyntax cods:
                    statementBody = cods.Body;
                    expressionBody = cods.ExpressionBody?.Expression;
                    methodAnalysis.Kind = MethodKind.ConversionOperator;
                    break;
                case IndexerDeclarationSyntax ids:
                    statementBody = null;
                    expressionBody = ids.ExpressionBody?.Expression;
                    methodAnalysis.Kind = MethodKind.IndexGetter;
                    break;
                case LocalFunctionStatementSyntax localFunctionStatementSyntax:
                    // TODO: handle local function
                    statementBody = localFunctionStatementSyntax.Body;
                    expressionBody = localFunctionStatementSyntax.ExpressionBody?.Expression;
                    methodAnalysis.Kind = MethodKind.LocalFunction;
                    break;
                default:
                    throw new NotImplementedException($"unimplemented declaration type {declaration.GetType()}");
            }


            if (statementBody == null && expressionBody == null
            ) // no Body or ExpressionBody, this is the case for { get; set; }-style property declarations etc
                return methodAnalysis;

            var body = (SyntaxNode) statementBody ?? expressionBody;


            var reads = new List<SyntaxNode>();
            var writes = new List<SyntaxNode>();
            var news = new List<SyntaxNode>();
            var calls = new List<SyntaxNode>();
            var getCalls = new List<SyntaxNode>();
            var setCalls = new List<SyntaxNode>();

            var implicitConversions = new List<Tuple<SyntaxNode, IMethodSymbol>>();

            if (declaration is ConstructorDeclarationSyntax cds2)
                if (cds2.Initializer != null)
                    calls.Add(cds2.Initializer);

            void AnalyzeExpression(ExpressionSyntax expression, bool treatAsWrite = false, bool treatAsRead = true,
                bool ignoreAssignmentWrite = false)
            {
                if (expression == null) return;
                var op = _model.GetOperation(expression);
                while (op != null)
                {
                    if (op.Syntax != expression) break;
                    if (op is IConversionOperation conversionOperation)
                    {
                        if (conversionOperation.Conversion.IsUserDefined)
                        {
                            implicitConversions.Add(new Tuple<SyntaxNode, IMethodSymbol>(expression, conversionOperation.Conversion.MethodSymbol));
                        }
                    }
                    op = op.Parent;
                }
                switch (expression)
                {
                    case AssignmentExpressionSyntax assignmentExpressionSyntax:
                        if (assignmentExpressionSyntax.Kind() == SyntaxKind.SimpleAssignmentExpression)
                        {
                            if (!ignoreAssignmentWrite) AnalyzeExpression(assignmentExpressionSyntax.Left, true, false);
                        }
                        else
                        {
                            calls.Add(assignmentExpressionSyntax);
                            if (!ignoreAssignmentWrite) AnalyzeExpression(assignmentExpressionSyntax.Left, true);
                        }

                        AnalyzeExpression(assignmentExpressionSyntax.Right);
                        break;
                    case BinaryExpressionSyntax binaryExpressionSyntax:
                        calls.Add(binaryExpressionSyntax);
                        AnalyzeExpression(binaryExpressionSyntax.Left);
                        AnalyzeExpression(binaryExpressionSyntax.Right);
                        break;
                    case CastExpressionSyntax castExpressionSyntax:
                        calls.Add(castExpressionSyntax);
                        break;
                    case ElementAccessExpressionSyntax elementAccessExpressionSyntax:
                        if (treatAsWrite) writes.Add(elementAccessExpressionSyntax);
                        if (treatAsRead) reads.Add(elementAccessExpressionSyntax);
                        AnalyzeExpression(elementAccessExpressionSyntax.Expression);
                        foreach (var argument in elementAccessExpressionSyntax.ArgumentList.Arguments)
                            AnalyzeExpression(argument.Expression);
                        break;
                    case IdentifierNameSyntax identifierNameSyntax:
                        if (treatAsWrite) writes.Add(identifierNameSyntax);
                        if (treatAsRead) reads.Add(identifierNameSyntax);
                        break;
                    case ImplicitArrayCreationExpressionSyntax implicitArrayCreationExpressionSyntax:
                        foreach (var initializerExpression in implicitArrayCreationExpressionSyntax.Initializer
                            .Expressions)
                            AnalyzeExpression(initializerExpression);
                        break;
                    case InterpolatedStringExpressionSyntax interpolatedStringExpressionSyntax:
                        foreach (var interpolationPart in interpolatedStringExpressionSyntax.Contents)
                            if (interpolationPart is InterpolationSyntax interpolationSyntax)
                                AnalyzeExpression(interpolationSyntax.Expression);
                        break;
                    case InvocationExpressionSyntax invocationExpressionSyntax:
                        calls.Add(invocationExpressionSyntax);
                        AnalyzeExpression(invocationExpressionSyntax.Expression);
                        foreach (var argument in invocationExpressionSyntax.ArgumentList.Arguments)
                            AnalyzeExpression(argument.Expression);
                        break;
                    case LiteralExpressionSyntax _:
                        // ignore literals
                        break;
                    case MemberAccessExpressionSyntax memberAccessExpressionSyntax:
                        if (treatAsWrite) writes.Add(memberAccessExpressionSyntax);
                        if (treatAsRead) reads.Add(memberAccessExpressionSyntax);
                        AnalyzeExpression(memberAccessExpressionSyntax.Expression);
                        break;
                    case ObjectCreationExpressionSyntax objectCreationExpressionSyntax:
                        news.Add(objectCreationExpressionSyntax);
                        calls.Add(objectCreationExpressionSyntax);
                        if (objectCreationExpressionSyntax.ArgumentList != null)
                            foreach (var argument in objectCreationExpressionSyntax.ArgumentList.Arguments)
                                AnalyzeExpression(argument.Expression);

                        if (objectCreationExpressionSyntax.Initializer != null)
                            foreach (var initializerExpression in objectCreationExpressionSyntax.Initializer.Expressions
                            )
                                AnalyzeExpression(initializerExpression, ignoreAssignmentWrite: true);

                        break;
                    case ParenthesizedExpressionSyntax parenthesizedExpressionSyntax:
                        AnalyzeExpression(parenthesizedExpressionSyntax.Expression);
                        break;
                    case ParenthesizedLambdaExpressionSyntax parenthesizedLambdaExpressionSyntax:
                        // TODO: do something I guess
                        break;
                    case PostfixUnaryExpressionSyntax postfixUnaryExpressionSyntax:
                        calls.Add(postfixUnaryExpressionSyntax);
                        AnalyzeExpression(postfixUnaryExpressionSyntax.Operand, true);
                        break;
                    case PrefixUnaryExpressionSyntax prefixUnaryExpressionSyntax:
                        calls.Add(prefixUnaryExpressionSyntax);
                        AnalyzeExpression(prefixUnaryExpressionSyntax.Operand, true);
                        break;
                    case SimpleLambdaExpressionSyntax simpleLambdaExpressionSyntax:
                        // TODO: do something I guess
                        break;
                    default:
                        throw new NotImplementedException($"unimplemented expression type {expression.GetType()}");
                }
            }

            void AnalyzeStatement(StatementSyntax statement)
            {
                switch (statement)
                {
                    case BlockSyntax block:
                        foreach (var child in block.ChildNodes()) AnalyzeStatement((StatementSyntax) child);
                        break;
                    case BreakStatementSyntax _:
                        // do nothing
                        break;
                    case CommonForEachStatementSyntax commonForEachStatement:
                        AnalyzeExpression(commonForEachStatement.Expression);
                        AnalyzeStatement(commonForEachStatement.Statement);
                        break;
                    case DoStatementSyntax doStatement:
                        AnalyzeExpression(doStatement.Condition);
                        AnalyzeStatement(doStatement.Statement);
                        break;
                    case ExpressionStatementSyntax expressionStatement:
                        AnalyzeExpression(expressionStatement.Expression);
                        break;
                    case ForStatementSyntax forStatement:
                        AnalyzeExpression(forStatement.Condition);
                        if (forStatement.Declaration != null)
                            foreach (var forDeclaration in forStatement.Declaration.Variables)
                                AnalyzeExpression(forDeclaration.Initializer
                                    .Value);
                        foreach (var initializer in forStatement.Initializers) AnalyzeExpression(initializer);
                        foreach (var incrementor in forStatement.Incrementors) AnalyzeExpression(incrementor);
                        AnalyzeStatement(forStatement.Statement);
                        break;
                    case IfStatementSyntax ifStatement:
                        AnalyzeExpression(ifStatement.Condition);
                        AnalyzeStatement(ifStatement.Statement);
                        break;
                    case LocalDeclarationStatementSyntax localDeclarationStatement:
                        foreach (var variable in localDeclarationStatement.Declaration.Variables)
                            if (variable.Initializer != null)
                                AnalyzeExpression(variable.Initializer.Value);
                        break;
                    case LocalFunctionStatementSyntax localFunctionStatement:
                        // TODO: analyze local function
                        break;
                    case LockStatementSyntax lockStatement:
                        methodAnalysis.Locks.Add(Location.FromSyntaxNode(lockStatement));
                        AnalyzeExpression(lockStatement.Expression, true);
                        AnalyzeStatement(lockStatement.Statement);
                        break;
                    case ReturnStatementSyntax returnStatement:
                        AnalyzeExpression(returnStatement.Expression);
                        break;
                    case SwitchStatementSyntax switchStatement:
                        AnalyzeExpression(switchStatement.Expression);
                        foreach (var section in switchStatement.Sections)
                        foreach (var sectionStatement in section.Statements)
                            AnalyzeStatement(sectionStatement);

                        break;
                    case ThrowStatementSyntax throwStatement:
                        methodAnalysis.Throws.Add(Location.FromSyntaxNode(throwStatement));
                        AnalyzeExpression(throwStatement.Expression);
                        break;
                    case TryStatementSyntax tryStatement:
                        methodAnalysis.Tries.Add(Location.FromSyntaxNode(tryStatement));
                        AnalyzeStatement(tryStatement.Block);
                        foreach (var @catch in tryStatement.Catches) AnalyzeStatement(@catch.Block);
                        if (tryStatement.Finally != null) AnalyzeStatement(tryStatement.Finally.Block);
                        break;
                    case UnsafeStatementSyntax unsafeStatementSyntax:
                        methodAnalysis.Unsafes.Add(Location.FromSyntaxNode(unsafeStatementSyntax));
                        AnalyzeStatement(unsafeStatementSyntax.Block);
                        break;
                    case UsingStatementSyntax usingStatement:
                        if (usingStatement.Expression != null) AnalyzeExpression(usingStatement.Expression);
                        if (usingStatement.Declaration != null)
                            foreach (var variable in usingStatement.Declaration.Variables)
                                AnalyzeExpression(variable.Initializer.Value);
                        AnalyzeStatement(usingStatement.Statement);
                        // TODO: using calls `Dispose()`, need to add Dispose to call list
                        // or just ignore it because most things that use Dispose are impure anyways and using them makes a method impure
                        break;
                    case WhileStatementSyntax whileStatement:
                        AnalyzeExpression(whileStatement.Condition);
                        AnalyzeStatement(whileStatement.Statement);
                        break;
                    case YieldStatementSyntax yieldStatement:
                        AnalyzeExpression(yieldStatement.Expression);
                        break;
                    default:
                        throw new NotImplementedException($"unimplemented statement type {statement.GetType()}");
                }
            }

            switch (body)
            {
                case ExpressionSyntax bodyExpression:
                    AnalyzeExpression(bodyExpression);
                    break;
                case StatementSyntax bodyStatement:
                    AnalyzeStatement(bodyStatement);
                    break;
            }

            foreach (var read in reads)
            {
                var symbol = _model.GetSymbolInfo(read).Symbol;
                // only properties and fields are interesting
                if (!(symbol is IPropertySymbol) && !(symbol is IFieldSymbol)) continue;
                // const fields are fine to read from
                if (symbol is IFieldSymbol fs && fs.IsConst) continue;
                // if it's a property, add it to the getCalls list to check for purity
                if (symbol is IPropertySymbol) getCalls.Add(read);
                if (symbol.IsStatic)
                {
                    methodAnalysis.NonLocalReads.Add(NamedLocation.FromSyntaxNode(read));
                }
                else
                {
                    // TODO: if the read is to a member of a local variable it should be considered local
                    // TODO: but only if the local variable is not a reference to something non-local
                    // this probably requires rather complex data flow analysis
                    // e.g.
                    // int Method(SomeClass x) {
                    //    var localVariable = x;
                    //    return localVariable.foobar;
                    // }
                    // If SomeClass is a reference type, then reading localVariable.foobar is a read access to x.foobar, which would be considered a reference read

                    // there's something about unconstrained type parameters in the documentation to `IsReferenceType`
                    // I am not sure if that is relevant here
                    if (symbol.ContainingType.IsReferenceType)
                        methodAnalysis.ReferenceReads.Add(NamedLocation.FromSyntaxNode(read));
                }
            }

            foreach (var write in writes)
            {
                var symbol = _model.GetSymbolInfo(write).Symbol;

                if (symbol is IParameterSymbol ps)
                    if (ps.RefKind == RefKind.Out || ps.RefKind == RefKind.Ref)
                        methodAnalysis.NonLocalWrites.Add(NamedLocation.FromSyntaxNode(write));
                if (!(symbol is IPropertySymbol) && !(symbol is IFieldSymbol)) continue;

                if (symbol is IPropertySymbol) setCalls.Add(write);
                if (symbol.IsStatic)
                {
                    methodAnalysis.NonLocalWrites.Add(NamedLocation.FromSyntaxNode(write));
                }
                else
                {
                    // TODO: if the write is to a member of a local variable it should be considered local
                    // TODO: but only if the local variable is not a reference to something non-local
                    // this probably requires rather complex data flow analysis
                    // e.g.
                    // void Method(SomeClass x) {
                    //    var localVariable = x;
                    //    localVariable.foobar = 1;
                    // }
                    // If SomeClass is a reference type, then writing localVariable.foobar is a write access to x.foobar, which would be considered non-local

                    // 
                    if (symbol.ContainingType.IsReferenceType || symbol.ContainingType == method.ContainingType)
                        methodAnalysis.NonLocalWrites.Add(NamedLocation.FromSyntaxNode(write));
                }
            }


            foreach (var @new in news)
            {
                var constructor = (IMethodSymbol) _model.GetSymbolInfo(@new).Symbol;
                if (constructor == null)
                {
                    // TODO: what now?
                    // this happens for `Func<>` types, but possible for others too
                    // I would treat these as value types for now, but I am not sure if that is correct
                    methodAnalysis.ValueNews.Add(NamedLocation.FromSyntaxNode(@new));
                }
                else
                {
                    if (constructor.ContainingType.IsValueType)
                        methodAnalysis.ValueNews.Add(NamedLocation.FromSyntaxNode(@new));
                    else
                        methodAnalysis.ReferenceNews.Add(NamedLocation.FromSyntaxNode(@new));
                }
            }


            methodAnalysis.ImpureCalls.AddRange(calls
                .Select(invocation =>  new
                {
                    invocation, 
                    methodSymbol = (IMethodSymbol) _model.GetSymbolInfo(invocation).Symbol
                })
                .Concat(
                    getCalls
                        .Select(invocation => new
                        {
                            invocation,
                            methodSymbol = ((IPropertySymbol) _model.GetSymbolInfo(invocation).Symbol).GetMethod
                        }))
                .Concat(setCalls
                    .Select(invocation => new
                    {
                        invocation, methodSymbol = ((IPropertySymbol) _model.GetSymbolInfo(invocation).Symbol).SetMethod
                    }))
                .Concat(implicitConversions
                    .Select(t => new
                    {
                        invocation = t.Item1,
                        methodSymbol = t.Item2
                    }))
                .Where(t => t.methodSymbol != null)
                .Where(t => !(_analyzed.ContainsKey(t.methodSymbol) ? _analyzed[t.methodSymbol] : AnalyzeMethod(t.methodSymbol)).IsPure)
                .Select(t => NamedLocation.FromSyntaxNode(t.invocation)));

            _analyzed[method] = methodAnalysis;
            return methodAnalysis;
        }

    }
}