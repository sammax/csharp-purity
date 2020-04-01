using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace pure
{
    public class Analyzer
    {
        private readonly Dictionary<IMethodSymbol, MethodAnalysis> analyzed;
        private readonly CSharpCompilation compilation;
        private readonly SemanticModel model;
        private readonly CompilationUnitSyntax root;
        private readonly SyntaxTree tree;

        public Analyzer(string program, string path = "")
        {
            tree = CSharpSyntaxTree.ParseText(program, path: path);
            root = tree.GetCompilationUnitRoot();
            compilation = CSharpCompilation.Create("AnalyzedAssembly")
                .AddReferences(MetadataReference.CreateFromFile(
                    typeof(object).Assembly.Location))
                .AddSyntaxTrees(tree);
            model = compilation.GetSemanticModel(tree);
            analyzed = new Dictionary<IMethodSymbol, MethodAnalysis>();
        }

        public IEnumerable<IMethodSymbol> Methods()
        {
            var methodsDeclarations = root.DescendantNodes().OfType<MethodDeclarationSyntax>();
            var propertyDeclarations = root.DescendantNodes().OfType<BasePropertyDeclarationSyntax>();
            var constructorDeclarations = root.DescendantNodes().OfType<ConstructorDeclarationSyntax>();
            var destructorDeclarations = root.DescendantNodes().OfType<DestructorDeclarationSyntax>();
            var operatorDeclarations = root.DescendantNodes().OfType<OperatorDeclarationSyntax>();
            var conversionOperatorDeclarations = root.DescendantNodes().OfType<ConversionOperatorDeclarationSyntax>();
            var indexDeclarations = root.DescendantNodes().OfType<IndexerDeclarationSyntax>();
            
            var methodSymbols = methodsDeclarations.Select(m => model.GetDeclaredSymbol(m));
            var propertySymbols = propertyDeclarations
                .Select(p => model.GetDeclaredSymbol(p) as IPropertySymbol)
                .Where(s => s != null)
                .SelectMany(s => new List<IMethodSymbol>{s.GetMethod, s.SetMethod})
                .Where(s => s != null);
            var constructorSymbols = constructorDeclarations.Select(c => model.GetDeclaredSymbol(c));
            var destructorSymbols = destructorDeclarations.Select(c => model.GetDeclaredSymbol(c));
            var operatorSymbols = operatorDeclarations.Select(o => model.GetDeclaredSymbol(o));
            var conversionOperatorSymbols = conversionOperatorDeclarations.Select(o => model.GetDeclaredSymbol(o));
            var indexSymbols = indexDeclarations
                .Select(i => model.GetDeclaredSymbol(i) as IPropertySymbol)
                .Where(i => i != null)
                .SelectMany(i => new List<IMethodSymbol> {i.GetMethod, i.SetMethod})
                .Where(i => i != null);

            return methodSymbols
                .Concat(propertySymbols).Concat(constructorSymbols).Concat(destructorSymbols)
                .Concat(operatorSymbols).Concat(conversionOperatorSymbols).Concat(indexSymbols);
        }

        public MethodAnalysis AnalyzeMethod(IMethodSymbol method)
        {
            if (analyzed.ContainsKey(method)) return analyzed[method];


            var methodAnalysis = new MethodAnalysis();
            methodAnalysis.NonLocalReads = new List<VariableAccess>();
            methodAnalysis.NonLocalWrites = new List<VariableAccess>();
            methodAnalysis.Name = method.ToString();
            methodAnalysis.HasPureAttribute = method.GetAttributes().Any(attribute =>
                attribute.AttributeClass.ToString() == "System.Diagnostics.Contracts.PureAttribute");

            if (method.DeclaringSyntaxReferences.Length == 0)
            {
                methodAnalysis.NoDeclaration = true;
                return methodAnalysis;
            }
            
            var declaration = method.DeclaringSyntaxReferences.Single().GetSyntax();
            
            if (declaration is ArrowExpressionClauseSyntax)
            {
                declaration = declaration.Parent;
            }
            BlockSyntax body;
            ArrowExpressionClauseSyntax expressionBody;
            // TODO: local functions
            switch (declaration)
            {
                case MethodDeclarationSyntax mds:
                    body = mds.Body;
                    expressionBody = mds.ExpressionBody;
                    methodAnalysis.Kind = MethodKind.Method;
                    break;
                case AccessorDeclarationSyntax ads:
                    body = ads.Body;
                    expressionBody = ads.ExpressionBody;
                    if (ads.Parent.Parent is IndexerDeclarationSyntax)
                    {
                        methodAnalysis.Kind = ads.Kind() == SyntaxKind.GetAccessorDeclaration
                            ? MethodKind.IndexGetter
                            : MethodKind.IndexSetter;
                    }
                    else
                    {
                        methodAnalysis.Kind = ads.Kind() == SyntaxKind.GetAccessorDeclaration
                            ? MethodKind.PropertyGetter
                            : MethodKind.PropertySetter;
                    }

                    break;
                case ConstructorDeclarationSyntax cds:
                    body = cds.Body;
                    expressionBody = cds.ExpressionBody;
                    methodAnalysis.Kind = MethodKind.Constructor;
                    break;
                case DestructorDeclarationSyntax dds:
                    body = dds.Body;
                    expressionBody = dds.ExpressionBody;
                    methodAnalysis.Kind = MethodKind.Destructor;
                    break;
                case OperatorDeclarationSyntax ods:
                    body = ods.Body;
                    expressionBody = ods.ExpressionBody;
                    methodAnalysis.Kind = MethodKind.Operator;
                    break;
                case ConversionOperatorDeclarationSyntax cods:
                    body = cods.Body;
                    expressionBody = cods.ExpressionBody;
                    methodAnalysis.Kind = MethodKind.ConversionOperator;
                    break;
                case IndexerDeclarationSyntax ids:
                    body = null;
                    expressionBody = ids.ExpressionBody;
                    methodAnalysis.Kind = MethodKind.IndexGetter;
                    break;
                default:
                    throw new NotImplementedException($"unimplemented declaration type {declaration.GetType()}");
            }
            
            

            

            if (body == null && expressionBody == null) // no body or expressionbody, this is the case for { get; set; }-style property declarations etc
            {
                return methodAnalysis;
            }

            SyntaxNode actualBody = (SyntaxNode) body ?? expressionBody;

            // analyze calls
            methodAnalysis.HasImpureCalls = !actualBody.DescendantNodes().OfType<InvocationExpressionSyntax>()
                .All(
                    invocation =>
                    {
                        var invokedMethod = model.GetSymbolInfo(invocation).Symbol as IMethodSymbol;
                        if (invokedMethod.GetAttributes().Any(attribute =>
                            attribute.AttributeClass.ToString() == "System.Diagnostics.Contracts.PureAttribute"))
                            return true;

                        if (analyzed.ContainsKey(invokedMethod)) return analyzed[invokedMethod].IsPure;

                        var analysis = AnalyzeMethod(invokedMethod);
                        return analysis.IsPure;
                    });
            

            // analyze return
            methodAnalysis.HasVoidReturn = method.ReturnsVoid;

            // out/inout params
            methodAnalysis.HasOutParams = method.Parameters.Any(param => param.RefKind == RefKind.Out);

            // new
            methodAnalysis.HasNew = actualBody.DescendantNodes().OfType<ObjectCreationExpressionSyntax>().Any();

            // unsafe
            methodAnalysis.HasUnsafe = actualBody.DescendantNodes().OfType<UnsafeStatementSyntax>().Any()
                                       || declaration.GetLeadingTrivia()
                                           .Any(trivia => trivia.Token.Text == "unsafe");

            // try
            methodAnalysis.HasTries = actualBody.DescendantNodes().OfType<TryStatementSyntax>().Any();

            // locks
            methodAnalysis.HasLocks = actualBody.DescendantNodes().OfType<LockStatementSyntax>().Any();

            // throws
            methodAnalysis.HasThrows = actualBody.DescendantNodes().OfType<ThrowStatementSyntax>().Any();
            // reads and writes

            var reads = new List<SyntaxNode>();
            var writes = new List<SyntaxNode>();

            writes.AddRange(actualBody.DescendantNodes().OfType<AssignmentExpressionSyntax>()
                .Select(aes => aes.Left));


            IEnumerable<SyntaxNode> extractReads(SyntaxNode node)
            {
                switch (node)
                {
                    case LiteralExpressionSyntax literalExpressionSyntax:
                        return new List<SyntaxNode>();
                    case IdentifierNameSyntax identifierNameSyntax:
                        return new List<SyntaxNode> {identifierNameSyntax};
                    case MemberAccessExpressionSyntax memberAccessExpressionSyntax:
                        return new List<SyntaxNode> {memberAccessExpressionSyntax};
                    case ObjectCreationExpressionSyntax objectCreationExpressionSyntax:
                        if (objectCreationExpressionSyntax.ArgumentList != null)
                            return
                                objectCreationExpressionSyntax.ArgumentList.Arguments.SelectMany(a =>
                                    extractReads(a.Expression));

                        if (objectCreationExpressionSyntax.Initializer != null)
                            return
                                objectCreationExpressionSyntax.Initializer.Expressions.SelectMany(i =>
                                    extractReads(((AssignmentExpressionSyntax) i).Right));
                        break;
                    case InvocationExpressionSyntax invocationExpressionSyntax:
                        return
                            invocationExpressionSyntax.ArgumentList.Arguments.SelectMany(a =>
                                extractReads(a.Expression)).Concat(extractReads(invocationExpressionSyntax.Expression));
                        break;
                    case ImplicitArrayCreationExpressionSyntax implicitArrayCreationExpressionSyntax:
                        return implicitArrayCreationExpressionSyntax.Initializer.Expressions.SelectMany(e =>
                            extractReads(e));
                    case BinaryExpressionSyntax binaryExpressionSyntax:
                        return extractReads(binaryExpressionSyntax.Left)
                            .Concat(extractReads(binaryExpressionSyntax.Right));
                    case AssignmentExpressionSyntax assignmentExpressionSyntax:
                        return extractReads(assignmentExpressionSyntax.Right);
                    case PrefixUnaryExpressionSyntax prefixUnaryExpressionSyntax:
                        return extractReads(prefixUnaryExpressionSyntax.Operand);
                    case ParenthesizedExpressionSyntax parenthesizedExpressionSyntax:
                        return extractReads(parenthesizedExpressionSyntax.Expression);
                    case ElementAccessExpressionSyntax elementAccessExpressionSyntax:
                        return
                            elementAccessExpressionSyntax.ArgumentList.Arguments.SelectMany(a =>
                                extractReads(a.Expression)).Concat(extractReads(elementAccessExpressionSyntax.Expression));
                }

                return new List<SyntaxNode>();
            }


            // can't use `Body.Statements` because it doesn't contain statements in nested blocks
            // but the current approach also includes statements in local functions, which is problematic
            // local functions should be checked for purity if they are called, but the scope of the containing method should be considered local for them
            // local functions should not be checked if they are not called (e.g. they are returned)
            // rationale: a function that returns an impure function can itself be pure
            // the same problem exists for the writes where currently all `Left`s of all `AssignmentExpressionSyntax`es are added, even if they are in a local function and for all the other checks (impure calls, new, unsafe, throws, tries, locks) 
            // possible solution: extract this loop into a function, call it with only the top-level statements, then recursively descend into nested block statements with special treatment for local functions
            // for writes there is also the problem that it currently catches writes in `new TypeName{Prop = Value}` initializers
            if (body != null)
            {
                foreach (var statement in body.DescendantNodes().OfType<StatementSyntax>())
                    switch (statement)
                    {
                        case CommonForEachStatementSyntax commonForEachStatement:
                            reads.AddRange(extractReads(commonForEachStatement.Expression));
                            break;
                        case DoStatementSyntax doStatement:
                            reads.AddRange(extractReads(doStatement.Condition));
                            break;
                        case ExpressionStatementSyntax expressionStatement:
                            if (expressionStatement.Expression is AssignmentExpressionSyntax aes)
                                reads.AddRange(extractReads(aes.Right));
                            else if (expressionStatement.Expression is InvocationExpressionSyntax
                                invocationExpressionSyntax
                            )
                                reads.AddRange(extractReads(invocationExpressionSyntax));
                            else
                                throw new NotImplementedException(
                                    $"unimplemented expression type {expressionStatement.Expression.GetType()}");
                            break;
                        case ForStatementSyntax forStatement:
                            reads.AddRange(extractReads(forStatement.Condition));
                            if (forStatement.Declaration != null)
                                reads.AddRange(
                                    forStatement.Declaration.Variables.SelectMany(
                                        v => extractReads(v.Initializer.Value)));
                            foreach (var ae in forStatement.Initializers.Select(i => (AssignmentExpressionSyntax) i))
                                reads.AddRange(extractReads(ae.Right));

                            foreach (var incrementor in forStatement.Incrementors)
                                switch (incrementor)
                                {
                                    case PostfixUnaryExpressionSyntax postfixUnaryExpressionSyntax:
                                        writes.Add(postfixUnaryExpressionSyntax.Operand);
                                        break;
                                    case PrefixUnaryExpressionSyntax prefixUnaryExpressionSyntax:
                                        writes.Add(prefixUnaryExpressionSyntax.Operand);
                                        break;
                                    case AssignmentExpressionSyntax assignmentExpressionSyntax:
                                        // writes are already added when all `Left`s of all assignment expressions are added
                                        reads.AddRange(extractReads(assignmentExpressionSyntax.Right));
                                        break;
                                    default:
                                        throw new NotImplementedException(
                                            $"unimplemented incrementor type {incrementor.GetType()}");
                                }

                            break;
                        case IfStatementSyntax ifStatement:
                            reads.AddRange(extractReads(ifStatement.Condition));
                            break;
                        case LocalDeclarationStatementSyntax localDeclarationStatement:
                            reads.AddRange(localDeclarationStatement.Declaration.Variables
                                .Where(v => v.Initializer != null)
                                .SelectMany(v => extractReads(v.Initializer.Value)));
                            break;
                        case LockStatementSyntax lockStatement:
                            reads.AddRange(extractReads(lockStatement.Expression));
                            writes.Add(lockStatement.Expression);
                            break;
                        case ReturnStatementSyntax returnStatement:
                            reads.AddRange(extractReads(returnStatement.Expression));
                            break;
                        case SwitchStatementSyntax switchStatement:
                            reads.AddRange(extractReads(switchStatement.Expression));
                            break;
                        case ThrowStatementSyntax throwStatement:
                            reads.AddRange(extractReads(throwStatement.Expression));
                            break;
                        case UsingStatementSyntax usingStatement:
                            if (usingStatement.Expression != null)
                            {
                                var assignment = (AssignmentExpressionSyntax) usingStatement.Expression;
                                reads.AddRange(extractReads(assignment.Right));
                            }

                            if (usingStatement.Declaration != null)
                                reads.AddRange(
                                    usingStatement.Declaration.Variables.SelectMany(v => extractReads(v.Initializer)));
                            break;
                        case WhileStatementSyntax whileStatement:
                            reads.AddRange(extractReads(whileStatement.Condition));
                            break;
                        case YieldStatementSyntax yieldStatement:
                            reads.AddRange(extractReads(yieldStatement.Expression));
                            break;
                    }
            }
            else
            {
                reads.AddRange(extractReads(expressionBody.Expression));
            }


            IEnumerable<SyntaxNode> getIdentifierHierarchy(SyntaxNode read)
            {
                switch (read)
                {
                    case IdentifierNameSyntax ins:
                        return new List<SyntaxNode> {ins};
                    case MemberAccessExpressionSyntax maes:
                        var hierarchy = new List<SyntaxNode>();
                        switch (maes.Expression)
                        {
                            case IdentifierNameSyntax ins:
                                hierarchy.Add(ins);
                                break;
                            case InvocationExpressionSyntax ies:
                                hierarchy.Add(ies);
                                break;
                            case MemberAccessExpressionSyntax maes2:
                                hierarchy.AddRange(getIdentifierHierarchy(maes2));
                                break;
                            case ThisExpressionSyntax tes:
                                hierarchy.Add(tes);
                                break;
                            case ElementAccessExpressionSyntax eaes:
                                hierarchy.Add(eaes);
                                break;
                            default:
                                throw new NotImplementedException(
                                    $"unimplemented member access expression syntax {maes.Expression.GetType()}");
                        }

                        hierarchy.Add(maes.Name);
                        return hierarchy;
                    case ElementAccessExpressionSyntax eaes:
                        return new List<SyntaxNode>{eaes};
                    default:
                        throw new NotImplementedException($"unimplemented read expression syntax {read.GetType()}");
                }
            }


            foreach (var read in reads)
            {
                var identifierHierarchy = getIdentifierHierarchy(read);
                var mainIdentifier = identifierHierarchy.First();
                if (mainIdentifier is InvocationExpressionSyntax)
                    // the invocation is already checked for purity, and the result of the invocation is by definition local
                    // TODO: verify that this assumption is actually true
                    continue;
                if (mainIdentifier is ThisExpressionSyntax)
                    // read access to `this` is considered local
                    continue;

                var identifierNameSyntax = (IdentifierNameSyntax) mainIdentifier;
                var symbol = model.GetSymbolInfo(identifierNameSyntax).Symbol;
                if (symbol == null) throw new Exception("symbol not found");

                switch (symbol)
                {
                    case IParameterSymbol _:
                    case ILocalSymbol _:
                        continue;
                    case IFieldSymbol fieldSymbol:
                        if (!fieldSymbol.IsStatic)
                            continue;
                        else if (fieldSymbol.IsConst) continue;
                        break;
                    case INamespaceOrTypeSymbol _:
                        var isConst = true;
                        foreach (var node in identifierHierarchy)
                        {
                            var nodeSymbol = model.GetSymbolInfo(node).Symbol;
                            if (nodeSymbol is INamespaceOrTypeSymbol || nodeSymbol is IMethodSymbol) continue;
                            if (nodeSymbol is IPropertySymbol propertySymbol)
                            {
                                // getters are considered not-const for simplicity
                                // theoretically, we could check if the getter only accesses consts, and then consider it const
                                isConst = false;
                                break;
                            }

                            if (nodeSymbol is IFieldSymbol fieldSymbol)
                            {
                                if (!fieldSymbol.IsConst)
                                {
                                    isConst = false;
                                    break;
                                }
                            }
                            else
                            {
                                throw new NotImplementedException($"unimplemented symbol {nodeSymbol.GetType()}");
                            }
                        }

                        if (isConst) continue;
                        break;
                    case IMethodSymbol _:
                        continue;
                    default:
                        throw new NotImplementedException($"unimplemented symbol type {symbol.GetType()}");
                }

                // not local
                methodAnalysis.HasNonLocalReads = true;
                var span = read.SyntaxTree.GetLineSpan(read.Span);

                methodAnalysis.NonLocalReads.Add(new VariableAccess
                {
                    Name = read.ToString(),
                    Location = new Location
                    {
                        Source = read.SyntaxTree.FilePath,
                        SpanStart = read.SpanStart,
                        Line = span.StartLinePosition.Line + 1,
                        Character = span.StartLinePosition.Character + 1
                    }
                });
            }

            foreach (var write in writes)
            {
                var hierarchy = getIdentifierHierarchy(write);
                var mainIdentifier = hierarchy.First();
                if (mainIdentifier is ThisExpressionSyntax)
                {
                    // write to this => nonlocal
                }
                else
                {
                    var symbol = model.GetSymbolInfo(mainIdentifier).Symbol;
                    if (symbol == null) throw new Exception("symbol not found");

                    switch (symbol)
                    {
                        case IParameterSymbol parameterSymbol:
                            if (parameterSymbol.RefKind == RefKind.Ref || parameterSymbol.RefKind == RefKind.Out)
                                // assigning to refs and outs is non-local
                                break;
                            else if (!parameterSymbol.Type.IsValueType)
                                // assigning to members of a reference type is non-local
                                if (hierarchy.Count() > 1
                                ) // the assignment is to a member and not directly to the variable 
                                    break;
                            continue;
                        case ILocalSymbol _: // writes to local symbols and their members is generally allowed
                            // TODO: what if a local symbol is assigned a reference type parameter and then a member of that local symbol is written to?
                            //    can that even be detected?
                            //    dataflow analysis?
                            continue;
                        case IFieldSymbol _:
                            // write to fields is non-local
                            break;
                        case IPropertySymbol _:
                            // write to properties is non-local
                            break;
                        case INamespaceOrTypeSymbol _:
                            // definitely non-local
                            break;
                        default:
                            throw new NotImplementedException($"unimplemented symbol type {symbol.GetType()}");
                    }
                }

                // not local
                methodAnalysis.HasNonLocalWrites = true;
                var span = write.SyntaxTree.GetLineSpan(write.Span);

                methodAnalysis.NonLocalWrites.Add(new VariableAccess
                {
                    Name = write.ToString(),
                    Location = new Location
                    {
                        Source = write.SyntaxTree.FilePath,
                        SpanStart = write.SpanStart,
                        Line = span.StartLinePosition.Line + 1,
                        Character = span.StartLinePosition.Character + 1
                    }
                });
            }

            analyzed[method] = methodAnalysis;
            return methodAnalysis;
        }
    }
}