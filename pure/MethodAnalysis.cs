using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace pure
{
    public class MethodAnalysis
    {
        public MethodKind Kind;
        public bool NoDeclaration;
        public string Name;
        public bool HasVoidReturn;
        public bool HasOutParams;
        public bool IsStatic;
        public bool HasImpureCalls => ImpureCalls.Any();
        public bool HasNonLocalReads => NonLocalReads.Any();
        public bool HasReferenceReads => ReferenceReads.Any();
        public bool HasNonLocalWrites => NonLocalWrites.Any();
        public bool HasReferenceNews => ReferenceNews.Any();
        public bool HasNew => HasReferenceNews || ValueNews.Any();
        public bool UnsafeMethod;
        public bool HasUnsafes => UnsafeMethod || Unsafes.Any();
        public bool HasTries => Tries.Any();
        public bool HasLocks => Locks.Any();
        public bool HasThrows => Throws.Any();
        

        public bool HasPureAttribute;

        // TODO: verify that those checks are necessary and sufficient for purity
        public bool IsPure
        {
            get
            {
                if (NoDeclaration)
                {
                    return true;
                }
                return !HasVoidReturn &&
                       !HasOutParams &&
                       !HasImpureCalls &&
                       !HasNonLocalReads &&
                       !HasNonLocalWrites &&
                       !HasReferenceNews &&
                       !HasUnsafes &&
                       !HasThrows &&
                       !HasLocks;
            }
        }


        public readonly List<NamedLocation> NonLocalReads = new List<NamedLocation>();
        public readonly List<NamedLocation> ReferenceReads = new List<NamedLocation>();
        public readonly List<NamedLocation> NonLocalWrites = new List<NamedLocation>();
        public readonly List<NamedLocation> ImpureCalls = new List<NamedLocation>();
        public readonly List<NamedLocation> ReferenceNews = new List<NamedLocation>();
        public readonly List<NamedLocation> ValueNews = new List<NamedLocation>();
        public readonly List<Location> Throws = new List<Location>();
        public readonly List<Location> Unsafes = new List<Location>();
        public readonly List<Location> Tries = new List<Location>();
        public readonly List<Location> Locks = new List<Location>();
        
    }

    public struct NamedLocation
    {
        public string Name;
        public Location Location;

        
        public static NamedLocation FromSyntaxNode(SyntaxNode node)
        {
            return new NamedLocation
            {
                Name = node.ToString(),
                Location = Location.FromSyntaxNode(node)
            };
        }
    }
    public struct Location
    {
        public string Source;
        public int Line;
        public int Character;
        public int SpanStart;

        public static Location FromSyntaxNode(SyntaxNode node)
        {
            var span = node.SyntaxTree.GetLineSpan(node.Span);
            return new Location
            {
                Source = node.SyntaxTree.FilePath,
                SpanStart = node.SyntaxTree.Length,
                Line = span.StartLinePosition.Line + 1,
                Character = span.StartLinePosition.Character + 1
            };
        }
    }

    public enum MethodKind
    {
        Method, PropertyGetter, PropertySetter, Constructor, Destructor, Operator, ConversionOperator, IndexGetter, IndexSetter,
        LocalFunction
    }
}