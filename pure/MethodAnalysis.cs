using System.Collections.Generic;

namespace pure
{
    public struct MethodAnalysis
    {
        public MethodKind Kind;
        public bool NoDeclaration;
        public string Name;
        public bool HasVoidReturn;
        public bool HasOutParams;
        public bool HasImpureCalls;
        public bool HasNonLocalReads;
        public bool HasNonLocalWrites;
        public bool HasNew;
        public bool HasUnsafe;
        public bool HasTries;
        public bool HasLocks;
        public bool HasThrows;

        public bool HasPureAttribute;

        // TODO: verify that those checks are necessary and sufficient for purity
        public bool IsPure =>
            !HasVoidReturn && !HasOutParams && !HasImpureCalls && !HasNonLocalReads && !HasNonLocalWrites &&
            !HasNew && !HasUnsafe && !HasThrows && !HasLocks;
        


        public List<VariableAccess> NonLocalReads;
        public List<VariableAccess> NonLocalWrites;

    }

    public struct VariableAccess
    {
        public string Name;
        public Location Location;
    }
    public struct Location
    {
        public string Source;
        public int Line;
        public int Character;
        public int SpanStart;
    }

    public enum MethodKind
    {
        Method, PropertyGetter, PropertySetter, Constructor, Destructor, Operator, ConversionOperator, IndexGetter, IndexSetter
    }
}