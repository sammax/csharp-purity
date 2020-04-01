using System;
using System.IO;

namespace pure
{
    internal class Data
    {
        public static int N = 1;
    }

    internal class Program
    {
        private static void Main(string[] args)
        {
            var fileName = args.Length == 0 ? "Example.cs" : args[0];
            var programText = File.ReadAllText(fileName);
            var analyzer = new Analyzer(programText, fileName);
            var methods = analyzer.Methods();
            foreach (var method in methods)
            {
                var result = analyzer.AnalyzeMethod(method);
                Console.WriteLine($"method {result.Name}: {result.Kind}");
                Console.WriteLine($"  Has [Pure]: {result.HasPureAttribute}");
                Console.WriteLine($"  Has Void Return: {result.HasVoidReturn}");
                Console.WriteLine($"  Has Out Params: {result.HasOutParams}");
                Console.WriteLine($"  Has Impure Calls: {result.HasImpureCalls}");
                Console.WriteLine($"  Has New: {result.HasNew}");
                Console.WriteLine($"  Has Unsafe: {result.HasUnsafe}");
                Console.WriteLine($"  Has Non-Local Reads: {result.HasNonLocalReads}");
                if (result.HasNonLocalReads)
                    foreach (var read in result.NonLocalReads)
                        Console.WriteLine(
                            $"    {read.Name} at {read.Location.Source}:{read.Location.Line}:{read.Location.Character} ({read.Location.SpanStart})");
                Console.WriteLine($"  Has Non-Local Writes: {result.HasNonLocalWrites}");
                if (result.HasNonLocalWrites)
                    foreach (var write in result.NonLocalWrites)
                        Console.WriteLine(
                            $"    {write.Name} at {write.Location.Source}:{write.Location.Line}:{write.Location.Character} ({write.Location.SpanStart})");
                Console.WriteLine($"  Has Unsafe: {result.HasUnsafe}");
                Console.WriteLine($"  Has Tries: {result.HasTries}");
                Console.WriteLine($"  Has Locks: {result.HasLocks}");
                Console.WriteLine($"  Has Throws: {result.HasThrows}");
                Console.Write("  => Is ");
                Console.WriteLine(result.IsPure ? "Pure" : "Impure");
            }
        }
    }
}