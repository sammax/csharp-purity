using System;
using System.IO;

namespace pure
{
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
                if (result.HasImpureCalls)
                    foreach (var read in result.ImpureCalls)
                        Console.WriteLine(
                            $"    {read.Name} at {read.Location.Source}:{read.Location.Line}:{read.Location.Character} ({read.Location.SpanStart})");
                Console.WriteLine($"  Has Reference New: {result.HasReferenceNews}");
                if (result.HasReferenceNews)
                    foreach (var read in result.ReferenceNews)
                        Console.WriteLine(
                            $"    {read.Name} at {read.Location.Source}:{read.Location.Line}:{read.Location.Character} ({read.Location.SpanStart})");
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
                Console.WriteLine($"  Has Unsafe: {result.HasUnsafes}");
                if (result.HasUnsafes)
                    foreach (var read in result.Unsafes)
                        Console.WriteLine(
                            $"    at {read.Source}:{read.Line}:{read.Character} ({read.SpanStart})");
                Console.WriteLine($"  Has Tries: {result.HasTries}");
                if (result.HasTries)
                    foreach (var read in result.Tries)
                        Console.WriteLine(
                            $"    at {read.Source}:{read.Line}:{read.Character} ({read.SpanStart})");
                Console.WriteLine($"  Has Locks: {result.HasLocks}");
                if (result.HasLocks)
                    foreach (var read in result.Locks)
                        Console.WriteLine(
                            $"    at {read.Source}:{read.Line}:{read.Character} ({read.SpanStart})");
                Console.WriteLine($"  Has Throws: {result.HasThrows}");
                if (result.HasThrows)
                    foreach (var read in result.Throws)
                        Console.WriteLine(
                            $"    at {read.Source}:{read.Line}:{read.Character} ({read.SpanStart})");
                Console.Write("  => Is ");
                Console.WriteLine(result.IsPure ? "Pure" : "Impure");
            }
        }
    }
}