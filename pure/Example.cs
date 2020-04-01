using System;
using System.Collections;
using System.Diagnostics.Contracts;

namespace Xyz
{
    internal class Abc
    {
        public static string Text = "Hello World";
    }
}

namespace HelloWorld
{
    internal struct Data : IDisposable
    {
        public static int N = 1;
        public int M;


        public string ExpressionMethod()
        {
            return "foobar";
        }

        public int X
        {
            [Pure]
            get
            {
                var x = N * 1;
                M = x;
                return x;
            }
            set => N = value;
        }

        public void Dispose()
        {
            Program.getInst();

            static string LocalFunc(int i)
            {
                return $"i: {i}";
            }


            var x = 1;

            var AnotherLocalFunc = new Func<int, int>(i => i + N);

            Func<int, int> YetAnotherLocalFunc = i => { return i - 1; };

            // calling local functions apparently is internally done via Func.Invoke, which requires the code of some stdlib assembly
            // LocalFunc(AnotherLocalFunc(YetAnotherLocalFunc(1)));

            var foo = Program.getInst().foo().Length;

            var len = ExpressionMethod().Length;
        }
    }

    internal class Program
    {
        private static int a = 1;

        private static Program program;

        private int b = 100;

        private Data d;

        public static Program getInst()
        {
            return program;
        }

        private static Data asdf(string[] args)
        {
            const int c = 1;
            var d1 = new Data {M = 1};
            var d2 = new Data();
            d2.M = 2;
            var num = getNum(d1);

            var asdf = new[] {"foo", "bar"};
            foreach (var str in asdf) num += str.Length;

            for (var i = 0; i < 12; i++) num -= i;
            for (var i = 0; i < 12; i += a) num -= i;


            var x = d2.M = c + num;

            for (x = 1; x < 10; num = x)
            {
            }

            var d3 = d2 = new Data();

            if ((x = 1) == 1)
            {
            }

            do
            {
                x -= 1;
            } while (x > 0);

            lock (program)
            {
            }


            try
            {
                throw new Exception("test");
            }
            catch (Exception e)
            {
                switch (x)
                {
                    case 1:
                        while (x < 100) x += 1;
                        break;
                    case 2: break;
                }
            }


            return d2;
        }

        private IEnumerable range(int from)
        {
            var x = from;
            while (true)
            {
                yield return x;
                x += 1;
                if (x > b) yield break;
            }
        }

        [Pure]
        private static int getNum(Data d)
        {
            var x = 1;
            x += d.M;
            var doy = DateTime.Now.DayOfYear;
            d.M = 2;
            return incr(Data.N, ref x);
        }

        [Pure]
        private static int incr(int num, ref int a)
        {
            if (num > 1)
            {
                a = 2;
                Program.a += 1;
            }
            else
            {
                Data.N -= 1;
            }

            return num + 1;
        }

        private unsafe int* foobar()
        {
            var a = 1;
            var x = &a;

            using (d = new Data())
            {
                d.M = 1;
            }

            using Data d2 = new Data(), d3 = new Data();


            return x + d2.M;
        }

        private void asdf(Program program)
        {
            var b = 1;
            int c;
            program.b = 1;
            program.d.M = 2;
            program.d = new Data();
            unsafe
            {
                var a = foobar();
                c = *a;
            }

            var x = a + b + c;
        }

        public string foo()
        {
            return "foo";
        }
    }
}