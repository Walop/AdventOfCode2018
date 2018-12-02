using System;
using System.Collections.Generic;
using System.IO;

namespace Day_1_Part_2
{
    class Program
    {
        static void Main(string[] args)
        {
            var input = new List<int>();
            var sr = new StreamReader(Environment.CurrentDirectory + "\\input");
            while (!sr.EndOfStream)
            {
                input.Add(Convert.ToInt32(sr.ReadLine()));
            }
            var i = 0;
            var rounds = 1;
            var sum = 0;
            var prevSums = new HashSet<int>();
            while (!prevSums.Contains(sum))
            {
                prevSums.Add(sum);
                if (i == input.Count)
                {
                    i = 0;
                    Console.Out.WriteLineAsync($"{rounds.ToString()} rounds done");
                    rounds++;
                }
                sum += input[i];
                i++;
            }
            Console.WriteLine(sum);
        }
    }
}
