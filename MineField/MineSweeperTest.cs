using System;
using System.Linq;
using System.Linq.Expressions;
using NUnit.Framework;
using FluentAssertions;

namespace Fatvat.Katas.MineSweeper
{
    [TestFixture]
    public class MineSweeperTest
    {
        private void AssertMineField(string input, string expectedOutput)
        {
            Assert.That(MineField.Read(input).Display(), Is.EqualTo(expectedOutput));
        }

        // ReSharper disable InconsistentNaming
        [Test]
        public void Given_Simplest_MineField_Can_ProduceOutput()
        {
            const string mineFieldInput = "1 1\n*";
            const string expectedOutput = "*\n";

            AssertMineField(mineFieldInput, expectedOutput);
        }

        [Test]
        public void Given_Minefield_With_No_Mines_Produce_Empty_Output()
        {
            const string mineFieldInput = "1 1\n-";
            const string expectedOutput = "0\n";

            AssertMineField(mineFieldInput, expectedOutput);
        }

        [Test]
        public void Given_2x2_Minefield_With_No_Mines_Produce_Empty_Output()
        {
            const string mineFieldInput = "2 2\n--\n--\n";
            const string expectedOutput = "00\n00\n";

            AssertMineField(mineFieldInput, expectedOutput);
        }

        [Test]
        public void Given_1x3_Minefield_With_No_Mines_Produce_Empty_Output()
        {
            const string mineFieldInput = "1 3\n-\n-\n-\n";
            const string expectedOutput = "0\n0\n0\n";

            AssertMineField(mineFieldInput, expectedOutput);
        }

        [Test]
        [Ignore]
        public void Given_2x2_MineFIeld_With_Single_Mine_Produce_Output()
        {
            const string mineFieldInput = "2 2\n*-\n--\n";
            const string expectedOutput = "*1\n11\n";

            AssertMineField(mineFieldInput, expectedOutput);
        }

        // ReSharper restore InconsistentNaming
    }

    public class MineField
    {
        private readonly char[,] m_HasMine;

        private MineField(char[,] hasMine)
        {
            m_HasMine = hasMine;
        }

        public static MineField Read(string input)
        {
            var lines = input.Split(new [] {'\n'});

            int[] header = lines[0].Split(new[] {' '}).Select(int.Parse).ToArray();
            var fieldX = header[0];
            var fieldY = header[1];

            char[,] grid = new char[fieldX,fieldY];
            for (var i = 0; i < fieldX; ++i)
            {
                for (var j = 0; j < fieldY; ++j)
                {
                    grid[i,j] = lines[j+1][i];
                }
            }
            
            return new MineField(grid);
        }

        public string Display()
        {
            string result = "";
            for (var i = 0; i < m_HasMine.GetLength(1); ++i)
            {
                for (var j = 0; j < m_HasMine.GetLength(0); ++j)
                {
                    result += IsNotMine(j, i) ? SurroundingMines(j,i).ToString() : "*";
                }
                result += "\n";
            }

            return result;
        }

        private bool IsNotMine(int j, int i)
        {
            return m_HasMine[j,i] != '*';
        }

        private int SurroundingMines(int x, int y)
        {
            var length1 = m_HasMine.GetLength(1);
            var length2 = m_HasMine.GetLength(0);

            var mines = 0;

            for (var i = Math.Max(y-1,0); i < Math.Min(y+2, length1); ++i)
            {
                for (var j = Math.Max(x-1,0); j < Math.Min(x+2,length2); ++j)
                {
                    if (i == j) continue;
                    mines += IsNotMine(j, i) ? 0 : 1;
                }
            }
            return mines;
        }


    }
}
