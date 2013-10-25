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
                    grid[i, j] = lines[i + 1][j];
                }
            }
            
            return new MineField(grid);
        }

        public string Display()
        {
            string result = "";
            for (var i = 0; i < m_HasMine.GetLength(0); ++i)
            {
                for (var j = 0; j < m_HasMine.GetLength(1); ++j)
                {
                    result += m_HasMine[i, j] != '*' ? '0' : '*';
                }
                result += "\n";
            }

            return result;
        }
    }
}
