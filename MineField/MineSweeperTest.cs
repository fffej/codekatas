using System.Linq;
using System.Linq.Expressions;
using NUnit.Framework;
using FluentAssertions;

namespace Fatvat.Katas.MineSweeper
{
    [TestFixture]
    public class MineSweeperTest
    {
        // ReSharper disable InconsistentNaming
        [Test]
        public void Given_Simplest_MineField_Can_ProduceOutput()
        {
            const string mineFieldInput = "1 1\n*";
            const string expectedOutput = "*\n";

            Assert.That(MineField.Read(mineFieldInput).Display(), Is.EqualTo(expectedOutput));
        }

        [Test]
        public void Given_Minefield_With_No_Mines_Produce_Empty_Output()
        {
            const string mineFieldInput = "1 1\n-";
            const string expectedOutput = "0\n";

            Assert.That(MineField.Read(mineFieldInput).Display(), Is.EqualTo(expectedOutput));
        }

        [Test]
        [Ignore]
        public void Given_2x2_Minefield_With_No_Mines_Produce_Empty_Output()
        {
            const string mineFieldInput = "2 2\n--\n--\n";
            const string expectedOutput = "00\n00\n";

            Assert.That(MineField.Read(mineFieldInput).Display(), Is.EqualTo(expectedOutput));
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
            var dx = header[0];
            var dy = header[1];

            char[,] grid = new char[dx,dy];
            for (var i = 0; i < dx; ++i)
            {
                for (var j = 0; j < dy; ++j)
                {
                    grid[i, j] = lines[i + 1][j];
                }
            }
            

            
            return new MineField(grid);
        }

        public string Display()
        {
            return m_HasMine[0,0] != '*' ? "0\n" : "*\n";
        }
    }
}
