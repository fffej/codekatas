using NUnit.Framework;
using FluentAssertions;

namespace Fatvat.Katas.MineSweeper
{
    [TestFixture]
    public class MineSweeperTest
    {
        [Test]
        public void EmptyMineField()
        {
            var mineField = new MineField(".");
            Assert.That(mineField.Show(), Is.EqualTo("0"));
        }

        [Test]
        public void OneMine()
        {
            var mineField = new MineField("*");
            Assert.That(mineField.Show(), Is.EqualTo("*"));
        }

        [Test]
        public void OneLine()
        {
            var mineField = new MineField(".*.");
            Assert.That(mineField.Show(), Is.EqualTo("1*1"));
        }

        [Test]
        public void EmptyMineFieldWithTwoRows()
        {
            var mineField = new MineField("...\n...");
            Assert.That(mineField.Show(), Is.EqualTo("000\n000"));
        }

        [Test]
        public void MineFieldWithTwoRows()
        {
            var mineField = new MineField(".*.\n*..");
            Assert.That(mineField.Show(), Is.EqualTo("2*1\n*21"));
        }
    }

    public class MineField
    {
        private readonly string[] m_MineField;

        public MineField(string mineField)
        {
            m_MineField = mineField.Split('\n');
        }

        public string Show()
        {
            var result = "";
            for (var line = 0; line < m_MineField.Length; ++line)
            {
                for (var column = 0; column < m_MineField[line].Length; ++column)
                {
                    var surroundingMines = GetNumberOfSurroundingMines(line, column);

                    if (!IsMine(line, column))
                    {
                        result += surroundingMines;
                    }
                    else
                    {
                        result += '*';
                    }
                }
                result += "\n";
            }

            return result.TrimEnd();
        }

        private int GetNumberOfSurroundingMines(int line, int column)
        {
            int surroundingMines = 0;

            for (var dx = -1; dx <= 1; dx++)
            {
                for (var dy = -1; dy <= 1; dy++)
                {
                    if (dx == 0 && dy == 0)
                    {
                        continue;
                    }
                    if (IsMine(line + dx, column + dy))
                    {
                        surroundingMines++;
                    }
                }
            }

            return surroundingMines;
        }

        private bool IsMine(int line, int index)
        {
            bool isInBounds = line >= 0 && line < m_MineField.Length &&
                     index >= 0 && index < m_MineField[line].Length;

            return isInBounds && m_MineField[line][index] == '*';
        }
    }
}
